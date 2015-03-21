{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}

-- | Module responsible for MIL code generation.
--
-- All generated code operates in monads: one for pure computations and some
-- analogues (with more effects) for built-in monads. Basically, all
-- expressions (even the simplest ones, like literals and variables) result in
-- some sequence of binds (possibly empty) and return. We give fresh names to
-- subexpressions and introduce sequencing.
module FunLang.CodeGenMil
  ( codeGen
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.List (foldl')

import FunLang.AST
import FunLang.AST.TypeAnnotated
import FunLang.AST.Helpers
import FunLang.TypeChecker
import FunLang.TypeChecker.TypeEnv
import FunLang.TypeChecker.Helpers
import FunLang.BuiltIn
import FunLang.Utils
import qualified MIL.AST as MIL
import qualified MIL.AST.Builder as MIL
import qualified MIL.AST.Helpers as MIL
import qualified MIL.AST.PrettyPrinter as MIL
import qualified MIL.BuiltIn as MIL

import System.IO.Unsafe

-- | Entry point to the code generator.
-- Takes a type checked program in FunLang and a type environment and produces
-- a source program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.SrcProgram
codeGen tyProgram typeEnv = unsafePerformIO $ runReaderT (evalStateTFrom 0 (runCG $ codeGenProgram tyProgram)) typeEnv

-- | Code generation monad. Uses 'StateT' for providing fresh variable names
-- and 'Reader' for querying the type environment.
-- 'IO' may be used for debug printing.
newtype CodeGenM a = CG { runCG :: StateT NameSupply (ReaderT TypeEnv IO) a }
  deriving (Monad, MonadState NameSupply, MonadReader TypeEnv, Functor, Applicative, MonadIO)

-- | A counter for generating unique variable names.
type NameSupply = Int

codeGenProgram :: TyProgram -> CodeGenM MIL.SrcProgram
codeGenProgram (Program _ srcTypeDefs tyFunDefs) = do
  (milTypeDefs, milConWrappers) <- unzip <$> mapM codeGenTypeDef srcTypeDefs
  milFunDefs <- mapM codeGenFunDef tyFunDefs
  return $ MIL.Program (builtInMilTypeDefs ++ milTypeDefs, builtInMilFunDefs ++ concat milConWrappers ++ milFunDefs)

-- | Code generation for data type.
-- Returns an MIL type definition and a list of wrapper functions for data
-- constructors.
-- See Note [Data constructors and purity].
codeGenTypeDef :: SrcTypeDef -> CodeGenM (MIL.SrcTypeDef, [MIL.SrcFunDef])
codeGenTypeDef (TypeDef _ srcTypeName srcTypeVars srcConDefs) = do
  let milTypeName = typeNameMil $ getTypeName srcTypeName
  let milTypeVars = map (typeVarMil . getTypeVar) srcTypeVars
  (milConDefs, milConWrappers) <- unzip <$> mapM codeGenConDef srcConDefs
  return (MIL.TypeDef milTypeName milTypeVars milConDefs, milConWrappers)

-- | Code generation for data constructor.
-- Returns an MIL constructor definition and a wrapper function definition.
-- See Note [Data constructors and purity].
codeGenConDef :: SrcConDef -> CodeGenM (MIL.SrcConDef, MIL.SrcFunDef)
codeGenConDef (ConDef _ srcConName srcConFields) = do
  let conName = getConName srcConName
  let milConFields = map srcTypeMil srcConFields
  milConWrapper <- codeGenConWrapper conName
  return (MIL.ConDef (conNameMil conName) milConFields, milConWrapper)

-- | See Note [Data constructors and purity].
codeGenConWrapper :: ConName -> CodeGenM MIL.SrcFunDef
codeGenConWrapper conName = do
  conType <- asks (dcontiType . getDataConTypeInfo conName . getDataConTypeEnv)
  let conWrapperType = monadTypeMil conType
  let conNameExpr = MIL.ConNameE (conNameMil conName) ()
  conWrapperBody <- conWrapperMilExpr conType conNameExpr
  return (MIL.FunDef (conWrapperFunNameMil conName) conWrapperType conWrapperBody)

-- | Note [Data constructors and purity]:
-- There is a problem with function types of data constructors and FunLang's
-- monad for pure computations. These function types are produced by the MIL
-- type checker, which doesn't know about this pure monad. Data constructors in
-- MIL are completely pure (they don't get special monadic types). They can be
-- referenced (and partially applied) from FunLang's pure functions (which are
-- not completely pure, they work inside a pure monad) and then the types don't
-- match. It would be very hard (or even impossible) to figure out the places
-- where we should do type conversions differently when referencing data
-- constructors.
--
-- Solution: Each data constructor gets its wrapper function in the generated
-- code (see 'conWrapperMilExpr'), which has a converted monadic type of the
-- constructor (with pure monad all over the place).
--
-- Could we instead just always use `return` when processing data constructors?
-- What is done right now looks kind of easier, since you generate this return
-- only once in the wrapper and then just don't worry about constructors.

-- | Generates a body for a data constructor wrapper function.
-- Takes a constructor function type and an expression to begin with, which
-- also plays a role of the accumulator for the result.
--
-- It is like these clever programs that try to produce a term from a
-- polymorphic type (see also `Theorems for free`), but it is much more simple
-- and restricted. It doesn't try to handle all possible types, but only the
-- shape that data constructor types have.
conWrapperMilExpr :: Type -> MIL.SrcExpr -> CodeGenM MIL.SrcExpr
conWrapperMilExpr conType conAppMilExpr =
  case conType of
    -- TODO: It looks like built-in monads will never get here,
    -- so this is just the base case for this function
    TyApp {} -> return $ MIL.ReturnE pureSrcMonadMil conAppMilExpr

    -- Each arrow type produces a lambda and adds an application to the data
    -- constructor.
    TyArrow t1 t2 -> do
      v <- newMilVar
      let conExprAppliedToVar = MIL.AppE conAppMilExpr (MIL.VarE v)
      lambdaExprBody <- conWrapperMilExpr t2 conExprAppliedToVar
      let lambdaExpr = MIL.mkSrcLambda v (typeMil t1) lambdaExprBody
      return $ MIL.ReturnE pureSrcMonadMil lambdaExpr

    -- Each forall type produces a type lambda and adds a type application to
    -- the data constructor.
    TyForAll tv t -> do
      let conExprAppliedToTypeVar = MIL.TypeAppE conAppMilExpr (MIL.SrcTyTypeCon $ MIL.typeVarToTypeName (typeVarMil tv))
      typeLambdaExprBody <- conWrapperMilExpr t conExprAppliedToTypeVar
      let typeLambdaExpr = MIL.TypeLambdaE (typeVarMil tv) typeLambdaExprBody
      return $ MIL.ReturnE pureSrcMonadMil typeLambdaExpr

codeGenFunDef :: TyFunDef -> CodeGenM MIL.SrcFunDef
codeGenFunDef (FunDef _ srcFunName _ tyFunEqs) = do
  let funName = getFunName srcFunName
  milFunSrcType <- monadTypeMil <$> asks (ftiType . getFunTypeInfo funName . getFunTypeEnv)
  -- All generated code is monadic. Therefore, function types should have a
  -- monad.
  let (MIL.SrcTyApp funMonad _) = milFunSrcType
  milFunBody <- codeGenFunEqs funMonad tyFunEqs
  return $ MIL.FunDef (funNameMil funName) milFunSrcType milFunBody

-- | Takes function equations of the function definition and a function monad
-- and returns an MIL expression.
codeGenFunEqs :: MIL.SrcType -> [TyFunEq] -> CodeGenM MIL.SrcExpr
codeGenFunEqs funMonad tyFunEqs = do
  funEqExprs <- mapM (codeGenExpr funMonad . getFunEqBody) tyFunEqs
  return $ fst $ head funEqExprs  -- TODO: combine equations (descoped)

-- | Expression code generation.
-- Takes a monad of the containing function.
-- Returns an MIL expression and its type.
codeGenExpr :: MIL.SrcType -> TyExpr -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenExpr funMonad tyExpr =
  let exprType = getTypeOf tyExpr in
  case tyExpr of
    LitE tyLit ->
      return ( MIL.ReturnE funMonad (literalMil tyLit)
             , MIL.SrcTyApp funMonad (typeMil exprType))

    -- See Note [Data constructors and purity].
    ConNameE conType srcConName -> do
      let conName = getConName srcConName
      let conWrapperType = monadTypeMil conType
      return ( MIL.VarE $ conWrapperVarMil conName
             , conWrapperType)

    VarE _ varType var -> do
      let milVar = varMil var
      let funName = varToFunName var
      isGlobalFunction <- asks (isFunctionDefined funName . getFunTypeEnv)
      if isGlobalFunction || isMonadType varType
        then return ( MIL.VarE milVar
                    , monadTypeMil varType)
        else return ( MIL.ReturnE funMonad $ MIL.VarE milVar
                    , MIL.SrcTyApp funMonad $ typeMil varType)

    LambdaE _ _ tyVarBinders tyBodyExpr -> do
      (milBodyExpr, milBodyType) <- codeGenExpr funMonad tyBodyExpr
      foldM (\(mBodyExpr, mBodyType) tvb -> do
          let varType = getTypeOf tvb
          let milVar = varMil (getVar $ getBinderVar tvb)
          let milVarType = typeMil varType
          return ( MIL.ReturnE funMonad $
                     MIL.LambdaE (MIL.VarBinder (milVar, milVarType)) mBodyExpr
                 , MIL.SrcTyApp funMonad (MIL.SrcTyArrow milVarType mBodyType)))
        (milBodyExpr, milBodyType)
        (reverse tyVarBinders)

    TypeLambdaE _ _ srcTypeVars tyBodyExpr -> do
      (milBodyExpr, milBodyType) <- codeGenExpr funMonad tyBodyExpr
      return $ foldr (\tv (mBodyExpr, mBodyType) ->
                   ( MIL.ReturnE funMonad (MIL.TypeLambdaE (typeVarMil tv) mBodyExpr)
                   , MIL.SrcTyApp funMonad (MIL.SrcTyForAll (typeVarMil tv) mBodyType)))
                 (milBodyExpr, milBodyType)
                 (map getTypeVar srcTypeVars)

    TypeAppE _ _ tyAppExpr srcArgType -> do
      (milAppExpr, milAppExprType) <- codeGenExpr funMonad tyAppExpr
      var <- newMilVar
      return ( MIL.mkSrcLet var (MIL.getSrcResultType milAppExprType) milAppExpr $
                 MIL.TypeAppE (MIL.VarE var) (srcTypeMil srcArgType)
             , monadTypeMil exprType)

    DoE _ _ tyStmts -> codeGenDoBlock funMonad tyStmts

    BinOpE _ resultType srcBinOp tyExpr1 tyExpr2 ->
      codeGenBinOp funMonad (getBinOp srcBinOp) tyExpr1 tyExpr2 resultType

    ParenE _ tySubExpr -> codeGenExpr funMonad tySubExpr

literalMil :: TyLiteral -> MIL.SrcExpr
literalMil UnitLit {}         = MIL.LitE MIL.UnitLit
literalMil (IntLit _ _ i)     = MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ _ f _) = MIL.LitE (MIL.FloatLit f)
literalMil (StringLit _ _ s)  = stringMil s

stringMil :: String -> MIL.SrcExpr
stringMil "" = MIL.mkSrcConName "Empty_Str"
stringMil (c:cs) =
  MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str")
                     (MIL.mkCharLit c))
           (stringMil cs)

-- | Code generation of binary operations.
-- Takes a monad of containing function, binary operation, its operands, a type
-- of the result.
-- Returns an MIL expression and its type.
codeGenBinOp :: MIL.SrcType -> BinOp -> TyExpr -> TyExpr -> Type -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenBinOp funMonad binOp tyExpr1 tyExpr2 resultType = do
  case binOp of
    App -> do
      (milExpr1, milExpr1Type) <- codeGenExpr funMonad tyExpr1
      (milExpr2, milExpr2Type) <- codeGenExpr funMonad tyExpr2
      var1 <- newMilVar
      var2 <- newMilVar
      -- FunLang functions always get pure monad stack as a first type
      -- constructor, so we don't need to generate `return`, but type
      -- conversion for milResultType should communicate that.
      let appE = MIL.AppE (MIL.VarE var1) (MIL.VarE var2)
      return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
                 MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2
                   appE
             , monadTypeMil resultType)

codeGenDoBlock :: MIL.SrcType -> [TyStmt] -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenDoBlock funMonad [ExprS _ tyExpr] = codeGenExpr funMonad tyExpr
-- Every expression code generation results in return.
-- TODO: take funMonad into account
codeGenDoBlock funMonad [ReturnS _ _ tyExpr] = codeGenExpr funMonad tyExpr
{-
codeGenDoBlock (tyStmt:tyStmts) funMonad =
  case tyStmt of
    ExprS _ tyExpr -> do
      (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
      (milBodyExpr, milBodyType) <- codeGenDoBlock tyStmts funMonad
      return ( MIL.LetE (MIL.VarBinder (MIL.Var "_", MIL.getMonadResultType milBindType))
                 milBindExpr milBodyExpr
             , milBodyType)

    BindS _ tyVarBinder tyExpr -> do
      (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
      (milBodyExpr, milBodyType) <- codeGenDoBlock tyStmts funMonad
      return ( MIL.LetE (MIL.VarBinder ( varMil (getVar $ getBinderVar tyVarBinder)
                                       , MIL.getMonadResultType milBindType))
                 milBindExpr milBodyExpr
             , milBodyType)

    ReturnS _ _ tyExpr -> do
      (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
      (milBodyExpr, milBodyType) <- codeGenDoBlock tyStmts funMonad
      return ( MIL.LetE (MIL.VarBinder (MIL.Var "_", MIL.getMonadResultType milBindType))
                 milBindExpr milBodyExpr
             , milBodyType)
-}
{-

-- * Type conversions

-- | Note [Type conversion]:
--
-- There are several different versions of type conversion. Sometimes we need
-- to convert source types, in other cases, we are working with internal type
-- representation.
-- TODO: revise
-- * 'typeMil' is the simplest one. It converts an internal type representation
-- to an MIL type. It doesn't know about generated monads (like Pure_M or
-- IO_M). It doesn't introduce any new monads, only maps built-in FunLang
-- monads to built-in MIL monads. Used for built-in functions and for some very
-- simple cases, like types of literals.
--
-- * 'monadTypeMil' is one of the main `working horses` of the type conversion.
-- It introduces Pure_M monad for pure functions and converts built-in monads
-- to generated aliases (like IO_M, for example). It introduces Pure_M on the
-- right of function arrows and inside forall type. Used more for local things
-- and argument positions.
--
-- * 'funTypeMil' introduces more monads compared to 'monadTypeMil' and is
-- used more with global function types and result positions. It puts Pure_M
-- before almost any type except for built-in FunLang monads.
--
-- * 'monadSrcTypeMil' is basically a 'SrcType' version of 'monadTypeMil'.
--
-- * 'monadSrcFunTypeMil' is basically a 'SrcType' version of
-- 'funTypeMil'.

-- | See Note [Type conversion].
-- The most interesting is the 'TyApp' transformation, because it deals with
-- monads. Type checker ensures that all type constructors are fully applied,
-- so it is safe to construct monadic types in this way (by direct pattern
-- matching), otherwise - we throw a panic.
typeMil :: Type -> MIL.Type
typeMil (TyVar typeVar) = MIL.TyVar $ typeVarMil typeVar
typeMil (TyArrow t1 t2) = MIL.TyArrow (typeMil t1) (typeMil t2)
typeMil (TyApp typeName typeArgs) =
  case (typeName, typeArgs) of
    (TypeName "IO", [ioResultType]) ->
      MIL.applyMonadType (MIL.MTyMonad MIL.IO) (typeMil ioResultType)
    (TypeName "IO", _) -> error "IO type is ill-formed"

    (TypeName "State", [_, stateResultType]) ->
      MIL.applyMonadType (MIL.MTyMonad MIL.State) (typeMil stateResultType)
    (TypeName "State", _) -> error "State type is ill-formed"

    _ -> foldl' (\mt t -> MIL.TyApp mt (typeMil t))
           (MIL.TyTypeCon $ typeNameMil typeName)
           typeArgs
typeMil (TyForAll typeVar t) = MIL.TyForAll (typeVarMil typeVar) (typeMil t)

-- | See Note [Type conversion].
monadTypeMil :: Type -> MIL.Type
monadTypeMil (TyVar typeVar) = MIL.TyVar $ typeVarMil typeVar
monadTypeMil (TyArrow t1 t2) = MIL.TyArrow (monadTypeMil t1) (monadFunTypeMil t2)
monadTypeMil (TyApp typeName typeArgs) =
  case (typeName, typeArgs) of
    (TypeName "IO", [ioResultType]) ->
      MIL.applyMonadType ioMonadMil (monadTypeMil ioResultType)
    (TypeName "IO", _) -> error "IO type is ill-formed"

    (TypeName "State", [_, stateResultType]) ->
      MIL.applyMonadType (MIL.MTyMonad MIL.State) (monadTypeMil stateResultType)
    (TypeName "State", _) -> error "State type is ill-formed"

    _ -> foldl' (\mt t -> MIL.TyApp mt (monadTypeMil t))
           (MIL.TyTypeCon $ typeNameMil typeName)
           typeArgs
monadTypeMil (TyForAll typeVar t) = MIL.TyForAll (typeVarMil typeVar) (monadFunTypeMil t)

-- | See Note [Type conversion].
monadFunTypeMil :: Type -> MIL.Type
monadFunTypeMil t@(TyApp typeName typeArgs) =
  case typeName of
    TypeName "IO" -> monadTypeMil t
    TypeName "State" -> monadTypeMil t
    _ -> MIL.applyMonadType pureMonadMil (monadTypeMil t)
monadFunTypeMil t = MIL.applyMonadType pureMonadMil (monadTypeMil t)

-- | See Note [Type conversion].
-- Monadic types are transformed in different cases depending on their kind.
-- * IO and State have kind `* => *` so they are caught in 'SrcTyCon'.
monadSrcTypeMil :: SrcType -> CodeGenM MIL.Type
monadSrcTypeMil (SrcTyCon srcTypeName) =
  case getTypeName srcTypeName of
    TypeName "IO" -> return $ MIL.TyMonad ioMonadMil
    TypeName "State" -> return $ MIL.TyMonad (MIL.MTyMonad MIL.State)
    typeName -> do
      -- 'SrcTyCon' can represent both type names and type variables, so we
      -- need to distinguish between them in order to generate correct MIL
      -- code.
      dataTypeEnv <- asks getDataTypeEnv
      if isTypeDefined typeName dataTypeEnv
        then return $ MIL.TyTypeCon (typeNameMil typeName)
        else return $ MIL.TyVar (typeVarMil $ typeNameToTypeVar typeName)
monadSrcTypeMil (SrcTyApp _ st1 st2) =
  MIL.TyApp <$> monadSrcTypeMil st1 <*> monadSrcTypeMil st2
monadSrcTypeMil (SrcTyArrow _ st1 st2) =
  MIL.TyArrow <$> monadSrcTypeMil st1 <*> monadSrcFunTypeMil st2
monadSrcTypeMil (SrcTyForAll _ stv st) =
  MIL.TyForAll (typeVarMil $ getTypeVar stv) <$> monadSrcFunTypeMil st
monadSrcTypeMil (SrcTyParen _ st) = monadSrcTypeMil st

-- | See Note [Type conversion].
monadSrcFunTypeMil :: SrcType -> CodeGenM MIL.Type
monadSrcFunTypeMil t@(SrcTyCon srcTypeName) =
  case getTypeName srcTypeName of
    TypeName "IO" -> monadSrcTypeMil t
    TypeName "State" -> monadSrcTypeMil t
    _ -> MIL.applyMonadType pureMonadMil <$> monadSrcTypeMil t
monadSrcFunTypeMil st = MIL.applyMonadType pureMonadMil <$> monadSrcTypeMil st
-}

-- | TODO
srcTypeMil :: SrcType -> MIL.SrcType
srcTypeMil (SrcTyCon srcTypeName) = MIL.SrcTyTypeCon (typeNameMil $ getTypeName srcTypeName)
srcTypeMil (SrcTyApp _ st1 st2) =
  let normalCaseApp = MIL.SrcTyApp (srcTypeMil st1) (srcTypeMil st2) in
  case st1 of
    SrcTyCon srcTypeName ->
      case getTypeName srcTypeName of
        TypeName "IO" -> MIL.SrcTyApp ioSrcMonadMil (srcTypeMil st2)
        _ -> normalCaseApp
    (SrcTyApp _ (SrcTyCon srcTypeName') _) ->
      case getTypeName srcTypeName' of
        TypeName "State" -> MIL.SrcTyApp stateSrcMonadMil (srcTypeMil st2)  -- discard state type?
        _ -> normalCaseApp
    _ -> normalCaseApp
srcTypeMil (SrcTyArrow _ st1 st2) =
  MIL.SrcTyArrow (srcTypeMil st1) (MIL.SrcTyApp pureSrcMonadMil $ srcTypeMil st2)  -- TODO: built-ins?
srcTypeMil (SrcTyForAll _ stv st) =
  MIL.SrcTyForAll (typeVarMil $ getTypeVar stv)
    (MIL.SrcTyApp pureSrcMonadMil $ srcTypeMil st)  -- TODO: built-ins?
srcTypeMil (SrcTyParen _ st) = srcTypeMil st

-- | TODO
typeMil :: Type -> MIL.SrcType
typeMil (TyVar typeVar) = MIL.SrcTyTypeCon (typeNameMil $ typeVarToTypeName typeVar)
typeMil (TyArrow t1 t2) = MIL.SrcTyArrow (typeMil t1) (monadTypeMil t2)
typeMil (TyApp typeName typeArgs) =
  case (typeName, typeArgs) of
    (TypeName "IO", [ioResultType]) ->
      MIL.SrcTyApp ioSrcMonadMil (typeMil ioResultType)
    (TypeName "State", [_stateType, stateResultType]) ->
      MIL.SrcTyApp stateSrcMonadMil (typeMil stateResultType)
    _ -> foldl' (\at t -> MIL.SrcTyApp at (typeMil t))
                (MIL.SrcTyTypeCon $ typeNameMil typeName)
                typeArgs
typeMil (TyForAll typeVar t) = MIL.SrcTyForAll (typeVarMil typeVar) (monadTypeMil t)

-- | TODO
monadTypeMil :: Type -> MIL.SrcType
monadTypeMil t@(TyVar {}) = MIL.SrcTyApp pureSrcMonadMil (typeMil t)
monadTypeMil t@(TyArrow {}) = MIL.SrcTyApp pureSrcMonadMil (typeMil t)
monadTypeMil t@(TyApp typeName _typeArgs) =
  case typeName of
    TypeName "IO" -> typeMil t
    TypeName "State" -> typeMil t
    _ -> MIL.SrcTyApp pureSrcMonadMil (typeMil t)
monadTypeMil t@(TyForAll {}) = MIL.SrcTyApp pureSrcMonadMil (typeMil t)

-- * Conversion utils

typeNameMil :: TypeName -> MIL.TypeName
typeNameMil (TypeName typeNameStr) = MIL.TypeName typeNameStr

conNameMil :: ConName -> MIL.ConName
conNameMil (ConName conNameStr) = MIL.ConName conNameStr

conWrapperFunNameMil :: ConName -> MIL.FunName
conWrapperFunNameMil (ConName conNameStr) = MIL.FunName ("con_" ++ conNameStr)

conWrapperVarMil :: ConName -> MIL.Var
conWrapperVarMil (ConName conNameStr) = MIL.Var ("con_" ++ conNameStr)

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

typeVarMil :: TypeVar -> MIL.TypeVar
typeVarMil (TypeVar typeVarStr) = MIL.TypeVar typeVarStr

-- * CodeGenM operations

newMilVar :: CodeGenM MIL.Var
newMilVar = do
  i <- get
  modify (+1)
  return $ MIL.Var ("var_" ++ show i)

debugPrint :: String -> String -> IO ()
debugPrint name value = liftIO $ putStrLn ((name ++ ": ") ++ value)

