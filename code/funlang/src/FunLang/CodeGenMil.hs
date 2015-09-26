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
  , monadErrorTypeCons
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
  return $ MIL.Program ( builtInMilTypeDefs ++ milTypeDefs
                       , builtInMilFunDefs ++ concat milConWrappers ++ milFunDefs)

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
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv)
  let milFunSrcType = monadTypeMil funType
  -- All generated code is monadic. Therefore, function types should have a
  -- monad, except for FunLang's State monad case, where we fix this in
  -- additional pass, so we need to return pure stack explicitly.
  let funMonad = if isStateMonad funType
                   then pureSrcMonadMil
                   else let (MIL.SrcTyApp m _) = milFunSrcType in m
  milFunBody <- codeGenFunEqs funMonad tyFunEqs
  -- TODO: Maybe, it is possible to fix just those where funType has State monad at the top,
  -- because, that is where do expression can be.
  if isStateMonad funType
    then do
      (milFunBodyWithState, milFunSrcTypeWithState) <- addRefParameter milFunBody milFunSrcType
      return $ MIL.FunDef (funNameMil funName) milFunSrcTypeWithState milFunBodyWithState
    else
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
-- TODO: funMonad should be more dynamic depending on a context???
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
        then
          if isStateMonad varType
            then return ( MIL.VarE milVar
                        , MIL.SrcTyApp pureSrcMonadMil $ monadTypeMil varType)
            else return ( MIL.VarE milVar
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
      var1 <- newMilVar
      let milTyAppExpr = MIL.TypeAppE (MIL.VarE var) (srcTypeMil srcArgType)
      let milExprType = monadTypeMil exprType
      case milExprType of
        (MIL.SrcTyApp _ (MIL.SrcTyArrow (MIL.SrcTyApp (MIL.SrcTyTypeCon (MIL.TypeName "Ref")) refType) st2)) ->
          return ( MIL.mkSrcLet var (MIL.getSrcResultType milAppExprType) milAppExpr $
                     MIL.mkSrcLet var1 (MIL.SrcTyArrow (MIL.SrcTyApp (MIL.SrcTyTypeCon (MIL.TypeName "Ref")) refType) st2) milTyAppExpr $
                       MIL.AppE (MIL.VarE var1) (MIL.VarE $ MIL.Var "state_")
                 , st2)
        _ ->
          return ( MIL.mkSrcLet var (MIL.getSrcResultType milAppExprType) milAppExpr
                     milTyAppExpr
                 , milExprType)

    ThrowE _ _ srcType -> do
      let t' = srcTypeMil srcType
      return ( MIL.LiftE
                 (MIL.AppE (MIL.TypeAppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "throw_error") (MIL.mkSimpleSrcType "Unit"))
                                         t')
                           (MIL.LitE MIL.UnitLit))
                 (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") (MIL.mkSimpleSrcType "Unit"))
                 (MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State") $
                    (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType))
             , MIL.SrcTyApp funMonad t')

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
      var3 <- newMilVar
      let milExpr2' = if isStateMonad (getTypeOf tyExpr2)
                        then MIL.ReturnE funMonad milExpr2
                        else milExpr2
      let milLetSequenceWithoutBody body = MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
                                             MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2' body
      let appE = MIL.AppE (MIL.VarE var1) (MIL.VarE var2)
      -- FunLang functions always get pure monad stack as a first type
      -- constructor, so we don't need to generate `return`, but type
      -- conversion for milResultType should communicate that.
      let milResultType = monadTypeMil resultType
      case milResultType of
          (MIL.SrcTyApp _ (MIL.SrcTyArrow (MIL.SrcTyApp (MIL.SrcTyTypeCon (MIL.TypeName "Ref")) refType) st2)) ->
            return ( milLetSequenceWithoutBody $
                       MIL.mkSrcLet var3 (MIL.SrcTyArrow (MIL.SrcTyApp (MIL.SrcTyTypeCon (MIL.TypeName "Ref")) refType) st2) appE $
                         MIL.AppE (MIL.VarE var3) (MIL.VarE $ MIL.Var "state_")
                   , st2)
          _ ->
            return ( milLetSequenceWithoutBody appE
                   , milResultType)

    Catch -> do
      (milExpr1, milExpr1Type) <- codeGenExpr funMonad tyExpr1
      (milExpr2, _) <- codeGenExpr funMonad tyExpr2
      let catchErrorFunName = if funMonad == pureSrcMonadMil
                                then "catch_error_1"
                                else "catch_error_2"
      return ( MIL.AppE
                 (MIL.AppE (MIL.TypeAppE (MIL.TypeAppE (MIL.VarE $ MIL.Var catchErrorFunName) (MIL.mkSimpleSrcType "Unit")) (MIL.getSrcResultType milExpr1Type))
                           milExpr1)
                 (MIL.mkSrcLambda (MIL.Var "error_") (MIL.mkSimpleSrcType "Unit") milExpr2)
             , milExpr1Type)

    Add ->
      let arithFunName = case getTypeOf tyExpr1 of
                           TyApp (TypeName "Float") [] -> "add_float"
                           TyApp (TypeName "Int") [] -> "add_int"
                           _ -> error "codeGenBinOp: unsupported type for Add"
      in codeGenArith funMonad tyExpr1 tyExpr2 resultType arithFunName

    Sub ->
      let arithFunName = case getTypeOf tyExpr1 of
                           TyApp (TypeName "Float") [] -> "sub_float"
                           TyApp (TypeName "Int") [] -> "sub_int"
                           _ -> error "codeGenBinOp: unsupported type for Add"
      in codeGenArith funMonad tyExpr1 tyExpr2 resultType arithFunName

    Mul ->
      let arithFunName = case getTypeOf tyExpr1 of
                           TyApp (TypeName "Float") [] -> "mul_float"
                           TyApp (TypeName "Int") [] -> "mul_int"
                           _ -> error "codeGenBinOp: unsupported type for Add"
      in codeGenArith funMonad tyExpr1 tyExpr2 resultType arithFunName

    Div -> do
      let arithFunName = case getTypeOf tyExpr1 of
                           TyApp (TypeName "Float") [] -> "div_float"
                           TyApp (TypeName "Int") [] -> "div_int"
                           _ -> error "codeGenBinOp: unsupported type for Div"
      (milExpr1, milExpr1Type) <- codeGenExpr funMonad tyExpr1
      (milExpr2, milExpr2Type) <- codeGenExpr funMonad tyExpr2
      var1 <- newMilVar
      var2 <- newMilVar
      let milResultType = monadTypeMil resultType
      return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
                 MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2 $
                   MIL.LiftE (MIL.AppE (MIL.AppE (MIL.VarE $ MIL.Var arithFunName) (MIL.VarE var1)) (MIL.VarE var2))
                     (MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") (MIL.mkSimpleSrcType "Unit")) (MIL.mkSimpleSrcType "NonTerm")) funMonad
             , milResultType)

codeGenArith :: MIL.SrcType -> TyExpr -> TyExpr -> Type -> String -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenArith funMonad tyExpr1 tyExpr2 resultType arithFunName = do
  (milExpr1, milExpr1Type) <- codeGenExpr funMonad tyExpr1
  (milExpr2, milExpr2Type) <- codeGenExpr funMonad tyExpr2
  var1 <- newMilVar
  var2 <- newMilVar
  let milResultType = monadTypeMil resultType
  return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
             MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2 $
               MIL.ReturnE funMonad (MIL.AppE (MIL.AppE (MIL.VarE $ MIL.Var arithFunName) (MIL.VarE var1)) (MIL.VarE var2))
         , milResultType)

codeGenDoBlock :: MIL.SrcType -> [TyStmt] -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenDoBlock funMonad [ExprS _ tyExpr] = codeGenExpr funMonad tyExpr
codeGenDoBlock funMonad [ReturnS _ t tyExpr] = do
  (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
  var <- newMilVar
  let (milBodyExpr, milBodyType) = ( MIL.ReturnE (monadMil t) (MIL.VarE var)
                                   , MIL.SrcTyApp (monadMil t) (MIL.getSrcResultType milBindType))
  return ( MIL.mkSrcLet var (MIL.getSrcResultType milBindType)
             milBindExpr milBodyExpr
         , milBodyType)
codeGenDoBlock funMonad (tyStmt:tyStmts) =
  case tyStmt of
    ExprS _ tyExpr -> do
      (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
      var <- newMilVar
      (milBodyExpr, milBodyType) <- codeGenDoBlock funMonad tyStmts
      return ( MIL.mkSrcLet var (MIL.getSrcResultType milBindType)
                 milBindExpr milBodyExpr
             , milBodyType)

    BindS _ tyVarBinder tyExpr -> do
      (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
      (milBodyExpr, milBodyType) <- codeGenDoBlock funMonad tyStmts
      let milVar = varMil (getVar $ getBinderVar tyVarBinder)
      return ( MIL.mkSrcLet milVar (srcTypeMil $ getBinderSrcType tyVarBinder)
                 milBindExpr milBodyExpr
             , milBodyType)

    ReturnS _ _ tyExpr -> do
      (milBindExpr, milBindType) <- codeGenExpr funMonad tyExpr
      var <- newMilVar
      (milBodyExpr, milBodyType) <- codeGenDoBlock funMonad tyStmts
      return ( MIL.mkSrcLet var (MIL.getSrcResultType milBindType)
                 milBindExpr milBodyExpr
             , milBodyType)

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

-- TODO: Try not to have a monad in front of State functions and use returns when needed instead.
-- Should probably simplify code a bit.
-- Think more about binds and when effects actually happen, again.

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
    (SrcTyApp _ (SrcTyCon srcTypeName') st12) ->
      case getTypeName srcTypeName' of
        TypeName "State" ->
          let stateType = st12
              stateResultType = st2 in
            MIL.SrcTyApp pureSrcMonadMil $
            MIL.SrcTyArrow (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") (srcTypeMil stateType))
                           (MIL.SrcTyApp stateSrcMonadMil (srcTypeMil stateResultType))
        _ -> normalCaseApp
    _ -> normalCaseApp
srcTypeMil (SrcTyArrow _ st1 st2) = MIL.SrcTyArrow (srcTypeMil st1) (monadSrcTypeMil st2)
srcTypeMil (SrcTyForAll _ stv st) = MIL.SrcTyForAll (typeVarMil $ getTypeVar stv) (monadSrcTypeMil st)
srcTypeMil (SrcTyParen _ st) = srcTypeMil st

-- | TODO
monadSrcTypeMil :: SrcType -> MIL.SrcType
monadSrcTypeMil st@(SrcTyApp _ st1 st2) =
  let normalCaseApp = MIL.SrcTyApp pureSrcMonadMil (srcTypeMil st) in
  case st1 of
    SrcTyCon srcTypeName ->
      case getTypeName srcTypeName of
        TypeName "IO" -> srcTypeMil st
        _ -> normalCaseApp
    (SrcTyApp _ (SrcTyCon srcTypeName') _) ->
      case getTypeName srcTypeName' of
        TypeName "State" -> srcTypeMil st
        _ -> normalCaseApp
    _ -> normalCaseApp
monadSrcTypeMil st = MIL.SrcTyApp pureSrcMonadMil (srcTypeMil st)

-- | TODO
typeMil :: Type -> MIL.SrcType
typeMil (TyVar typeVar) = MIL.SrcTyTypeCon (typeNameMil $ typeVarToTypeName typeVar)
typeMil (TyArrow t1 t2) = MIL.SrcTyArrow (typeMil t1) (monadTypeMil t2)
typeMil (TyApp typeName typeArgs) =
  case (typeName, typeArgs) of
    (TypeName "IO", [ioResultType]) ->
      MIL.SrcTyApp ioSrcMonadMil (typeMil ioResultType)
    (TypeName "State", [stateType, stateResultType]) ->
        MIL.SrcTyApp pureSrcMonadMil $
        MIL.SrcTyArrow (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") (typeMil stateType))
                       (MIL.SrcTyApp stateSrcMonadMil (typeMil stateResultType))
    _ -> foldl' (\at t -> MIL.SrcTyApp at (typeMil t))
                (MIL.SrcTyTypeCon $ typeNameMil typeName)
                typeArgs
typeMil (TyForAll typeVar t) = MIL.SrcTyForAll (typeVarMil typeVar) (monadTypeMil t)

-- | TODO
monadTypeMil :: Type -> MIL.SrcType
monadTypeMil t@(TyApp typeName _typeArgs) =
  case typeName of
    TypeName "IO" -> typeMil t
    TypeName "State" -> typeMil t
    _ -> MIL.SrcTyApp pureSrcMonadMil (typeMil t)
monadTypeMil t = MIL.SrcTyApp pureSrcMonadMil (typeMil t)

-- | TODO
monadMil :: Type -> MIL.SrcType
monadMil (TyApp typeName typeArgs) =
  case (typeName, typeArgs) of
    (TypeName "IO", [_ioResultType]) -> ioSrcMonadMil
    (TypeName "State", [_stateType, _stateResultType]) -> stateSrcMonadMil
    _ -> error ("Unknown monad: " ++ show typeName)
monadMil t = error ("Not a monadic type: " ++ show t)

-- * State monad pass

addRefParameter :: MIL.SrcExpr -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
addRefParameter milFunBodyExpr milSrcFunType = do
  let (MIL.SrcTyApp _ (MIL.SrcTyArrow refType _)) = milSrcFunType
  return ( MIL.ReturnE pureSrcMonadMil $
             MIL.mkSrcLambda (MIL.Var "state_") refType milFunBodyExpr
         , milSrcFunType)

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

debugPrint :: String -> String -> CodeGenM ()
debugPrint name value = liftIO $ putStrLn ((name ++ ": ") ++ value)

-- * Built-ins

pureSrcMonadMil :: MIL.SrcType
pureSrcMonadMil = stateSrcMonadMil

stateSrcMonadMil :: MIL.SrcType
stateSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State") $
    MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
      (MIL.mkSimpleSrcType "NonTerm")

ioSrcMonadMil :: MIL.SrcType
ioSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State") $
    MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
      MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "NonTerm") $
        (MIL.mkSimpleSrcType "IO")

monadErrorTypeCons :: [MIL.Type -> MIL.MonadType]
monadErrorTypeCons =
  [ \et ->
    MIL.MTyMonadCons (MIL.SinMonad MIL.State) $
      MIL.MTyMonadCons (MIL.SinMonadApp (MIL.SinMonad MIL.Error) et) $
        MIL.MTyMonad (MIL.SinMonad MIL.NonTerm)
  , \et ->
    MIL.MTyMonadCons (MIL.SinMonad MIL.State) $
      MIL.MTyMonadCons (MIL.SinMonadApp (MIL.SinMonad MIL.Error) et) $
        MIL.MTyMonadCons (MIL.SinMonad MIL.NonTerm) $
          MIL.MTyMonad (MIL.SinMonad MIL.IO)
  ]

exceptionSrcType :: MIL.SrcType
exceptionSrcType = MIL.mkSimpleSrcType "Unit"

builtInMilTypeDefs :: [MIL.SrcTypeDef]
builtInMilTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "String") []
      [ MIL.ConDef (MIL.ConName "Empty_Str") []
      , MIL.ConDef (MIL.ConName "Cons_Str") [MIL.mkSimpleSrcType "Char", MIL.mkSimpleSrcType "String"]]
  ]

builtInMilFunDefs :: [MIL.SrcFunDef]
builtInMilFunDefs =
  [ conTrue
  , conFalse
  , printStringMilDef
  ]
  ++ readStringMilDef
  ++
  [ printIntMilDef
  , readIntMilDef
  , printFloatMilDef
  , readFloatMilDef
  , evalStateMilDef
  , execStateMilDef
  , getMilDef
  , putMilDef
  , modifyMilDef
  ]

conTrue :: MIL.SrcFunDef
conTrue =
  MIL.mkSrcFunDef "con_True" (MIL.SrcTyApp pureSrcMonadMil (MIL.mkSimpleSrcType "Bool")) $
    MIL.ReturnE pureSrcMonadMil (MIL.mkSrcConName "True")

conFalse :: MIL.SrcFunDef
conFalse =
  MIL.mkSrcFunDef "con_False" (MIL.SrcTyApp pureSrcMonadMil (MIL.mkSimpleSrcType "Bool")) $
    MIL.ReturnE pureSrcMonadMil (MIL.mkSrcConName "False")

printStringMilDef :: MIL.SrcFunDef
printStringMilDef =
  MIL.mkSrcFunDef "printString" (monadTypeMil (getBuiltInFunctionType $ FunName "printString"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.mkSrcLambda (MIL.Var "s_") (MIL.mkSimpleSrcType "String") $
         MIL.CaseE (MIL.mkSrcVar "s_")
           [ MIL.CaseAlt (MIL.ConP (MIL.ConName "Empty_Str") [],
               MIL.ReturnE ioSrcMonadMil (MIL.LitE MIL.UnitLit))
           , MIL.CaseAlt (MIL.ConP (MIL.ConName "Cons_Str")
                 [ MIL.VarBinder (MIL.Var "c_", MIL.mkSimpleSrcType "Char")
                 , MIL.VarBinder (MIL.Var "cs_", MIL.mkSimpleSrcType "String")],
               MIL.mkSrcLet (MIL.Var "unit_0") (MIL.mkSimpleSrcType "Unit")
                 (MIL.LiftE (MIL.AppE (MIL.mkSrcVar "print_char") (MIL.mkSrcVar "c_"))
                    (MIL.mkSimpleSrcType "IO")
                    ioSrcMonadMil)
                 (MIL.mkSrcLet (MIL.Var "printString_") (MIL.getSrcResultType $ monadTypeMil (getBuiltInFunctionType $ FunName "printString"))
                    (MIL.mkSrcVar "printString") $
                    MIL.AppE (MIL.mkSrcVar "printString_") (MIL.mkSrcVar "cs_")))])

readStringMilDef :: [MIL.SrcFunDef]
readStringMilDef =
  [ MIL.mkSrcFunDef "readString" (monadTypeMil (getBuiltInFunctionType $ FunName "readString")) $
      MIL.AppE (MIL.mkSrcVar "readString_") (MIL.mkSrcConName "Empty_Str")
  , MIL.mkSrcFunDef "readString_" (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                                  (MIL.SrcTyApp ioSrcMonadMil (MIL.mkSimpleSrcType "String"))) $
      MIL.mkSrcLambda (MIL.Var "acc_") (MIL.mkSimpleSrcType "String") $
        MIL.mkSrcLet (MIL.Var "c_") (MIL.mkSimpleSrcType "Char")
          (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") ioSrcMonadMil)
          (MIL.CaseE (MIL.mkSrcVar "c_")
             [ MIL.CaseAlt (MIL.LitP $ MIL.CharLit ' ',
                 MIL.ReturnE ioSrcMonadMil (MIL.AppE (MIL.AppE (MIL.mkSrcVar "reverseString_") (MIL.mkSrcVar "acc_")) (MIL.mkSrcConName "Empty_Str")))
             , MIL.CaseAlt (MIL.DefaultP,
                 MIL.AppE (MIL.mkSrcVar "readString_") (MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str") (MIL.mkSrcVar "c_")) (MIL.mkSrcVar "acc_")))])
  , MIL.mkSrcFunDef "reverseString_" (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                                     (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String") (MIL.mkSimpleSrcType "String"))) $
      MIL.mkSrcLambda (MIL.Var "s_") (MIL.mkSimpleSrcType "String") $
        MIL.mkSrcLambda (MIL.Var "acc_") (MIL.mkSimpleSrcType "String") $
          MIL.CaseE (MIL.mkSrcVar "s_")
            [ MIL.CaseAlt (MIL.ConP (MIL.ConName "Empty_Str") [],
                MIL.mkSrcVar "acc_")
            , MIL.CaseAlt (MIL.ConP (MIL.ConName "Cons_Str")
                  [ MIL.VarBinder (MIL.Var "c_", MIL.mkSimpleSrcType "Char")
                  , MIL.VarBinder (MIL.Var "cs_", MIL.mkSimpleSrcType "String")],
                MIL.AppE (MIL.AppE (MIL.mkSrcVar "reverseString_") (MIL.mkSrcVar "cs_"))
                         (MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str") (MIL.mkSrcVar "c_")) (MIL.mkSrcVar "acc_")))
            ]
  ]

printIntMilDef :: MIL.SrcFunDef
printIntMilDef =
  MIL.mkSrcFunDef "printInt" (monadTypeMil (getBuiltInFunctionType $ FunName "printInt"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.mkSrcLambda (MIL.Var "i_") (MIL.mkSimpleSrcType "Int") $ MIL.ReturnE ioSrcMonadMil (MIL.LitE MIL.UnitLit))

readIntMilDef :: MIL.SrcFunDef
readIntMilDef =
  MIL.mkSrcFunDef "readInt" (monadTypeMil (getBuiltInFunctionType $ FunName "readInt"))
    (MIL.ReturnE ioSrcMonadMil (MIL.LitE $ MIL.IntLit 1))

printFloatMilDef :: MIL.SrcFunDef
printFloatMilDef =
  MIL.mkSrcFunDef "printFloat" (monadTypeMil (getBuiltInFunctionType $ FunName "printFloat"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.mkSrcLambda (MIL.Var "f_") (MIL.mkSimpleSrcType "Float") $ MIL.ReturnE ioSrcMonadMil (MIL.LitE MIL.UnitLit))

readFloatMilDef :: MIL.SrcFunDef
readFloatMilDef =
  MIL.mkSrcFunDef "readFloat" (monadTypeMil (getBuiltInFunctionType $ FunName "readFloat"))
    (MIL.ReturnE ioSrcMonadMil (MIL.LitE $ MIL.FloatLit 1.0))

evalStateMilDef :: MIL.SrcFunDef
evalStateMilDef =
  MIL.mkSrcFunDef "evalState" (monadTypeMil (getBuiltInFunctionType $ FunName "evalState"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.TypeLambdaE (MIL.TypeVar "S_") $
         MIL.ReturnE pureSrcMonadMil $
           MIL.TypeLambdaE (MIL.TypeVar "A_") $
             MIL.ReturnE pureSrcMonadMil $
               MIL.mkSrcLambda (MIL.Var "sa") (typeMil $ stateType (mkTypeVar "S_") (mkTypeVar "A_")) $
                 MIL.ReturnE pureSrcMonadMil $
                   MIL.mkSrcLambda (MIL.Var "s") (MIL.mkSimpleSrcType "S_") $
                     MIL.mkSrcLet (MIL.Var "state_") (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") (MIL.mkSimpleSrcType "S_"))
                       (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "new_ref") (MIL.mkSimpleSrcType "S_")) (MIL.VarE $ MIL.Var "s"))
                       (MIL.mkSrcLet (MIL.Var "sa_f") (MIL.getSrcResultType (typeMil $ stateType (mkTypeVar "S_") (mkTypeVar "A_"))) (MIL.VarE $ MIL.Var "sa") $
                          MIL.AppE (MIL.VarE $ MIL.Var "sa_f") (MIL.VarE $ MIL.Var "state_")))

execStateMilDef :: MIL.SrcFunDef
execStateMilDef =
  MIL.mkSrcFunDef "execState" (monadTypeMil (getBuiltInFunctionType $ FunName "execState"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.TypeLambdaE (MIL.TypeVar "S_") $
         MIL.ReturnE pureSrcMonadMil $
           MIL.TypeLambdaE (MIL.TypeVar "A_") $
             MIL.ReturnE pureSrcMonadMil $
               MIL.mkSrcLambda (MIL.Var "sa") (typeMil $ stateType (mkTypeVar "S_") (mkTypeVar "A_")) $
                 MIL.ReturnE pureSrcMonadMil $
                   MIL.mkSrcLambda (MIL.Var "s") (MIL.mkSimpleSrcType "S_") $
                     MIL.mkSrcLet (MIL.Var "state_") (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") (MIL.mkSimpleSrcType "S_"))
                       (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "new_ref") (MIL.mkSimpleSrcType "S_")) (MIL.VarE $ MIL.Var "s")) $
                       MIL.mkSrcLet (MIL.Var "res") (MIL.mkSimpleSrcType "A_")
                         (MIL.mkSrcLet (MIL.Var "sa_f") (MIL.getSrcResultType (typeMil $ stateType (mkTypeVar "S_") (mkTypeVar "A_"))) (MIL.VarE $ MIL.Var "sa") $
                            MIL.AppE (MIL.VarE $ MIL.Var "sa_f") (MIL.VarE $ MIL.Var "state_"))
                         (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "read_ref") (MIL.mkSimpleSrcType "S_"))
                                   (MIL.VarE $ MIL.Var "state_")))

getMilDef :: MIL.SrcFunDef
getMilDef =
  MIL.mkSrcFunDef "get" (monadTypeMil (getBuiltInFunctionType $ FunName "get"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.TypeLambdaE (MIL.TypeVar "S_") $
         MIL.ReturnE pureSrcMonadMil $
         MIL.mkSrcLambda (MIL.Var "state_") (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref")
                                                          (MIL.mkSimpleSrcType "S_")) $
           MIL.mkSrcLet (MIL.Var "state_value") (MIL.mkSimpleSrcType "S_")
             (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "read_ref") (MIL.mkSimpleSrcType "S_"))
                       (MIL.VarE $ MIL.Var "state_"))
             (MIL.ReturnE stateSrcMonadMil (MIL.VarE $ MIL.Var "state_value")))

putMilDef :: MIL.SrcFunDef
putMilDef =
  MIL.mkSrcFunDef "put" (monadTypeMil (getBuiltInFunctionType $ FunName "put"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.TypeLambdaE (MIL.TypeVar "S_") $
         MIL.ReturnE pureSrcMonadMil $
           MIL.mkSrcLambda (MIL.Var "state_value") (MIL.mkSimpleSrcType "S_") $
             MIL.ReturnE pureSrcMonadMil $
             MIL.mkSrcLambda (MIL.Var "state_") (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref")
                                                              (MIL.mkSimpleSrcType "S_")) $
               MIL.mkSrcLet (MIL.Var "unit_var") (MIL.mkSimpleSrcType "Unit")
                 (MIL.AppE (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "write_ref") (MIL.mkSimpleSrcType "S_"))
                                     (MIL.VarE $ MIL.Var "state_"))
                           (MIL.VarE $ MIL.Var "state_value"))
                 (MIL.ReturnE stateSrcMonadMil (MIL.LitE MIL.UnitLit)))

modifyMilDef :: MIL.SrcFunDef
modifyMilDef =
  MIL.mkSrcFunDef "modify" (monadTypeMil (getBuiltInFunctionType $ FunName "modify"))
    (MIL.ReturnE pureSrcMonadMil $
       MIL.TypeLambdaE (MIL.TypeVar "S_") $
         MIL.ReturnE pureSrcMonadMil $
           MIL.mkSrcLambda (MIL.Var "state_function") (MIL.SrcTyArrow (MIL.mkSimpleSrcType "S_")
                                                                      (MIL.SrcTyApp pureSrcMonadMil (MIL.mkSimpleSrcType "S_"))) $
             MIL.ReturnE pureSrcMonadMil $
             MIL.mkSrcLambda (MIL.Var "state_") (MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref")
                                                              (MIL.mkSimpleSrcType "S_")) $
               MIL.mkSrcLet (MIL.Var "state_value") (MIL.mkSimpleSrcType "S_")
                 (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "read_ref") (MIL.mkSimpleSrcType "S_"))
                           (MIL.VarE $ MIL.Var "state_")) $
                 MIL.mkSrcLet (MIL.Var "new_state_value") (MIL.mkSimpleSrcType "S_")
                   (MIL.AppE (MIL.VarE $ MIL.Var "state_function") (MIL.VarE $ MIL.Var "state_value")) $
                   (MIL.AppE (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "write_ref") (MIL.mkSimpleSrcType "S_"))
                                       (MIL.VarE $ MIL.Var "state_"))
                             (MIL.VarE $ MIL.Var "state_value")))

