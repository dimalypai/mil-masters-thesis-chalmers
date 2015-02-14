{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
--
-- All generated code operates in two monads: one for pure computations and
-- another one for default (impure) computations.
-- Statement sequences are represented with monadic binds.
-- Basically, all expressions (even the simplest ones, like literals and
-- variables) result in some sequence of binds (possibly empty) and return. We
-- give fresh names to subexpressions and introduce sequencing.
module OOLang.CodeGenMil
  ( codeGen
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
-- 'second' is used just to transform components of a pair
import Control.Arrow (second)

import OOLang.AST
import OOLang.AST.TypeAnnotated
import OOLang.AST.Helpers
import OOLang.TypeChecker
import OOLang.TypeChecker.TypeEnv
import OOLang.BuiltIn
import OOLang.Utils
import qualified MIL.AST as MIL
import qualified MIL.AST.Builder as MIL
import qualified MIL.AST.Helpers as MIL
import qualified MIL.BuiltIn as MIL

import System.IO.Unsafe

-- | Entry point to the code generator.
-- Takes a type checked program in OOLang and a type environment and produces a
-- source program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.SrcProgram
codeGen tyProgram typeEnv = unsafePerformIO $ runReaderT (evalStateTFrom 0 (runCG $ codeGenProgram tyProgram)) (typeEnv, Map.empty)

-- | Code generation monad. Uses 'StateT' for providing fresh variable names
-- and 'Reader' for querying the type environment and class types.
-- 'IO' may be used for debug printing.
newtype CodeGenM a = CG { runCG :: StateT NameSupply (ReaderT (TypeEnv, ClassTypes) IO) a }
  deriving (Monad, MonadState NameSupply, MonadReader (TypeEnv, ClassTypes), Functor, Applicative, MonadIO)

-- | All field and method names accessible from a class and a MIL type of the
-- object representation.
type ClassTypes = Map.Map ClassName ([Var], [FunName], MIL.SrcType)

-- | A counter for generating unique variable names.
type NameSupply = Int

-- | Entry point into the type checking of the program.
-- There is a list of MIL functions generated for each class definition.
-- There is an MIL function generated for each function definition.
-- All these definitions are then regrouped and the built-ins are added.
codeGenProgram :: TyProgram -> CodeGenM MIL.SrcProgram
codeGenProgram (Program _ tyClassDefs tyFunDefs) = do
  classTypes <- asks getClassTypes
  classTypes' <- foldM (\cts (ClassDef _ srcClassName _ _) ->
    collectClassTypes (getClassName srcClassName) cts)
    classTypes tyClassDefs
  local (second $ const classTypes') $ do
    classMilFunDefs <- concat <$> mapM codeGenClassDef tyClassDefs
    milFunDefs <- mapM codeGenFunDef tyFunDefs
    return $ MIL.Program (builtInMilTypeDefs, builtInMilFunDefs ++ (classMilFunDefs ++ milFunDefs))

-- | Collects information about the class: field and methods names. Constructs
-- a type of the object representation.
collectClassTypes :: ClassName -> ClassTypes -> CodeGenM ClassTypes
collectClassTypes className classTypes = do
  classTypeEnv <- asks (getClassTypeEnv . getTypeEnv)
  let (fieldNames, fieldTypes) = unzip (getClassFieldsAssoc className classTypeEnv)
      (methodNames, methodTypeInfos) = unzip (getClassMethodsAssoc className classTypeEnv)
      methodTypes = map ftiType methodTypeInfos
      -- TODO: virtual methods and self parameters
      classTupleType = MIL.SrcTyTuple [ MIL.SrcTyTuple $ map srcTypeMil fieldTypes
                                      , MIL.SrcTyTuple $ map srcTypeMil methodTypes]
  return $ Map.insert className (fieldNames, methodNames, classTupleType) classTypes

codeGenClassDef :: TyClassDef -> CodeGenM [MIL.SrcFunDef]
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) = do
  let (_, tyMethodDecls) = partitionClassMembers tyMembers
  mapM codeGenClassMethod tyMethodDecls

-- | TODO: inits (together with constructor).
-- TODO: use
{-
codeGenClassField :: ClassName -> TyFieldDecl -> CodeGenM MIL.SrcType
codeGenClassField className (FieldDecl _ tyDecl _) = do
  let fieldName = getVar $ getDeclVarName tyDecl
  fieldType <- asks (getClassFieldType className fieldName . getClassTypeEnv . getTypeEnv)
  return $ typeMil fieldType
-}
-- | TODO: add method specifics.
codeGenClassMethod :: TyMethodDecl -> CodeGenM MIL.SrcFunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> CodeGenM MIL.SrcFunDef
codeGenFunDef (FunDef _ srcFunName tyFunType tyStmts) = do
  let funName = getFunName srcFunName
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv . getTypeEnv)
  retType <- asks (ftiReturnType . getFunTypeInfo funName . getFunTypeEnv . getTypeEnv)
  let isPure = isPureType $ unReturn retType
  let funMonad = if isPure
                   then pureSrcMonadMil
                   else impureSrcMonadMil
  let milFunSrcType = srcFunTypeMil tyFunType retType
  (funBody, _) <- codeGenStmts tyStmts funMonad
  let funParams = getFunParams tyFunType
  let funBodyWithParams = foldr (\tyVarBinder e ->
                                   MIL.mkSrcLambda (varMil (getVar $ getBinderVar tyVarBinder))
                                                   (srcTypeMil $ getTypeOf tyVarBinder) e)
                            funBody funParams
  return $ MIL.FunDef (funNameMil funName) milFunSrcType funBodyWithParams

-- | List of statements is not empty.
-- Takes a monad of the containing function.
--
-- Declaration statement needs a special treatment to get variable scope right.
codeGenStmts :: [TyStmt] -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
{-
codeGenStmts [DeclS _ decl] funMonad =
  codeGenDecl decl funMonad ( MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit)
                            , MIL.applyMonadType funMonad MIL.unitType)
-}
codeGenStmts [tyStmt] funMonad = codeGenStmt tyStmt funMonad
{-
codeGenStmts ((DeclS _ decl):tyStmts) funMonad = do
  milBodyExprWithType <- codeGenStmts tyStmts funMonad
  codeGenDecl decl funMonad milBodyExprWithType
-}
codeGenStmts (tyStmt:tyStmts) funMonad = do
  var <- newMilVar
  (milBindExpr, milBindExprType) <- codeGenStmt tyStmt funMonad
  (milBodyExpr, milBodyExprType) <- codeGenStmts tyStmts funMonad
  return ( MIL.mkSrcLet var (MIL.getSrcResultType milBindExprType) milBindExpr milBodyExpr
         , milBodyExprType)

-- | Takes a monad of the containing function.
codeGenStmt :: TyStmt -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenStmt tyStmt funMonad =
  case tyStmt of
    ExprS _ tyExpr -> codeGenExpr tyExpr funMonad

    -- TODO:
    -- + "then" with state putting operations
    -- + ANF/SSA construction
    --AssignS _ srcAssignOp tyExprLeft tyExprRight _ -> undefined

    DeclS {} -> error "codeGenStmt: DeclS should have a special treatment."
{-
-- | Code generation for declarations.
-- It takes an expression which will become a body of the monadic bind, where a
-- declared variable will be in scope and a type of this expression.
codeGenDecl :: TyDeclaration -> MIL.TypeM -> (MIL.Expr, MIL.Type) -> CodeGenM (MIL.Expr, MIL.Type)
codeGenDecl (Decl _ tyVarBinder mTyInit _) funMonad (milBodyExpr, milBodyExprType) = do
  (milInitExpr, _) <- case mTyInit of
                        Just tyInit -> codeGenExpr (getInitExpr tyInit) funMonad
                        -- It may be a variable with Mutable type, so we need
                        -- 'getUnderType'.
                        Nothing -> codeGenExpr (maybeDefaultExpr $ getUnderType $ getTypeOf tyVarBinder) funMonad
  return ( MIL.LetE ( MIL.VarBinder (varMil (getVar $ getBinderVar tyVarBinder)
                    , typeMil $ getTypeOf tyVarBinder))
             milInitExpr
             milBodyExpr
         , milBodyExprType)
-}
-- | Expression code generation.
-- Takes a monad of the containing function.
codeGenExpr :: TyExpr -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenExpr tyExpr funMonad =
  case tyExpr of
    LitE lit -> return
      ( MIL.ReturnE funMonad (literalMil lit)
      , MIL.SrcTyApp funMonad (srcTypeMil $ getTypeOf tyExpr))

    VarE _ varType var varPure -> do
      varCase <- getVarCase var varType
      case varCase of
        LocalVarValueType ->
          return ( MIL.ReturnE funMonad (MIL.VarE $ varMil var)
                 , MIL.SrcTyApp funMonad (srcTypeMil varType))
        LocalVarFunType ->
          return ( MIL.ReturnE funMonad (MIL.VarE $ varMil var)
                 , MIL.SrcTyApp funMonad (srcTypeMil varType))
        GlobalFunWithParams ->
          return ( MIL.ReturnE funMonad (MIL.VarE $ varMil var)
                 , MIL.SrcTyApp funMonad (srcTypeMil varType))
        GlobalFunWithoutParams ->
          return ( MIL.VarE $ varMil var
                 , funSrcTypeMil varType)

    BinOpE _ resultType srcBinOp tyExpr1 tyExpr2 _ ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 resultType funMonad

    ParenE _ tySubExpr -> codeGenExpr tySubExpr funMonad
{-
    VarE _ varType var varPure -> do
      let funName = varToFunName var
      if isBuiltInFunction funName
        then codeGenBuiltInFunction funMonad var
        else do let varE = MIL.VarE $ MIL.VarBinder (varMil var, typeMil varType)
                case (isValueType varType, isPureFunType varType, funMonad == pureMonadMil) of
                  -- It is a function, so to make it monadic value, we need return.
                  (False, _, _) -> return ( MIL.ReturnE funMonad varE
                                          , MIL.applyMonadType funMonad (typeMil varType))
                  -- It is an impure value type inside a pure function, so it is a local
                  -- variable or parameter and it is pure, we need return.
                  (True, False, True) -> return ( MIL.ReturnE funMonad varE
                                                , MIL.applyMonadType funMonad (typeMil varType))
                  -- Pure_M monad value inside a pure or impure function.
                  (True, True, _) -> return (varE, typeMil varType)
                  -- It can be an impure value inside an impure function (must be global
                  -- impure function), then use 'funTypeMil' for type annotation, or it
                  -- can be a local variable, look at its purity annotation.
                  (True, False, False) -> if varPure
                                            then return ( MIL.ReturnE funMonad varE
                                                        , MIL.applyMonadType funMonad (typeMil varType))
                                            else return ( MIL.VarE $ MIL.VarBinder ( varMil var
                                                                                   , funTypeMil varType)
                                                        , funTypeMil varType)

    BinOpE _ resultType srcBinOp tyExpr1 tyExpr2 _ ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 resultType funMonad
-}
literalMil :: TyLiteral -> MIL.SrcExpr
literalMil UnitLit {} = MIL.LitE MIL.UnitLit
literalMil (BoolLit _ _ b) =
  if b
    then MIL.mkSrcConName "True"
    else MIL.mkSrcConName "False"
literalMil (IntLit _ _ i)     = MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ _ f _) = MIL.LitE (MIL.FloatLit f)
literalMil (NothingLit _ t _) =
  -- Monomorphise Nothing constructor.
  MIL.TypeAppE
    (MIL.mkSrcConName "Nothing")
    (srcTypeMil $ getMaybeUnderType t)

-- var can be:
-- + local variable of value type
-- + local variable of function type
-- + global pure function (without parameters)
-- + global impure function (without parameters)
-- + global pure/impure function with parameters
-- context can be:
-- + pure
-- + impure
data VarCase
  = LocalVarValueType
  | LocalVarFunType
  | GlobalFunWithParams
  | GlobalFunWithoutParams

getVarCase :: Var -> Type -> CodeGenM VarCase
getVarCase var varType = do
  let funName = varToFunName var
  isGlobalFun <- asks (isFunctionDefined funName . getFunTypeEnv . getTypeEnv)
  if isGlobalFun
    then do
      arity <- asks (ftiArity . getFunTypeInfo funName . getFunTypeEnv . getTypeEnv)
      if arity == 0
        then return GlobalFunWithoutParams
        else return GlobalFunWithParams
    else
      if isValueType varType
        then return LocalVarValueType
        else return LocalVarFunType

-- | Takes a monad of the containing function.
codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> Type -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenBinOp binOp tyExpr1 tyExpr2 resultType funMonad =
  case binOp of
    App -> do
      (milExpr1, milExpr1Type) <- codeGenExpr tyExpr1 funMonad
      (milExpr2, milExpr2Type) <- codeGenExpr tyExpr2 funMonad
      var1 <- newMilVar
      var2 <- newMilVar
      let (appE, milResultType) = if isValueType resultType
                                    then ( MIL.AppE (MIL.VarE var1) (MIL.VarE var2)
                                         , funSrcTypeMil resultType)
                                    else ( MIL.ReturnE funMonad (MIL.AppE (MIL.VarE var1) (MIL.VarE var2))
                                         -- TODO: It seems like it doesn't matter if
                                         -- 'srcTypeMil' or 'funSrcTypeMil' is used,
                                         -- because they do the same for the
                                         -- function (arrow) type. Is there a
                                         -- counter example?
                                         , MIL.SrcTyApp funMonad (srcTypeMil resultType))
      return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
                 MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2
                   appE
             , milResultType)
{-
-- | Takes a monad of the containing function.
codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> Type -> MIL.TypeM -> CodeGenM (MIL.Expr, MIL.Type)
codeGenBinOp App tyExpr1 tyExpr2 resultType funMonad = do
  let var1Type = MIL.getMonadResultType milExpr1Type
  let var2Type = MIL.getMonadResultType milExpr2Type
  -- Type checker ensured that the type of the left operand is a function type.
  -- All generated code is monadic, so it must be in a monad.
  let (MIL.TyArrow _ (MIL.TyApp (MIL.TyMonad var1Monad) _)) = var1Type
  let (appBodyExpr, appBodyType) = case (isValueType resultType, isPureFunType resultType, funMonad == pureMonadMil) of
                                     -- It is a partially applied function, so to make it monadic
                                     -- value, we need return.
                                     (False, _, _) -> (MIL.ReturnE funMonad appE, MIL.applyMonadType funMonad (funTypeMil resultType))
                                     --(True, False, True) -> MIL.ReturnE funMonad appE  -- TODO: should this be possible?
                                     -- Fully applied Pure function inside a pure or impure function.
                                     (True, True, _) -> (appE, funTypeMil resultType)
                                     -- Fully applied impure function inside an impure function.
                                     (True, False, False) ->
                                       if var1Monad /= funMonad  -- TODO: is it enough? alphaEq? aliases?
                                         then (MIL.LiftE appE var1Monad funMonad, funTypeMil resultType)
                                         else (appE, funTypeMil resultType)

-- | Built-in functions need a special treatment.
-- TODO: wrong types in bind
codeGenBuiltInFunction :: MIL.TypeM -> Var -> CodeGenM (MIL.Expr, MIL.Type)
codeGenBuiltInFunction funMonad funNameVar = do
  case funNameVar of
    Var "printString" -> codeGenArgBuiltInFunction (MIL.FunName "printString") funMonad
    Var "printBool"   -> do
      let milFunType = MIL.TyArrow MIL.boolType (MIL.ioType MIL.unitType)
      return ( MIL.ReturnE funMonad (MIL.VarE $ MIL.VarBinder (varMil funNameVar, milFunType))
             , MIL.applyMonadType funMonad milFunType)
    Var "printInt"    -> codeGenArgBuiltInFunction (MIL.FunName "printInt") funMonad
    Var "printFloat"  -> codeGenArgBuiltInFunction (MIL.FunName "printFloat") funMonad

    Var "readBool"    -> do
      let milFunType = MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.boolType
      return (MIL.VarE $ MIL.VarBinder (varMil funNameVar, milFunType), milFunType)
    Var "readInt"     -> codeGenNoArgBuiltInFunction (MIL.FunName "readInt") funMonad
    Var "readFloat"   -> codeGenNoArgBuiltInFunction (MIL.FunName "readFloat") funMonad

    _ -> error (prPrint funNameVar ++ "is not a built-in function")

-- | These functions have at least one parameter, so we just return them in the
-- function monad.
codeGenArgBuiltInFunction :: MIL.FunName -> MIL.TypeM -> CodeGenM (MIL.Expr, MIL.Type)
codeGenArgBuiltInFunction milFunName funMonad = do
  let milFunNameVar = MIL.funNameToVar milFunName
      milFunType = getMilBuiltInFunType milFunName
  return ( MIL.ReturnE funMonad (MIL.VarE $ MIL.VarBinder (milFunNameVar, milFunType))
         , MIL.applyMonadType funMonad milFunType)

-- | These functions don't expect arguments. We must lift them if they are in a
-- different monad. Type checking ensured that they are in the correct monad.
codeGenNoArgBuiltInFunction :: MIL.FunName -> MIL.TypeM -> CodeGenM (MIL.Expr, MIL.Type)
codeGenNoArgBuiltInFunction milFunName funMonad = do
  let milFunNameVar = MIL.funNameToVar milFunName
      milFunType = getMilBuiltInFunType milFunName
      (MIL.TyApp (MIL.TyMonad resultMonad) monadResultType) = milFunType
  if resultMonad /= funMonad  -- TODO: is it enough? alphaEq? aliases?
    then return ( MIL.LiftE (MIL.VarE $ MIL.VarBinder (milFunNameVar, milFunType)) resultMonad funMonad
                , MIL.applyMonadType funMonad monadResultType)
    else return ( MIL.VarE $ MIL.VarBinder (milFunNameVar, milFunType)
                , MIL.applyMonadType funMonad monadResultType)

-- | Note [Variable type in bind]:
--
-- In bind, the variable type should be "atomic" (non-monadic), so we must get
-- rid of 'TyPure'.

-- * Type conversions
-- TODO: more Pure_M???
typeMil (TyArrow t1 t2)     = MIL.TyArrow (typeMil t1) (funTypeMil t2)  -- TODO: Pure on the left?
funTypeMil (TyArrow t1 t2) = MIL.TyArrow (typeMil t1) (funTypeMil t2)
-}

-- | Note [Type transformation]:
--
-- There are two versions of type transformation: 'typeMil' and 'funTypeMil'.
-- Usually, on the top-level, 'typeMil' should be used, except for the cases,
-- where it is known that the type being transformed is a type of the function.
-- Another case, where 'funTypeMil' is used is when it is known that the type
-- being transformed is the type of the global impure function.
--
-- 'typeMil' introduces impure monad stack only for function return type.
-- 'funTypeMil' introduces impure monad stack also for atomic types, which are
-- not Pure.
--
-- It is important, that 'funTypeMil' recursive call is used only on the
-- right-hand side of the function arrow, since this is the only place, where
-- impure monad stack should be introduced, when 'funTypeMil' is not a
-- top-level call (in this case, atomic types will get impure monad stack as
-- well).
--
-- 'srcFunTypeMil' is introduced because we need to know where is the return
-- type to transform the type correctly (for which 'funSrcTypeMilRetType' is
-- used).

-- | See Note [Type transformation].
srcTypeMil :: Type -> MIL.SrcType
srcTypeMil TyUnit   = MIL.mkSimpleSrcType "Unit"
srcTypeMil TyBool   = MIL.mkSimpleSrcType "Bool"
srcTypeMil TyInt    = MIL.mkSimpleSrcType "Int"
srcTypeMil TyFloat  = MIL.mkSimpleSrcType "Float"
srcTypeMil TyString = MIL.mkSimpleSrcType "String"
srcTypeMil (TyClass className) = MIL.SrcTyTypeCon $ typeNameMil className
srcTypeMil (TyArrow t1 t2) = MIL.SrcTyArrow (srcTypeMil t1) (funSrcTypeMil t2)
srcTypeMil (TyPure t)      = MIL.SrcTyApp pureSrcMonadMil (srcTypeMil t)
srcTypeMil (TyMaybe t)     = MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") (srcTypeMil t)

-- | See Note [Type transformation].
funSrcTypeMil :: Type -> MIL.SrcType
funSrcTypeMil (TyArrow t1 t2) = MIL.SrcTyArrow (srcTypeMil t1) (funSrcTypeMil t2)
funSrcTypeMil (TyPure t)      = MIL.SrcTyApp pureSrcMonadMil (srcTypeMil t)
funSrcTypeMil (TyMaybe t)     = MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") (srcTypeMil t)
funSrcTypeMil t = MIL.SrcTyApp impureSrcMonadMil (srcTypeMil t)

-- | See Note [Type transformation].
srcFunTypeMil :: TyFunType -> ReturnType -> MIL.SrcType
srcFunTypeMil (FunType _ tyVarBinders _) retType =
  foldr (\(VarBinder _ t _ _) acc -> MIL.SrcTyArrow (srcTypeMil t) acc) (funSrcTypeMilRetType retType) tyVarBinders

-- | See Note [Type transformation].
funSrcTypeMilRetType :: ReturnType -> MIL.SrcType
funSrcTypeMilRetType (ReturnType (TyPure t))  = MIL.SrcTyApp pureSrcMonadMil (srcTypeMil t)
funSrcTypeMilRetType (ReturnType (TyMaybe t)) = MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") (srcTypeMil t)
funSrcTypeMilRetType (ReturnType t) = MIL.SrcTyApp impureSrcMonadMil (srcTypeMil t)

-- * Conversion utils

typeNameMil :: ClassName -> MIL.TypeName
typeNameMil (ClassName classNameStr) = MIL.TypeName classNameStr

conNameMil :: ClassName -> MIL.ConName
conNameMil (ClassName classNameStr) = MIL.ConName classNameStr

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

-- * CodeGenM operations

newMilVar :: CodeGenM MIL.Var
newMilVar = do
  i <- get
  modify (+1)
  return $ MIL.Var ("var_" ++ show i)

getTypeEnv :: (TypeEnv, ClassTypes) -> TypeEnv
getTypeEnv = fst

getClassTypes :: (TypeEnv, ClassTypes) -> ClassTypes
getClassTypes = snd

