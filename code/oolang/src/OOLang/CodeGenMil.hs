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
-- Used just to transform components of a pair
import Control.Arrow (first, second, (***))
import Data.Maybe (fromMaybe, isJust, fromJust)

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
codeGen tyProgram typeEnv = unsafePerformIO $ runReaderT (evalStateTFrom (0, Map.empty) (runCG $ codeGenProgram tyProgram)) (typeEnv, Map.empty)

-- | Code generation monad. Uses 'StateT' for providing fresh variable names
-- and 'Reader' for querying the type environment and class types.
-- 'IO' may be used for debug printing.
newtype CodeGenM a = CG { runCG :: StateT (NameSupply, VarMap) (ReaderT (TypeEnv, ClassTypes) IO) a }
  deriving (Monad, MonadState (NameSupply, VarMap), MonadReader (TypeEnv, ClassTypes), Functor, Applicative, MonadIO)

-- | All field and method names accessible from a class and a MIL type of the
-- object representation.
type ClassTypes = Map.Map ClassName ([Var], [FunName], MIL.SrcType)

-- | A counter for generating unique variable names.
type NameSupply = Int

-- | A map to keep track of the last occurence of a variable
-- and generate fresh names for new occurences. Used for assignments.
type VarMap = Map.Map Var Int

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
    (classMilTypeDefs, classMilFunDefs) <- (concat *** concat) <$> (unzip <$> mapM codeGenClassDef tyClassDefs)
    milFunDefs <- mapM codeGenFunDef tyFunDefs
    return $ MIL.Program (builtInMilTypeDefs ++ classMilTypeDefs, builtInMilFunDefs ++ (classMilFunDefs ++ milFunDefs))

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

codeGenClassDef :: TyClassDef -> CodeGenM ([MIL.SrcTypeDef], [MIL.SrcFunDef])
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) = do
  let className = getClassName srcClassName
  let (tyFieldDecls, tyMethodDecls) = partitionClassMembers tyMembers
  classDataTypeDef <- codeGenClassDataTypeDef className tyFieldDecls
  let classMilTypeDefs = [ classDataTypeDef
                         , codeGenClassTypeDef className
                         ]
  classDataConstructor <- codeGenClassDataConstructor className tyFieldDecls
  let classMilFunDefs = [ classDataConstructor
                        , codeGenClassConstructor className
                        , codeGenClassFunDef className
                        ]
  return (classMilTypeDefs, classMilFunDefs)

codeGenClassDataTypeDef :: ClassName -> [TyFieldDecl] -> CodeGenM MIL.SrcTypeDef
codeGenClassDataTypeDef className tyFieldDecls = do
  fieldTypes <- mapM (classFieldTypeMil className) tyFieldDecls
  return $ MIL.TypeDef (classDataTypeNameMil className) []
    [MIL.ConDef (classDataConNameMil className) [MIL.SrcTyTuple fieldTypes]]

classFieldTypeMil :: ClassName -> TyFieldDecl -> CodeGenM MIL.SrcType
classFieldTypeMil className (FieldDecl _ tyDecl _) = do
  let fieldName = getVar $ getDeclVarName tyDecl
  fieldType <- asks (getClassFieldType className fieldName . getClassTypeEnv . getTypeEnv)
  return $ srcTypeMil fieldType

codeGenClassTypeDef :: ClassName -> MIL.SrcTypeDef
codeGenClassTypeDef className =
  MIL.TypeDef (typeNameMil className) []
    [MIL.ConDef (conNameMil className) [MIL.SrcTyTuple [classDataSrcTypeMil className]]]

codeGenClassDataConstructor :: ClassName -> [TyFieldDecl] -> CodeGenM MIL.SrcFunDef
codeGenClassDataConstructor className tyFieldDecls = do
  fieldDeclsExpr <- codeGenClassFieldDecls className tyFieldDecls
  return $ MIL.FunDef (classDataConstructorNameMil className)
    (MIL.SrcTyApp pureSrcMonadMil (classDataSrcTypeMil className)) fieldDeclsExpr

-- | TODO: constructor.
codeGenClassFieldDecls :: ClassName -> [TyFieldDecl] -> CodeGenM MIL.SrcExpr
codeGenClassFieldDecls className tyFieldDecls = do
  let fieldVars = map (classFieldVar . getVar . getFieldDeclVarName) tyFieldDecls
  let conExpr = MIL.ReturnE pureSrcMonadMil $
       MIL.AppE (MIL.ConNameE (classDataConNameMil className) ()) (MIL.TupleE $ map (MIL.VarE . varMil) fieldVars)
  codeGenFields conExpr tyFieldDecls

-- | Takes an expression that will use a sequence of class field bind
-- expressions at the end.
-- Starts with this expression at the bottom and builds a sequence of binds
-- upwards (hence reverse).
codeGenFields :: MIL.SrcExpr -> [TyFieldDecl] -> CodeGenM MIL.SrcExpr
codeGenFields milBodyExpr tyFieldDecls =
  foldM (\e (FieldDecl _ tyDecl _) -> fst <$> codeGenDecl tyDecl classFieldVar pureSrcMonadMil (e, undefined))
    milBodyExpr (reverse tyFieldDecls)

codeGenClassConstructor :: ClassName -> MIL.SrcFunDef
codeGenClassConstructor className =
  MIL.FunDef (classConstructorNameMil className) (MIL.SrcTyApp pureSrcMonadMil (classSrcTypeMil className)) $
    MIL.mkSrcLet (MIL.Var classDataVarName) (classDataSrcTypeMil className) (MIL.VarE (MIL.funNameToVar $ classDataConstructorNameMil className))
      (MIL.AppE (MIL.VarE (MIL.funNameToVar $ classDefNameMil className)) (MIL.mkSrcVar classDataVarName))

codeGenClassFunDef :: ClassName -> MIL.SrcFunDef
codeGenClassFunDef className =
  MIL.FunDef (classDefNameMil className) (classDefSrcTypeMil className) $
    MIL.mkSrcLambda (MIL.Var classDataVarName) (classDataSrcTypeMil className) $
      MIL.ReturnE pureSrcMonadMil (MIL.AppE (MIL.ConNameE (conNameMil className) ()) (MIL.TupleE [MIL.mkSrcVar classDataVarName]))

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
  resetVariablesMap
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
-- Declaration and assignment statements need a special treatment to get
-- variable scope right.
codeGenStmts :: [TyStmt] -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenStmts [DeclS _ decl] funMonad =
  codeGenDecl decl id funMonad ( MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit)
                               , MIL.SrcTyApp funMonad (MIL.mkSimpleSrcType "Unit"))
codeGenStmts [stmt@(AssignS {})] funMonad = do
  preCodeGenAssign stmt
  codeGenAssign stmt funMonad ( MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit)
                              , MIL.SrcTyApp funMonad (MIL.mkSimpleSrcType "Unit"))
codeGenStmts [tyStmt] funMonad = codeGenStmt tyStmt funMonad

codeGenStmts ((DeclS _ decl):tyStmts) funMonad = do
  milBodyExprWithType <- codeGenStmts tyStmts funMonad
  codeGenDecl decl id funMonad milBodyExprWithType
codeGenStmts (stmt@(AssignS {}):tyStmts) funMonad = do
  preCodeGenAssign stmt
  milBodyExprWithType <- codeGenStmts tyStmts funMonad
  codeGenAssign stmt funMonad milBodyExprWithType
codeGenStmts (tyStmt:tyStmts) funMonad = do
  (milBindExpr, milBindExprType) <- codeGenStmt tyStmt funMonad
  var <- newMilVar
  (milBodyExpr, milBodyExprType) <- codeGenStmts tyStmts funMonad
  return ( MIL.mkSrcLet var (MIL.getSrcResultType milBindExprType) milBindExpr milBodyExpr
         , milBodyExprType)

-- | Takes a monad of the containing function.
codeGenStmt :: TyStmt -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenStmt tyStmt funMonad =
  case tyStmt of
    ExprS _ tyExpr -> codeGenExpr tyExpr funMonad

    AssignS {} -> error "codeGenStmt: AssignS should have a special treatment."
    DeclS {} -> error "codeGenStmt: DeclS should have a special treatment."

-- | Code generation for declarations.
-- It takes an expression which will become a body of the monadic bind, where a
-- declared variable will be in scope and a type of this expression.
-- It also takes a function to transform a variable that is being declared.
-- Used to add extra prefixes/suffixes to the name.
codeGenDecl :: TyDeclaration -> (Var -> Var) -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenDecl (Decl _ tyVarBinder mTyInit _) varTransform funMonad (milBodyExpr, milBodyExprType) = do
  let var = getVar $ getBinderVar tyVarBinder
  (milInitExpr, milInitExprType) <-
    case mTyInit of
      Just tyInit -> codeGenExpr (getInitExpr tyInit) funMonad
      -- It may be a variable with Mutable type, so we need 'getUnderType'.
      Nothing -> codeGenExpr (maybeDefaultExpr $ getUnderType $ getTypeOf tyVarBinder) funMonad
  return ( MIL.mkSrcLet (varMil $ varTransform var) (MIL.getSrcResultType milInitExprType) milInitExpr milBodyExpr
         , milBodyExprType)

-- | Fresh name for new assigned variable occurence should be generated
-- separately, because of the order in which code is generated for statements.
preCodeGenAssign :: TyStmt -> CodeGenM ()
preCodeGenAssign (AssignS _ srcAssignOp tyExprLeft _ _) = do
  case getAssignOp srcAssignOp of
    AssignMut -> preCodeGenAssignMut tyExprLeft
    _ -> return ()

-- | Code generation for assignments.
-- It takes an expression which will become a body of the monadic bind, where a
-- new occurence of the assigned variable will be in scope and a type of this
-- expression.
codeGenAssign :: TyStmt -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenAssign (AssignS _ srcAssignOp tyExprLeft tyExprRight _) funMonad milBodyExprWithType =
  case getAssignOp srcAssignOp of
    AssignMut -> codeGenAssignMut tyExprLeft tyExprRight funMonad milBodyExprWithType
    AssignRef -> codeGenAssignRef tyExprLeft tyExprRight funMonad milBodyExprWithType

preCodeGenAssignMut :: TyExpr -> CodeGenM ()
preCodeGenAssignMut tyExprLeft =
  case tyExprLeft of
    VarE _ _ var _ -> void $ nextVar var

codeGenAssignMut :: TyExpr -> TyExpr -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenAssignMut tyExprLeft tyExprRight funMonad (milBodyExpr, milBodyExprType) =
  case tyExprLeft of
    VarE _ _ var _ -> do
      milVar <- currentVar var
      (milExprRight, milExprRightType) <- codeGenExpr tyExprRight funMonad
      return ( MIL.mkSrcLet milVar (MIL.getSrcResultType milExprRightType) milExprRight milBodyExpr
             , milBodyExprType)

codeGenAssignRef :: TyExpr -> TyExpr -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenAssignRef tyExprLeft tyExprRight funMonad (milBodyExpr, milBodyExprType) =
  case tyExprLeft of
    VarE _ _ var _ -> do
      (milExprRight, milExprRightType) <- codeGenExpr tyExprRight funMonad
      exprRightVar <- newMilVar
      stmtResultVar <- newMilVar
      return ( MIL.mkSrcLet stmtResultVar (MIL.mkSimpleSrcType "Unit")
                 (MIL.mkSrcLet exprRightVar (MIL.getSrcResultType milExprRightType) milExprRight $
                    MIL.LiftE (MIL.AppE (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "write_ref") (MIL.getSrcResultType milExprRightType))
                                                  (MIL.VarE $ varMil var))
                                        (MIL.VarE exprRightVar))
                      (MIL.mkSimpleSrcType "State")
                      impureSrcMonadMilWithStateBase)
                 milBodyExpr
             , MIL.SrcTyApp funMonad (MIL.mkSimpleSrcType "Unit"))

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
      milVar <- currentVar var
      case varCase of
        LocalVarValueType ->
          return ( MIL.ReturnE funMonad (MIL.VarE milVar)
                 , MIL.SrcTyApp funMonad (srcTypeMil varType))
        LocalVarFunType ->
          return ( MIL.ReturnE funMonad (MIL.VarE milVar)
                 , MIL.SrcTyApp funMonad (srcTypeMil varType))
        GlobalFunWithParams ->
          return ( MIL.ReturnE funMonad (MIL.VarE milVar)
                 , MIL.SrcTyApp funMonad (srcTypeMil varType))
        GlobalFunWithoutParams ->
          return ( MIL.VarE milVar
                 , funSrcTypeMil varType)

    -- Only 'new'
    ClassAccessE _ t srcClassName _ -> do
      let className = getClassName srcClassName
      return ( MIL.VarE (MIL.funNameToVar $ classConstructorNameMil className)
             , MIL.SrcTyApp pureSrcMonadMil (srcTypeMil t))

    NewRefE _ _ tyRefUnderExpr -> do
      (milRefUnderExpr, milRefUnderExprType) <- codeGenExpr tyRefUnderExpr funMonad
      refUnderExprVar <- newMilVar
      let milRefUnderType = MIL.getSrcResultType milRefUnderExprType
      let milRefType = MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") milRefUnderType
      return ( MIL.mkSrcLet refUnderExprVar milRefUnderType milRefUnderExpr $
                 (MIL.LiftE (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "new_ref") milRefUnderType)
                                      (MIL.VarE refUnderExprVar))
                    (MIL.mkSimpleSrcType "State")
                    impureSrcMonadMilWithStateBase)
             , MIL.SrcTyApp funMonad milRefType)

    BinOpE _ resultType srcBinOp tyExpr1 tyExpr2 _ ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 resultType funMonad

    -- TODO: Should the type from JustE annotation be used more?
    JustE _ _ tySubExpr -> do
      (milSubExpr, milSubExprMonadType) <- codeGenExpr tySubExpr funMonad
      let milSubExprType = MIL.getSrcResultType milSubExprMonadType
      var <- newMilVar
      return ( MIL.mkSrcLet var milSubExprType milSubExpr $
                 MIL.ReturnE funMonad (MIL.AppE (MIL.TypeAppE (MIL.mkSrcConName "Just") milSubExprType)
                                                (MIL.VarE var))
             , MIL.SrcTyApp funMonad $ MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") milSubExprType)

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
-}
literalMil :: TyLiteral -> MIL.SrcExpr
literalMil UnitLit {} = MIL.LitE MIL.UnitLit
literalMil (BoolLit _ _ b) =
  if b
    then MIL.mkSrcConName "True"
    else MIL.mkSrcConName "False"
literalMil (IntLit _ _ i)     = MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ _ f _) = MIL.LitE (MIL.FloatLit f)
literalMil (StringLit _ _ s)  = stringMil s
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
  let (appBodyExpr, appBodyType) = case (isValueType resultType, isPureFunType resultType, funMonad == pureMonadMil) of
                                     (True, False, True) -> MIL.ReturnE funMonad appE  -- TODO: should this be possible?

-- | Note [Variable type in bind]:
--
-- In bind, the variable type should be "atomic" (non-monadic), so we must get
-- rid of 'TyPure'.
-}

-- * Type conversions

-- | Note [Type transformation]:
--
-- There are two main versions of type transformation: 'srcTypeMil' and
-- 'funSrcTypeMil'. Usually, on the top-level, 'srcTypeMil' should be used,
-- except for the cases, where it is known that the type being transformed is a
-- type of a function (?). Another case, where 'funSrcTypeMil' is used is when
-- it is known that the type being transformed is the type of the global impure
-- function (with arity 0).
--
-- 'srcTypeMil' introduces impure monad stack only for function return type.
-- 'funSrcTypeMil' introduces impure monad stack also for atomic types, which
-- are not Pure.
--
-- It is important, that 'funSrcTypeMil' recursive call is used only on the
-- right-hand side of the function arrow, since this is the only place, where
-- impure monad stack should be introduced, when 'funSrcTypeMil' is not a
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
srcTypeMil (TyMutable t)   = srcTypeMil t
srcTypeMil (TyRef t)       = MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") (srcTypeMil t)

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

-- * Conversion utils, name helpers

typeNameMil :: ClassName -> MIL.TypeName
typeNameMil (ClassName classNameStr) = MIL.TypeName classNameStr

classDataTypeNameMil :: ClassName -> MIL.TypeName
classDataTypeNameMil (ClassName classNameStr) = MIL.TypeName (classNameStr ++ classDataSuffix)

conNameMil :: ClassName -> MIL.ConName
conNameMil (ClassName classNameStr) = MIL.ConName classNameStr

classDataConNameMil :: ClassName -> MIL.ConName
classDataConNameMil (ClassName classNameStr) = MIL.ConName (classNameStr ++ classDataSuffix)

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

classDataConstructorNameMil :: ClassName -> MIL.FunName
classDataConstructorNameMil (ClassName classNameStr) = MIL.FunName ("new_" ++ (classNameStr ++ classDataSuffix))

classConstructorNameMil :: ClassName -> MIL.FunName
classConstructorNameMil (ClassName classNameStr) = MIL.FunName ("new_" ++ classNameStr)

classDefNameMil :: ClassName -> MIL.FunName
classDefNameMil (ClassName classNameStr) = MIL.FunName ("class_" ++ classNameStr)

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

classFieldVar :: Var -> Var
classFieldVar (Var varStr) = Var ("self_" ++ varStr)

classDataSrcTypeMil :: ClassName -> MIL.SrcType
classDataSrcTypeMil (ClassName classNameStr) = MIL.mkSimpleSrcType (classNameStr ++ classDataSuffix)

classSrcTypeMil :: ClassName -> MIL.SrcType
classSrcTypeMil (ClassName classNameStr) = MIL.mkSimpleSrcType classNameStr

classDefSrcTypeMil :: ClassName -> MIL.SrcType
classDefSrcTypeMil className =
  MIL.SrcTyArrow (classDataSrcTypeMil className) (MIL.SrcTyApp pureSrcMonadMil (classSrcTypeMil className))

classDataSuffix :: String
classDataSuffix = "_Data"

classDataVarName :: String
classDataVarName = "self_data"

-- * CodeGenM operations

newMilVar :: CodeGenM MIL.Var
newMilVar = do
  i <- fst <$> get
  modify $ first (+1)
  return $ MIL.Var ("var_" ++ show i)

-- | Since VarMap is used for local variables, it needs to be reset between
-- functions.
resetVariablesMap :: CodeGenM ()
resetVariablesMap = modify (second $ const Map.empty)

nextVar :: Var -> CodeGenM MIL.Var
nextVar var@(Var varStr) = do
  varMap <- snd <$> get
  let i' = case Map.lookup var varMap of
             Just i -> i + 1
             Nothing -> 1
  modify (second $ Map.insert var i')
  return $ MIL.Var (varStr ++ "_" ++ show i')

currentVar :: Var -> CodeGenM MIL.Var
currentVar var@(Var varStr) = do
  varMap <- snd <$> get
  return $ case Map.lookup var varMap of
             Just i -> MIL.Var (varStr ++ "_" ++ show i)
             Nothing -> varMil var

getTypeEnv :: (TypeEnv, ClassTypes) -> TypeEnv
getTypeEnv = fst

getClassTypes :: (TypeEnv, ClassTypes) -> ClassTypes
getClassTypes = snd

-- * Built-ins

pureSrcMonadMil :: MIL.SrcType
pureSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType)
    (MIL.mkSimpleSrcType "NonTerm")

impureSrcMonadMil :: MIL.SrcType
impureSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
    MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "NonTerm") $
      MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State")
        (MIL.mkSimpleSrcType "IO")

impureSrcMonadMilWithStateBase :: MIL.SrcType
impureSrcMonadMilWithStateBase =
  MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
    MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "NonTerm")
      (MIL.mkSimpleSrcType "State")

exceptionSrcType :: MIL.SrcType
exceptionSrcType = MIL.mkSimpleSrcType "Unit"

builtInMilTypeDefs :: [MIL.SrcTypeDef]
builtInMilTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkSimpleSrcType "A"]]
  , MIL.TypeDef (MIL.TypeName "String") []
      [ MIL.ConDef (MIL.ConName "Empty_Str") []
      , MIL.ConDef (MIL.ConName "Cons_Str") [MIL.mkSimpleSrcType "Char", MIL.mkSimpleSrcType "String"]]
  ]

builtInMilFunTypes :: [(MIL.FunName, MIL.SrcType)]
builtInMilFunTypes =
  [ (MIL.FunName "printString", MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readString",  MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "String"))
  , (MIL.FunName "printBool",   MIL.SrcTyArrow (MIL.mkSimpleSrcType "Bool")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readBool",    MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Bool"))
  , (MIL.FunName "printInt",    MIL.SrcTyArrow (MIL.mkSimpleSrcType "Int")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readInt",     MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Int"))
  , (MIL.FunName "printFloat",  MIL.SrcTyArrow (MIL.mkSimpleSrcType "Float")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readFloat",   MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Float"))
  ]

-- | Unsafe. Make sure that there exists such a built-in function.
getMilBuiltInFunType :: MIL.FunName -> MIL.SrcType
getMilBuiltInFunType milFunName = fromJust $ lookup milFunName builtInMilFunTypes

builtInMilFunDefs :: [MIL.SrcFunDef]
builtInMilFunDefs =
  [ printStringMilDef
  ]
  ++ readStringMilDef
  ++
  [ printBoolMilDef
  , readBoolMilDef
  , printIntMilDef
  , readIntMilDef
  , printFloatMilDef
  , readFloatMilDef
  ]

printStringMilDef :: MIL.SrcFunDef
printStringMilDef =
  MIL.mkSrcFunDef "printString" (getMilBuiltInFunType $ MIL.FunName "printString") $
    MIL.mkSrcLambda (MIL.Var "s_") (MIL.mkSimpleSrcType "String") $
      MIL.CaseE (MIL.mkSrcVar "s_")
        [ MIL.CaseAlt (MIL.ConP (MIL.ConName "Empty_Str") [],
            MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))
        , MIL.CaseAlt (MIL.ConP (MIL.ConName "Cons_Str")
              [ MIL.VarBinder (MIL.Var "c_", MIL.mkSimpleSrcType "Char")
              , MIL.VarBinder (MIL.Var "cs_", MIL.mkSimpleSrcType "String")],
            MIL.mkSrcLet (MIL.Var "unit_0") (MIL.mkSimpleSrcType "Unit")
              (MIL.LiftE (MIL.AppE (MIL.mkSrcVar "print_char") (MIL.mkSrcVar "c_"))
                 (MIL.mkSimpleSrcType "IO")
                 impureSrcMonadMil)
              (MIL.AppE (MIL.mkSrcVar "printString") (MIL.mkSrcVar "cs_")))]

readStringMilDef :: [MIL.SrcFunDef]
readStringMilDef =
  [ MIL.mkSrcFunDef "readString" (getMilBuiltInFunType $ MIL.FunName "readString") $
      MIL.AppE (MIL.mkSrcVar "readString_") (MIL.mkSrcConName "Empty_Str")
  , MIL.mkSrcFunDef "readString_" (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                                  (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "String"))) $
      MIL.mkSrcLambda (MIL.Var "acc_") (MIL.mkSimpleSrcType "String") $
        MIL.mkSrcLet (MIL.Var "c_") (MIL.mkSimpleSrcType "Char")
          (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") impureSrcMonadMil)
          (MIL.CaseE (MIL.mkSrcVar "c_")
             [ MIL.CaseAlt (MIL.LitP $ MIL.CharLit ' ',
                 MIL.ReturnE impureSrcMonadMil (MIL.AppE (MIL.AppE (MIL.mkSrcVar "reverseString_") (MIL.mkSrcVar "acc_")) (MIL.mkSrcConName "Empty_Str")))
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

printBoolMilDef :: MIL.SrcFunDef
printBoolMilDef =
  MIL.mkSrcFunDef "printBool" (getMilBuiltInFunType $ MIL.FunName "printBool") $
    MIL.mkSrcLambda (MIL.Var "b_") (MIL.mkSimpleSrcType "Bool") $
      MIL.CaseE (MIL.mkSrcVar "b_")
        [ MIL.CaseAlt (MIL.ConP (MIL.ConName "True") [],
            MIL.AppE (MIL.mkSrcVar "printString") (stringMil "true"))
        , MIL.CaseAlt (MIL.ConP (MIL.ConName "False") [],
            MIL.AppE (MIL.mkSrcVar "printString") (stringMil "false"))
        ]

readBoolMilDef :: MIL.SrcFunDef
readBoolMilDef =
  MIL.mkSrcFunDef "readBool" (getMilBuiltInFunType $ MIL.FunName "readBool") $
    MIL.mkSrcLet (MIL.Var "c_1") (MIL.mkSimpleSrcType "Char")
      (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") impureSrcMonadMil)
      (MIL.CaseE (MIL.mkSrcVar "c_1")
         [ readBoolCaseAlt "true" "True" 2
         , readBoolCaseAlt "false" "False" 2
         , readBoolErrorCaseAlt
         ])

readBoolCaseAlt :: String -> String -> Int -> MIL.SrcCaseAlt
readBoolCaseAlt "" conNameStr _ =
  MIL.CaseAlt (MIL.LitP $ MIL.CharLit ' ',
    MIL.ReturnE impureSrcMonadMil (MIL.mkSrcConName conNameStr))
readBoolCaseAlt (c:cs) conNameStr i =
  MIL.CaseAlt (MIL.LitP $ MIL.CharLit c,
    MIL.mkSrcLet (MIL.Var $ "c_" ++ show i) (MIL.mkSimpleSrcType "Char")
      (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") impureSrcMonadMil)
      (MIL.CaseE (MIL.mkSrcVar $ "c_" ++ show i)
         [ readBoolCaseAlt cs conNameStr (i+1)
         , readBoolErrorCaseAlt
         ]))

readBoolErrorCaseAlt :: MIL.SrcCaseAlt
readBoolErrorCaseAlt =
  MIL.CaseAlt (MIL.DefaultP,
    MIL.mkSrcLet (MIL.Var "errRes_") (MIL.mkSimpleSrcType "Bool")
      (MIL.AppE (MIL.TypeAppE (MIL.TypeAppE (MIL.mkSrcVar "throw_error") (MIL.mkSimpleSrcType "Unit")) (MIL.mkSimpleSrcType "Bool"))
                (MIL.LitE MIL.UnitLit))
      (MIL.ReturnE impureSrcMonadMil (MIL.mkSrcVar "errRes_")))

printIntMilDef :: MIL.SrcFunDef
printIntMilDef =
  MIL.mkSrcFunDef "printInt" (getMilBuiltInFunType $ MIL.FunName "printInt")
    (MIL.mkSrcLambda (MIL.Var "i_") (MIL.mkSimpleSrcType "Int") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readIntMilDef :: MIL.SrcFunDef
readIntMilDef =
  MIL.mkSrcFunDef "readInt" (getMilBuiltInFunType $ MIL.FunName "readInt")
    (MIL.ReturnE impureSrcMonadMil (MIL.LitE $ MIL.IntLit 1))

printFloatMilDef :: MIL.SrcFunDef
printFloatMilDef =
  MIL.mkSrcFunDef "printFloat" (getMilBuiltInFunType $ MIL.FunName "printFloat")
    (MIL.mkSrcLambda (MIL.Var "f_") (MIL.mkSimpleSrcType "Float") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readFloatMilDef :: MIL.SrcFunDef
readFloatMilDef =
  MIL.mkSrcFunDef "readFloat" (getMilBuiltInFunType $ MIL.FunName "readFloat")
    (MIL.ReturnE impureSrcMonadMil (MIL.LitE $ MIL.FloatLit 1.0))

stringMil :: String -> MIL.SrcExpr
stringMil "" = MIL.mkSrcConName "Empty_Str"
stringMil (c:cs) =
  MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str")
                     (MIL.mkCharLit c))
           (stringMil cs)

