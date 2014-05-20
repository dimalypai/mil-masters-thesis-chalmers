{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
--
-- All generated code operates in two monads: Id for pure computations and
-- Impure_M for default (impure) computations. Impure_M is a fixed stack of
-- monads, see 'impureMonadType'.
-- Statement sequences are represented with monadic binds.
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
import OOLang.Utils
import qualified MIL.AST as MIL
import qualified MIL.BuiltIn as MIL
import qualified MIL.Transformations.IdExprMonadElimination as MIL

-- | Entry point to the code generator.
-- Takes a type checked program in OOLang and a type environment and produces a
-- program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.Program
codeGen tyProgram typeEnv = runReaderFrom (typeEnv, Map.empty) $ evalStateTFrom 0 (runCG $ codeGenProgram tyProgram)

-- | Code generation monad. Uses 'Reader' for querying the type environment and
-- class types.
newtype CodeGenM a = CG { runCG :: StateT NameSupply (Reader (TypeEnv, ClassTypes)) a }
  deriving (Monad, MonadState NameSupply, MonadReader (TypeEnv, ClassTypes), Functor, Applicative)

-- | All field and method names accessible from the class and a MIL type of the
-- object representation.
type ClassTypes = Map.Map ClassName ([Var], [FunName], MIL.Type)

-- | A counter for generating unique variable names.
type NameSupply = Int

-- | Entry point into the type checking of the program.
-- There is a list of MIL functions generated for each class definition.
-- TODO: type aliases
-- There is an MIL function generated for each function definition.
-- All these definitions are then regrouped and the built-ins are added.
codeGenProgram :: TyProgram -> CodeGenM MIL.Program
codeGenProgram (Program _ tyClassDefs tyFunDefs) = do
  classTypes <- asks snd
  classTypes' <- foldM (\cts (ClassDef _ srcClassName _ _) ->
    collectClassTypes (getClassName srcClassName) cts)
    classTypes tyClassDefs
  local (second $ const classTypes') $ do
    classMilFunDefs <- concat <$> mapM codeGenClassDef tyClassDefs
    milFunDefs <- mapM codeGenFunDef tyFunDefs
    return $ MIL.Program (builtInTypeDefs, builtInAliasDefs, classMilFunDefs ++ milFunDefs)

-- | Collects information about the class: field and methods names. Constructs
-- a type of the object representation.
collectClassTypes :: ClassName -> ClassTypes -> CodeGenM ClassTypes
collectClassTypes className classTypes = do
  classTypeEnv <- asks (getClassTypeEnv . fst)
  let fields = getClassFieldsAssoc className classTypeEnv
      methods = getClassMethodsAssoc className classTypeEnv
      (fieldNames, fieldTypes) = unzip fields
      (methodNames, methodTypes) = unzip methods
      -- TODO: virtual methods and self parameters
      classTupleType = MIL.TyTuple [ MIL.TyTuple $ map typeMil fieldTypes
                                   , MIL.TyTuple $ map typeMil methodTypes]
  return $ Map.insert className (fieldNames, methodNames, classTupleType) classTypes

codeGenClassDef :: TyClassDef -> CodeGenM [MIL.FunDef]
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) = do
  let (_, tyMethodDecls) = partitionClassMembers tyMembers
  classMemberFunDefs <- mapM codeGenClassMethod tyMethodDecls
  return $ classMemberFunDefs

-- | TODO: inits (together with constructor).
codeGenClassField :: ClassName -> TyFieldDecl -> CodeGenM MIL.Type
codeGenClassField className (FieldDecl _ tyDecl _) = do
  let fieldName = getVar $ getDeclVarName tyDecl
  fieldType <- asks (getClassFieldType className fieldName . getClassTypeEnv . fst)
  return $ typeMil fieldType

-- | TODO: add method specifics.
codeGenClassMethod :: TyMethodDecl -> CodeGenM MIL.FunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> CodeGenM MIL.FunDef
codeGenFunDef (FunDef _ srcFunName _ tyStmts) = do
  let funName = getFunName srcFunName
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv . fst)
  let isPure = isPureFunType funType
  let (funMonad, milFunType) = if isPure
                                 then (idMonad, typeMil funType)
                                 else (impureMonad, MIL.monadReturnType impureMonad (typeMil funType))
  funBody <- codeGenStmts tyStmts funMonad
  funParams <- asks (ftiParams . getFunTypeInfo funName . getFunTypeEnv . fst)
  let funBodyWithParams = foldr (\(p, t) e -> MIL.LambdaE (MIL.VarBinder (varMil p, typeMil t)) e)
                            funBody funParams
  return $ MIL.FunDef (funNameMil funName) milFunType funBodyWithParams

-- | List of statements is not empty.
-- Takes a monad of the containing function.
codeGenStmts :: [TyStmt] -> MIL.TypeM -> CodeGenM MIL.Expr

codeGenStmts [tyStmt@(ExprS {})] funMonad = codeGenStmt tyStmt funMonad

codeGenStmts [tyStmt] funMonad = do
  milExpr <- codeGenStmt tyStmt funMonad
  let milExprType = typeMil $ getTypeOf tyStmt
  return $ MIL.LetE (MIL.VarBinder (MIL.Var "_", milExprType))
             milExpr
             (MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit))

codeGenStmts (tyStmt:tyStmts) funMonad = do
  milBindExpr <- codeGenStmt tyStmt funMonad
  let milBindExprType = typeMil $ getTypeOf tyStmt
  milBodyExpr <- codeGenStmts tyStmts funMonad
  return $ MIL.LetE (MIL.VarBinder (MIL.Var "_", milBindExprType))
             milBindExpr
             milBodyExpr

-- | Takes a monad of the containing function.
codeGenStmt :: TyStmt -> MIL.TypeM -> CodeGenM MIL.Expr
codeGenStmt tyStmt funMonad =
  case tyStmt of
    -- TODO: It will be a bind introducing new variable
    DeclS _ tyDecl -> undefined

    ExprS _ tyExpr -> codeGenExpr tyExpr funMonad

    -- TODO:
    -- + "then" with state putting operations
    -- + ANF/SSA construction
    AssignS _ _ srcAssignOp tyExprLeft tyExprRight _ -> undefined

-- | Expression code generation.
-- Takes a monad of the containing function.
codeGenExpr :: TyExpr -> MIL.TypeM -> CodeGenM MIL.Expr
codeGenExpr tyExpr funMonad = do
  milExpr <-
    case tyExpr of
      LitE lit -> return $ MIL.ReturnE idMonad (literalMil lit)

      VarE _ varType var ->
        let milVarType = case (isPureType varType, funMonad == idMonad) of
                           (False, False) -> MIL.monadReturnType impureMonad (typeMil varType)
                           (_, _) -> typeMil varType
        in return $ MIL.VarE $ MIL.VarBinder (varMil var, milVarType)

      BinOpE _ _ srcBinOp tyExpr1 tyExpr2 ->
        codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 funMonad

      ParenE _ tySubExpr -> codeGenExpr tySubExpr funMonad
  let exprType = getTypeOf tyExpr
      milExprType = if not $ isPureType exprType
                      then MIL.TyApp (MIL.TyMonad funMonad) (typeMil exprType)
                      else typeMil exprType
  exprVar <- newMilVar
  return $
    MIL.LetE (MIL.VarBinder (exprVar, MIL.getMonadResultType milExprType))
      (case (isPureType exprType, funMonad == idMonad) of
         (True, False) -> MIL.LiftE milExpr idMonad funMonad
         (_, _) -> milExpr)
      (MIL.ReturnE funMonad (MIL.VarE $ MIL.VarBinder (exprVar, MIL.getMonadResultType milExprType)))

literalMil :: Literal t s -> MIL.Expr
literalMil UnitLit {} = MIL.LitE MIL.UnitLit
literalMil (BoolLit _ _ b) =
  if b
    then MIL.ConNameE (MIL.ConName "True")  (MIL.mkSimpleType "Bool")
    else MIL.ConNameE (MIL.ConName "False") (MIL.mkSimpleType "Bool")
literalMil (IntLit _ _ i)     = MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ _ f _) = MIL.LitE (MIL.FloatLit f)
literalMil (StringLit _ _ s)  = MIL.LitE (MIL.StringLit s)
literalMil NothingLit {} =
  MIL.ConNameE (MIL.ConName "Nothing")
    (MIL.TyForAll (MIL.TypeVar "A") $
       MIL.TyApp (MIL.TyTypeCon $ MIL.TypeName "Maybe") (MIL.mkTypeVar "A"))

-- | Takes a monad of the containing function.
codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> MIL.TypeM -> CodeGenM MIL.Expr
codeGenBinOp App tyExpr1 tyExpr2 funMonad =
  MIL.AppE <$> codeGenExpr tyExpr1 funMonad <*> codeGenExpr tyExpr2 funMonad

-- * Type conversions

-- | Internal type representation transformation.
typeMil :: Type -> MIL.Type
typeMil TyUnit   = MIL.unitType
typeMil TyBool   = MIL.mkSimpleType "Bool"
typeMil TyInt    = MIL.intType
typeMil TyFloat  = MIL.floatType
typeMil TyString = MIL.stringType
typeMil (TyClass className) = MIL.TyTypeCon $ typeNameMil className
typeMil (TyArrow t1 t2)     = MIL.TyArrow (typeMil t1) (typeMil t2)
typeMil (TyPure t)          = MIL.TyApp (MIL.TyMonad idMonad) (typeMil t)
typeMil (TyMaybe t)         = MIL.TyApp (MIL.TyTypeCon $ MIL.TypeName "Maybe") (typeMil t)
typeMil (TyMutable t)       = typeMil t

-- * Conversion utils

typeNameMil :: ClassName -> MIL.TypeName
typeNameMil (ClassName classNameStr) = MIL.TypeName classNameStr

conNameMil :: ClassName -> MIL.ConName
conNameMil (ClassName classNameStr) = MIL.ConName classNameStr

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

-- * Built-ins

builtInTypeDefs :: [MIL.TypeDef]
builtInTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Bool") [] [ MIL.ConDef (MIL.ConName "True")  []
                                         , MIL.ConDef (MIL.ConName "False") []]
  , MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkTypeVar "A"]]
  ]

builtInAliasDefs :: [MIL.AliasDef]
builtInAliasDefs =
  [MIL.AliasDef impureMonadName $ MIL.TyMonad impureMonadType]

-- * Monads

idMonad :: MIL.TypeM
idMonad = MIL.MTyMonad MIL.Id

impureMonadName :: MIL.TypeName
impureMonadName = MIL.TypeName "Impure_M"

impureMonad :: MIL.TypeM
impureMonad = MIL.MTyAlias impureMonadName

impureMonadType :: MIL.TypeM
impureMonadType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonadCons MIL.State $
      MIL.MTyMonadCons MIL.Lift $
        MIL.MTyMonadCons MIL.IO $
          MIL.MTyMonad MIL.Id

-- TODO: should be a built-in Exception class type.
exceptionType :: MIL.Type
exceptionType = MIL.unitType

-- * CodeGenM operations

newMilVar :: CodeGenM MIL.Var
newMilVar = do
  i <- get
  modify (+1)
  return $ MIL.Var ("var_" ++ show i)

