{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
module OOLang.CodeGenMil
  ( codeGen
  ) where

import Control.Monad.Reader
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
codeGen tyProgram typeEnv = runReader (runCG $ codeGenProgram tyProgram) (typeEnv, Map.empty)

-- | Code generation monad. Uses 'Reader' for querying the type environment and
-- class types.
newtype CodeGenM a = CG { runCG :: Reader (TypeEnv, ClassTypes) a }
  deriving (Monad, MonadReader (TypeEnv, ClassTypes), Functor, Applicative)

-- | All field and method names accessible from the class and a MIL type of the
-- object representation.
type ClassTypes = Map.Map ClassName ([Var], [FunName], MIL.Type)

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
  let isPure = isPureType funType
  let funBody = codeGenStmts tyStmts isPure
  let monadType = if isPure
                    then MIL.TyMonad idMonad
                    else MIL.TyMonad impureMonad
  -- TODO: only return type
  return $ MIL.FunDef (funNameMil funName) (MIL.TyApp monadType (typeMil funType)) funBody

-- | List of statements is not empty.
-- Takes a purity indicator.
-- TODO: should purity indicator be only global or annotate every statement and
-- even expression?
-- TODO: where and what do we return and in which monad?
codeGenStmts :: [TyStmt] -> Bool -> MIL.Expr

codeGenStmts [tyStmt@(ExprS {})] isPure = fst $ codeGenStmt tyStmt isPure

codeGenStmts [tyStmt] isPure =
  let (milExpr, milExprType) = codeGenStmt tyStmt isPure in
  MIL.LetE (MIL.VarBinder (MIL.Var "_", milExprType))  -- TODO: get monad result type
           milExpr
           (if isPure
              then MIL.ReturnE idMonad (MIL.LitE MIL.UnitLit)
              else MIL.ReturnE impureMonad (MIL.LitE MIL.UnitLit))

codeGenStmts (tyStmt:tyStmts) isPure =
  let (milExpr, milExprType) = codeGenStmt tyStmt isPure in
  MIL.LetE (MIL.VarBinder (MIL.Var "_", milExprType))  -- TODO: get monad result type
           milExpr
           (codeGenStmts tyStmts isPure)

codeGenStmt :: TyStmt -> Bool -> (MIL.Expr, MIL.Type)
codeGenStmt tyStmt isPure =
  case tyStmt of
    -- TODO: It will be a bind introducing new variable
    DeclS _ tyDecl -> undefined

    ExprS _ tyExpr ->
      let milExpr = codeGenExpr tyExpr isPure in
      if isPure  -- TODO
        then (MIL.ReturnE idMonad milExpr, MIL.unitType)
        else (MIL.ReturnE impureMonad milExpr, MIL.unitType)

    -- TODO:
    -- + "then" with state putting operations
    -- + ANF/SSA construction
    AssignS _ _ srcAssignOp tyExprLeft tyExprRight -> undefined

-- | Expression code generation.
-- Takes a purity indicator.
codeGenExpr :: TyExpr -> Bool -> MIL.Expr
codeGenExpr tyExpr isPure =
  case tyExpr of
    LitE lit -> literalMil lit

    VarE _ varType var ->
      MIL.VarE $ MIL.VarBinder (varMil var, typeMil varType)

    BinOpE _ _ srcBinOp tyExpr1 tyExpr2 ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 isPure

    ParenE _ tySubExpr -> codeGenExpr tySubExpr isPure

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

codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> Bool -> MIL.Expr
codeGenBinOp App tyExpr1 tyExpr2 isPure =
  MIL.AppE (codeGenExpr tyExpr1 isPure) (codeGenExpr tyExpr2 isPure)

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
typeMil (TyPure t)          = typeMil t
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

