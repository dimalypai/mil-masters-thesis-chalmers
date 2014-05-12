{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
module OOLang.CodeGenMil
  ( codeGen
  ) where

import Control.Monad.Reader
import Control.Applicative

import OOLang.AST
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
codeGen tyProgram typeEnv = runReader (runCG $ codeGenProgram tyProgram) typeEnv

-- | Code generation monad. Uses 'Reader' for querying the type environment.
newtype CodeGenM a = CG { runCG :: Reader TypeEnv a }
  deriving (Monad, MonadReader TypeEnv, Functor, Applicative)

-- | Entry point into the type checking of the program.
-- There is an MIL program generated for each class definition.
-- There is an MIL function generated for each function definition.
-- All these definitions are then regrouped and the built-ins are added.
codeGenProgram :: TyProgram -> CodeGenM MIL.Program
codeGenProgram (Program _ tyClassDefs tyFunDefs) = do
  classMilPrograms <- mapM codeGenClassDef tyClassDefs
  milFunDefs <- mapM codeGenFunDef tyFunDefs
  let classMilTypeDefs = concatMap MIL.getMilTypeDefs classMilPrograms
      classMilFunDefs = concatMap MIL.getMilFunDefs classMilPrograms
  return $ MIL.Program (builtInTypeDefs ++ classMilTypeDefs, classMilFunDefs ++ milFunDefs)

-- | Each class definition gets its own data type representing data (fields)
-- and a function definition for each method defined in this class.
codeGenClassDef :: TyClassDef -> CodeGenM MIL.Program
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) = do
  let className = getClassName srcClassName
  let (tyFieldDecls, tyMethodDecls) = partitionClassMembers tyMembers
      superClassField =
        case mSuperSrcClassName of
          Nothing -> []
          Just superSrcClassName -> [MIL.TyTypeCon (typeNameMil className)]
  classFields <- mapM (codeGenClassField className) tyFieldDecls
  let classConDef = MIL.ConDef (conNameMil className) (superClassField ++ classFields)
      classTypeDef = MIL.TypeDef (typeNameMil className) [] [classConDef]
  classMemberFunDefs <- mapM codeGenClassMethod tyMethodDecls
  return $ MIL.Program ([classTypeDef], classMemberFunDefs)

-- | TODO: inits (together with constructor).
codeGenClassField :: ClassName -> TyFieldDecl -> CodeGenM MIL.Type
codeGenClassField className (FieldDecl _ tyDecl _) = do
  let fieldName = getVar $ getDeclVarName tyDecl
  fieldType <- asks (getClassFieldType className fieldName . getClassTypeEnv)
  return $ typeMil fieldType

-- | TODO: add method specifics.
codeGenClassMethod :: TyMethodDecl -> CodeGenM MIL.FunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> CodeGenM MIL.FunDef
codeGenFunDef (FunDef _ srcFunName _ tyStmts) = do
  let funName = getFunName srcFunName
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv)
  let isPure = isPureFunType funType
  let funBody = codeGenStmts tyStmts isPure
  let monadType = if isPure
                    then MIL.TyMonad idMonad
                    else MIL.TyMonad allEffectsMonad
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
              else MIL.ReturnE allEffectsMonad
                               (MIL.LitE MIL.UnitLit))

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
      if isPure
        then (MIL.ReturnE idMonad milExpr, undefined)
        else (milExpr, undefined)

    -- TODO:
    -- + "then" with state putting operations
    -- + ANF/SSA construction
    AssignS _ srcAssignOp tyExprLeft tyExprRight -> undefined

-- | Expression code generation.
-- Takes a purity indicator.
codeGenExpr :: TyExpr -> Bool -> MIL.Expr
codeGenExpr tyExpr isPure =
  case tyExpr of
    LitE lit -> literalMil lit

    VarE _ varTy ->
      let var = varMil $ getVarTyVar varTy
          milVarType = typeMil $ getVarTyType varTy
      in (MIL.VarE $ MIL.VarBinder (var, milVarType))

    BinOpE _ srcBinOp tyExpr1 tyExpr2 ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 isPure

    ParenE _ tySubExpr -> codeGenExpr tySubExpr isPure

literalMil :: Literal s -> MIL.Expr
literalMil UnitLit {} = MIL.LitE MIL.UnitLit
literalMil (BoolLit _ b) =
  if b
    then MIL.ConNameE (MIL.ConName "True")  (MIL.mkSimpleType "Bool")
    else MIL.ConNameE (MIL.ConName "False") (MIL.mkSimpleType "Bool")
literalMil (IntLit _ i)     = MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ f _) = MIL.LitE (MIL.FloatLit f)
literalMil (StringLit _ s)  = MIL.LitE (MIL.StringLit s)
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

idMonad :: MIL.TypeM
idMonad = MIL.MTyMonad MIL.Id

allEffectsMonad :: MIL.TypeM
allEffectsMonad =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonadCons MIL.State $
      MIL.MTyMonadCons MIL.Lift $
        MIL.MTyMonad MIL.IO

-- TODO: should be a built-in Exception class type.
exceptionType :: MIL.Type
exceptionType = MIL.unitType

