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
import qualified MIL.AST as MIL

-- | Entry point to the code generator.
-- Takes a type checked program in OOLang and a type environment and produces a
-- program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.Program
codeGen tyProgram typeEnv = runReader (runCG $ codeGenProgram tyProgram) typeEnv

-- | Code generation monad. Uses 'Reader' for querying the type environment.
newtype CodeGenM a = CG { runCG :: Reader TypeEnv a }
  deriving (Monad, MonadReader TypeEnv, Functor, Applicative)

-- | TODO
codeGenProgram :: TyProgram -> CodeGenM MIL.Program
codeGenProgram (Program _ tyClassDefs tyFunDefs) = do
  classMilPrograms <- mapM codeGenClassDef tyClassDefs
  milFunDefs <- mapM codeGenFunDef tyFunDefs
  let classMilTypeDefs = concatMap MIL.getMilTypeDefs classMilPrograms
      classMilFunDefs = concatMap MIL.getMilFunDefs classMilPrograms
  return $ MIL.Program (builtInTypeDefs ++ classMilTypeDefs, classMilFunDefs ++ milFunDefs)

-- | TODO: outline the code gen idea
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

-- | TODO: inits (together with constructor)
codeGenClassField :: ClassName -> TyFieldDecl -> CodeGenM MIL.Type
codeGenClassField className (FieldDecl _ tyDecl _) = do
  let fieldName = getVar $ getDeclVarName tyDecl
  fieldType <- asks (getClassFieldType className fieldName . getClassTypeEnv)
  return $ typeMil fieldType

-- | TODO: ?
codeGenClassMethod :: TyMethodDecl -> CodeGenM MIL.FunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> CodeGenM MIL.FunDef
codeGenFunDef (FunDef _ srcFunName _ tyStmts) = do
  let funName = getFunName srcFunName
  let funBody = codeGenStmts tyStmts
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv)
  return $ MIL.FunDef (funNameMil funName) (typeMil funType) funBody

-- | List of statements is not empty.
codeGenStmts :: [TyStmt] -> MIL.Expr
codeGenStmts [ExprS _ tyExpr] = codeGenExpr tyExpr
codeGenStmts [tyStmt]         = MIL.LitE MIL.UnitLit

codeGenExpr :: TyExpr -> MIL.Expr
codeGenExpr tyExpr =
  case tyExpr of
    LitE lit -> literalMil lit

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

-- * Type conversions

-- | Internal type representation transformation.
typeMil :: Type -> MIL.Type
typeMil TyUnit   = MIL.mkSimpleType "Unit"
typeMil TyBool   = MIL.mkSimpleType "Bool"
typeMil TyInt    = MIL.mkSimpleType "Int"
typeMil TyFloat  = MIL.mkSimpleType "Float"
typeMil TyString = MIL.mkSimpleType "String"
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

-- * Built-ins

builtInTypeDefs :: [MIL.TypeDef]
builtInTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Bool") [] [ MIL.ConDef (MIL.ConName "True")  []
                                         , MIL.ConDef (MIL.ConName "False") []]
  , MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkTypeVar "A"]]
  ]

