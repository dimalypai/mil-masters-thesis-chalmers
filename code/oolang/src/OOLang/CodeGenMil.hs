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
  let (tyFieldDecls, tyMethodDecls) = partitionClassMembers tyMembers
      superClassField =
        case mSuperSrcClassName of
          Nothing -> []
          Just superSrcClassName -> [MIL.TyTypeCon (typeNameMil $ getClassName superSrcClassName)]
      classFields = map codeGenClassField tyFieldDecls
      classConDef = MIL.ConDef (conNameMil $ getClassName srcClassName) (superClassField ++ classFields)
      classTypeDef = MIL.TypeDef (typeNameMil $ getClassName srcClassName) [] [classConDef]
  classMemberFunDefs <- mapM codeGenClassMethod tyMethodDecls
  return $ MIL.Program ([classTypeDef], classMemberFunDefs)

-- | TODO: inits (together with constructor)
codeGenClassField :: TyFieldDecl -> MIL.Type
codeGenClassField (FieldDecl _ tyDecl _) = srcTypeToMilType (getDeclVarType tyDecl)

-- | TODO: ?
codeGenClassMethod :: TyMethodDecl -> CodeGenM MIL.FunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> CodeGenM MIL.FunDef
codeGenFunDef (FunDef _ srcFunName srcFunType tyStmts) = do
  let funBody = codeGenStmts tyStmts
      funType = srcFunTypeToMilType srcFunType
  return $ MIL.FunDef (funNameMil $ getFunName srcFunName) funType funBody

-- | List of statements is not empty.
codeGenStmts :: [TyStmt] -> MIL.Expr
codeGenStmts _ = MIL.LitE (MIL.UnitLit)  -- TODO

-- * Type conversions

srcFunTypeToMilType :: SrcFunType -> MIL.Type
srcFunTypeToMilType (FunType _ varBinders retSrcType) =
  let retType = srcTypeToMilType retSrcType
  in retType  -- TODO

srcTypeToMilType :: SrcType -> MIL.Type
srcTypeToMilType (SrcTyUnit  _) = MIL.mkSimpleType "Unit"
srcTypeToMilType (SrcTyBool  _) = MIL.mkSimpleType "Bool"
srcTypeToMilType (SrcTyInt   _) = MIL.mkSimpleType "Int"
srcTypeToMilType (SrcTyFloat _) = MIL.mkSimpleType "Float"
srcTypeToMilType (SrcTyClass srcClassName) =
  MIL.TyTypeCon $ typeNameMil (getClassName srcClassName)
srcTypeToMilType (SrcTyArrow _ st1 st2) =
  MIL.TyArrow (srcTypeToMilType st1) (srcTypeToMilType st2)
srcTypeToMilType (SrcTyPure _ st) = srcTypeToMilType st
srcTypeToMilType (SrcTyMaybe _ st) =
  MIL.TyApp (MIL.TyTypeCon $ MIL.TypeName "Maybe") (srcTypeToMilType st)
srcTypeToMilType (SrcTyMutable _ st) = srcTypeToMilType st
srcTypeToMilType (SrcTyParen _ st) = srcTypeToMilType st

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

