-- | Module responsible for MIL code generation.
module OOLang.CodeGenMil
  ( codeGen
  ) where

import OOLang.AST
import OOLang.AST.Helpers
import qualified MIL.AST as MIL

-- | Entry point to the code generator.
-- Takes a type checked program in OOLang and produces a program in MIL.
codeGen :: TyProgram -> MIL.Program
codeGen = codeGenProgram

-- | TODO: generate code for built-ins
codeGenProgram :: TyProgram -> MIL.Program
codeGenProgram (Program _ tyClassDefs tyFunDefs) =
  let classMilPrograms = map codeGenClassDef tyClassDefs
      classMilTypeDefs = concatMap MIL.getMilTypeDefs classMilPrograms
      classMilFunDefs = concatMap MIL.getMilFunDefs classMilPrograms
      milFunDefs = classMilFunDefs ++ map codeGenFunDef tyFunDefs
  in MIL.Program (builtInTypeDefs ++ classMilTypeDefs, milFunDefs)

-- | TODO: outline the code gen idea
codeGenClassDef :: TyClassDef -> MIL.Program
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) =
  let (tyFieldDecls, tyMethodDecls) = partitionClassMembers tyMembers
      superClassField =
        case mSuperSrcClassName of
          Nothing -> []
          Just superSrcClassName -> [MIL.TyTypeCon (typeNameMil $ getClassName superSrcClassName)]
      classFields = map codeGenClassField tyFieldDecls
      classConDef = MIL.ConDef (conNameMil $ getClassName srcClassName) (superClassField ++ classFields)
      classTypeDef = MIL.TypeDef (typeNameMil $ getClassName srcClassName) [] [classConDef]
      classMemberFunDefs = map codeGenClassMethod tyMethodDecls
  in MIL.Program ([classTypeDef], classMemberFunDefs)

-- | TODO: inits (together with constructor)
codeGenClassField :: TyFieldDecl -> MIL.Type
codeGenClassField (FieldDecl _ tyDecl _) = srcTypeToMilType (getDeclVarType tyDecl)

-- | TODO: ?
codeGenClassMethod :: TyMethodDecl -> MIL.FunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> MIL.FunDef
codeGenFunDef (FunDef _ srcFunName srcFunType tyStmts) =
  let funBody = codeGenStmts tyStmts
      funType = srcFunTypeToMilType srcFunType
  in MIL.FunDef (funNameMil $ getFunName srcFunName) funType funBody

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

effects :: MIL.TypeM
effects = undefined

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
  [ MIL.TypeDef (MIL.TypeName "Bool") [] [ MIL.ConDef (MIL.ConName "True") []
                                         , MIL.ConDef (MIL.ConName "False") []]
  , MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just") [MIL.mkTypeVar "A"]]
  ]

