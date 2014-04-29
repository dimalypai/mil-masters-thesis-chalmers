{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
module FunLang.CodeGenMil
  ( codeGen
  ) where

import FunLang.AST
import FunLang.AST.Helpers
import qualified MIL.AST as MIL

import Data.List (foldl')

-- | Entry point to the code generator.
-- Takes a type checked program in FunLang and produces a program in MIL.
codeGen :: TyProgram -> MIL.Program
codeGen = codeGenProgram

codeGenProgram :: TyProgram -> MIL.Program
codeGenProgram (Program _ srcTypeDefs tyFunDefs) =
  let milTypeDefs = map codeGenTypeDef srcTypeDefs
      milFunDefs = map codeGenFunDef tyFunDefs
  in MIL.Program (milTypeDefs, milFunDefs)

codeGenTypeDef :: SrcTypeDef -> MIL.TypeDef
codeGenTypeDef (TypeDef _ srcTypeName srcTypeVars srcConDefs) =
  let milTypeVars = map (typeVarMil . getTypeVar) srcTypeVars
      milConDefs = map codeGenConDef srcConDefs
  in MIL.TypeDef (typeNameMil $ getTypeName srcTypeName) milTypeVars milConDefs

codeGenConDef :: SrcConDef -> MIL.ConDef
codeGenConDef (ConDef _ srcConName srcConFields) =
  let milConFields = map srcTypeToMilType srcConFields
  in MIL.ConDef (conNameMil $ getConName srcConName) milConFields

codeGenFunDef :: TyFunDef -> MIL.FunDef
codeGenFunDef (FunDef _ srcFunName funSrcType tyFunEqs) =
  let funName = getFunName srcFunName
      milFunType = srcTypeToMilType funSrcType
      milFunBody = codeGenFunEqs tyFunEqs
  in MIL.FunDef (funNameMil funName) milFunType milFunBody

-- | TODO
codeGenFunEqs :: [TyFunEq] -> MIL.Expr
codeGenFunEqs tyFunEqs =
  let funEqExprs = map (codeGenExpr . getFunEqBody) tyFunEqs
  in head funEqExprs  -- TODO

codeGenExpr :: TyExpr -> MIL.Expr
codeGenExpr (LitE srcLit) = MIL.LitE (literalMil $ getLiteral srcLit)
codeGenExpr (VarE _ varTy) = MIL.VarE $ MIL.VarBinder (var, milVarType)
  where var = varMil $ getVarTyVar varTy
        milVarType = typeMil $ getVarTyType varTy
-- TODO
codeGenExpr (DoE _ [ReturnS _ tyExpr]) =
  MIL.ReturnE undefined (codeGenExpr tyExpr)

literalMil :: Literal -> MIL.Literal
literalMil UnitLit        = MIL.UnitLit
literalMil (IntLit i)     = MIL.IntLit i
literalMil (FloatLit f _) = MIL.FloatLit f
literalMil (StringLit s)  = MIL.StringLit s

typeMil :: Type -> MIL.Type
typeMil (TyVar typeVar) = MIL.TyVar $ typeVarMil typeVar
typeMil (TyArrow t1 t2) = MIL.TyArrow (typeMil t1) (typeMil t2)
typeMil (TyApp typeName typeArgs) =
  foldl' (\mt t -> MIL.TyApp mt (typeMil t)) (MIL.TyTypeCon $ typeNameMil typeName) typeArgs
typeMil (TyForAll typeVar t) = MIL.TyForAll (typeVarMil typeVar) (typeMil t)

-- | TODO
monadMil :: SrcType -> MIL.TypeM
monadMil (SrcTyCon srcTypeName) = MIL.MTyMonad MIL.IO  -- TODO

srcTypeToMilType :: SrcType -> MIL.Type
srcTypeToMilType (SrcTyCon srcTypeName) =
  MIL.TyTypeCon $ typeNameMil (getTypeName srcTypeName)
srcTypeToMilType (SrcTyApp _ st1 st2) =
  MIL.TyApp (srcTypeToMilType st1) (srcTypeToMilType st2)
srcTypeToMilType (SrcTyArrow _ st1 st2) =
  MIL.TyArrow (srcTypeToMilType st1) (srcTypeToMilType st2)
srcTypeToMilType (SrcTyForAll _ stv st) =
  MIL.TyForAll (typeVarMil $ getTypeVar stv) (srcTypeToMilType st)
srcTypeToMilType (SrcTyParen _ st) = srcTypeToMilType st

-- Conversion utils

typeNameMil :: TypeName -> MIL.TypeName
typeNameMil (TypeName typeNameStr) = MIL.TypeName typeNameStr

conNameMil :: ConName -> MIL.ConName
conNameMil (ConName conNameStr) = MIL.ConName conNameStr

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

typeVarMil :: TypeVar -> MIL.TypeVar
typeVarMil (TypeVar typeVarStr) = MIL.TypeVar typeVarStr

