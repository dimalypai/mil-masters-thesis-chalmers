{-# LANGUAGE ViewPatterns #-}

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
      milFunBody = codeGenFunEqs tyFunEqs milFunType
  in MIL.FunDef (funNameMil funName) milFunType milFunBody

-- | Takes function equations of the function definition and the transformed
-- function type and returns an MIL expression.
-- Type is needed for annotating monadic operations.
codeGenFunEqs :: [TyFunEq] -> MIL.Type -> MIL.Expr
codeGenFunEqs tyFunEqs milFunType =
  let funEqExprs = map (codeGenExpr milFunType . getFunEqBody) tyFunEqs
  in head funEqExprs  -- TODO

-- | Expression code generation.
-- Takes a transformed type of the containing function for annotating monadic
-- operations.
codeGenExpr :: MIL.Type -> TyExpr -> MIL.Expr
codeGenExpr milFunType srcExpr =
  case srcExpr of
    LitE srcLit -> MIL.LitE (literalMil $ getLiteral srcLit)

    VarE _ varTy ->
      let var = varMil $ getVarTyVar varTy
          milVarType = typeMil $ getVarTyType varTy
      in MIL.VarE $ MIL.VarBinder (var, milVarType)

    LambdaE _ varBinders tyBodyExpr ->
      foldr (\vb mexpr ->
          MIL.LambdaE (MIL.VarBinder ( varMil (getVar $ getBinderVar vb)
                                     , srcTypeToMilType $ getBinderType vb)) mexpr)
        (codeGenExpr milFunType tyBodyExpr)
        varBinders

    TypeLambdaE _ srcTypeVars tyBodyExpr ->
      foldr (\tv mexpr ->
          MIL.TypeLambdaE (typeVarMil tv) mexpr)
        (codeGenExpr milFunType tyBodyExpr)
        (map getTypeVar srcTypeVars)

    TypeAppE _ tyAppExpr srcArgType ->
      MIL.TypeAppE (codeGenExpr milFunType tyAppExpr) (srcTypeToMilType srcArgType)

    ConNameE srcConName -> MIL.ConNameE (conNameMil $ getConName srcConName) undefined  -- TODO

    BinOpE _ srcBinOp tyExpr1 tyExpr2 -> codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 milFunType

    ParenE _ tySubExpr -> codeGenExpr milFunType tySubExpr

    DoE _ tyStmts -> codeGenDoBlock tyStmts milFunType

literalMil :: Literal -> MIL.Literal
literalMil UnitLit        = MIL.UnitLit
literalMil (IntLit i)     = MIL.IntLit i
literalMil (FloatLit f _) = MIL.FloatLit f
literalMil (StringLit s)  = MIL.StringLit s

codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> MIL.Type -> MIL.Expr
codeGenBinOp App tyExpr1 tyExpr2 milFunType =
  MIL.AppE (codeGenExpr milFunType tyExpr1) (codeGenExpr milFunType tyExpr2)

-- | TODO
codeGenDoBlock :: [TyStmt] -> MIL.Type -> MIL.Expr
codeGenDoBlock [ExprS _ tyExpr] milFunType = codeGenExpr milFunType tyExpr
codeGenDoBlock [ReturnS _ tyExpr] milFunType@(MIL.TyApp (MIL.TyMonad monadType) _) =
  MIL.ReturnE monadType (codeGenExpr milFunType tyExpr)
codeGenDoBlock (tyStmt:tyStmts) milFunType =
  case tyStmt of
    ExprS _ tyExpr ->
      MIL.LetE (MIL.VarBinder (MIL.Var "_", MIL.TyTypeCon (MIL.TypeName "Unit")))  -- TODO
               (codeGenExpr milFunType tyExpr)
               (codeGenDoBlock tyStmts milFunType)

    BindS _ varBinder tyExpr ->
      MIL.LetE (MIL.VarBinder ( varMil (getVar $ getBinderVar varBinder)
                              , srcTypeToMilType $ getBinderType varBinder))
               (codeGenExpr milFunType tyExpr)
               (codeGenDoBlock tyStmts milFunType)

    ReturnS _ tyExpr ->
      MIL.LetE (MIL.VarBinder (MIL.Var "_", MIL.TyTypeCon (MIL.TypeName "Unit")))  -- TODO
               (codeGenExpr milFunType tyExpr)
               (codeGenDoBlock tyStmts milFunType)

-- | Internal type representation transformation.
-- The most interesting is the 'TyApp' transformation, because it deals with
-- monads. Type checker ensures that all type constructors are fully applied,
-- so it is safe to construct monadic types in this way (by direct pattern
-- matching), otherwise - we throw a panic.
typeMil :: Type -> MIL.Type
typeMil (TyVar typeVar) = MIL.TyVar $ typeVarMil typeVar
typeMil (TyArrow t1 t2) = MIL.TyArrow (typeMil t1) (typeMil t2)
typeMil (TyApp typeName typeArgs) =
  case (typeName, typeArgs) of
    (TypeName "IO", [ioResultType]) ->
      MIL.TyApp (MIL.TyMonad $ MIL.MTyMonad MIL.IO) (typeMil ioResultType)
    (TypeName "IO", _) -> error "IO type is ill-formed"

    (TypeName "State", [stateType, stateResultType]) ->
      MIL.TyApp (MIL.TyMonad $ MIL.MTyMonad (MIL.State $ typeMil stateType)) (typeMil stateResultType)
    (TypeName "State", _) -> error "State type is ill-formed"

    _ -> foldl' (\mt t -> MIL.TyApp mt (typeMil t))
           (MIL.TyTypeCon $ typeNameMil typeName)
           typeArgs
typeMil (TyForAll typeVar t) = MIL.TyForAll (typeVarMil typeVar) (typeMil t)

-- | Source type representation transformation.
-- Monadic types are transformed in different cases depending on their kind.
-- * IO has kind `* => *` so it is caught in 'SrcTyCon'
-- * State has kind `* => * => *` so it must be caught earlier, in 'SrcTyApp',
-- in order to construct an MilMonad with the type of the state.
srcTypeToMilType :: SrcType -> MIL.Type
srcTypeToMilType (SrcTyCon srcTypeName) =
  case getTypeName srcTypeName of
    TypeName "IO" -> MIL.TyMonad $ MIL.MTyMonad MIL.IO
    TypeName "State" -> error "srcTypeToMilType: State should have been handled earlier"
    typeName -> MIL.TyTypeCon $ typeNameMil typeName  -- TODO: type variables
srcTypeToMilType (SrcTyApp _ st1 st2) =
  case st1 of
    SrcTyCon (getTypeName -> TypeName "State") ->
      MIL.TyMonad $ MIL.MTyMonad (MIL.State $ srcTypeToMilType st2)
    _ -> MIL.TyApp (srcTypeToMilType st1) (srcTypeToMilType st2)
srcTypeToMilType (SrcTyArrow _ st1 st2) =
  MIL.TyArrow (srcTypeToMilType st1) (srcTypeToMilType st2)
srcTypeToMilType (SrcTyForAll _ stv st) =
  MIL.TyForAll (typeVarMil $ getTypeVar stv) (srcTypeToMilType st)
srcTypeToMilType (SrcTyParen _ st) = srcTypeToMilType st

-- * Conversion utils

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

