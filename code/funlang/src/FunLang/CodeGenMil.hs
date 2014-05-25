{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}

-- | Module responsible for MIL code generation.
module FunLang.CodeGenMil
  ( codeGen
  ) where

import Control.Monad.Reader
import Control.Applicative
import Data.List (foldl')

import FunLang.AST
import FunLang.AST.Helpers
import FunLang.TypeChecker
import FunLang.TypeChecker.TypeEnv
import qualified MIL.AST as MIL

-- | Entry point to the code generator.
-- Takes a type checked program in FunLang and a type environment and produces
-- a program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.Program
codeGen tyProgram typeEnv = runReader (runCG $ codeGenProgram tyProgram) typeEnv

-- | Code generation monad. Uses 'Reader' for querying the type environment.
newtype CodeGenM a = CG { runCG :: Reader TypeEnv a }
  deriving (Monad, MonadReader TypeEnv, Functor, Applicative)

codeGenProgram :: TyProgram -> CodeGenM MIL.Program
codeGenProgram (Program _ srcTypeDefs tyFunDefs) = do
  milTypeDefs <- mapM codeGenTypeDef srcTypeDefs
  milFunDefs <- mapM codeGenFunDef tyFunDefs
  return $ MIL.Program (milTypeDefs, [], milFunDefs)

codeGenTypeDef :: SrcTypeDef -> CodeGenM MIL.TypeDef
codeGenTypeDef (TypeDef _ srcTypeName srcTypeVars srcConDefs) = do
  let milTypeVars = map (typeVarMil . getTypeVar) srcTypeVars
  milConDefs <- mapM codeGenConDef srcConDefs
  return $ MIL.TypeDef (typeNameMil $ getTypeName srcTypeName) milTypeVars milConDefs

codeGenConDef :: SrcConDef -> CodeGenM MIL.ConDef
codeGenConDef (ConDef _ srcConName srcConFields) = do
  milConFields <- mapM srcTypeToMilType srcConFields
  return $ MIL.ConDef (conNameMil $ getConName srcConName) milConFields

codeGenFunDef :: TyFunDef -> CodeGenM MIL.FunDef
codeGenFunDef (FunDef _ srcFunName _ tyFunEqs) = do
  let funName = getFunName srcFunName
  milFunType <- typeMil <$> asks (ftiType . getFunTypeInfo funName . getFunTypeEnv)
  milFunBody <- codeGenFunEqs tyFunEqs milFunType
  return $ MIL.FunDef (funNameMil funName) milFunType milFunBody

-- | Takes function equations of the function definition and the transformed
-- function type and returns an MIL expression.
-- Type is needed for annotating monadic operations.
codeGenFunEqs :: [TyFunEq] -> MIL.Type -> CodeGenM MIL.Expr
codeGenFunEqs tyFunEqs milFunType = do
  funEqExprs <- mapM (codeGenExpr milFunType . getFunEqBody) tyFunEqs
  return $ head funEqExprs  -- TODO

-- | Expression code generation.
-- Takes a transformed type of the containing function for annotating monadic
-- operations.
codeGenExpr :: MIL.Type -> TyExpr -> CodeGenM MIL.Expr
codeGenExpr milFunType srcExpr =
  case srcExpr of
    LitE srcLit -> return $ MIL.LitE (literalMil $ getLiteral srcLit)

    VarE _ varTy -> do
      let var = varMil $ getVarTyVar varTy
          milVarType = typeMil $ getVarTyType varTy
      return (MIL.VarE $ MIL.VarBinder (var, milVarType))

    LambdaE _ varBinders tyBodyExpr -> do
      milBodyExpr <- codeGenExpr milFunType tyBodyExpr
      foldM (\mexpr vb -> do
          milVarType <- srcTypeToMilType $ getBinderType vb
          return $ MIL.LambdaE (MIL.VarBinder ( varMil (getVar $ getBinderVar vb)
                                              , milVarType)) mexpr)
        milBodyExpr
        (reverse varBinders)

    TypeLambdaE _ srcTypeVars tyBodyExpr -> do
      milBodyExpr <- codeGenExpr milFunType tyBodyExpr
      return $
        foldr (\tv mexpr ->
          MIL.TypeLambdaE (typeVarMil tv) mexpr)
        milBodyExpr
        (map getTypeVar srcTypeVars)

    TypeAppE _ tyAppExpr srcArgType ->
      MIL.TypeAppE <$> codeGenExpr milFunType tyAppExpr <*> srcTypeToMilType srcArgType

    ConNameE srcConName -> do
      let conName = getConName srcConName
      conType <- asks (dcontiType . getDataConTypeInfo conName . getDataConTypeEnv)
      return $ MIL.ConNameE (conNameMil conName) (typeMil conType)

    BinOpE _ srcBinOp tyExpr1 tyExpr2 -> codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 milFunType

    ParenE _ tySubExpr -> codeGenExpr milFunType tySubExpr

    DoE _ tyStmts -> codeGenDoBlock tyStmts milFunType

literalMil :: Literal -> MIL.Literal
literalMil UnitLit        = MIL.UnitLit
literalMil (IntLit i)     = MIL.IntLit i
literalMil (FloatLit f _) = MIL.FloatLit f
literalMil (StringLit s)  = MIL.StringLit s

codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> MIL.Type -> CodeGenM MIL.Expr
codeGenBinOp App tyExpr1 tyExpr2 milFunType =
  MIL.AppE <$> codeGenExpr milFunType tyExpr1 <*> codeGenExpr milFunType tyExpr2

-- | TODO
codeGenDoBlock :: [TyStmt] -> MIL.Type -> CodeGenM MIL.Expr
codeGenDoBlock [ExprS _ tyExpr] milFunType = codeGenExpr milFunType tyExpr
codeGenDoBlock [ReturnS _ tyExpr] milFunType@(MIL.TyApp (MIL.TyMonad monadType) _) = do
  milExpr <- codeGenExpr milFunType tyExpr
  return $ MIL.ReturnE monadType milExpr
codeGenDoBlock (tyStmt:tyStmts) milFunType =
  case tyStmt of
    ExprS _ tyExpr ->
      MIL.LetE (MIL.VarBinder (MIL.Var "_", MIL.TyTypeCon (MIL.TypeName "Unit"))) <$>  -- TODO
               codeGenExpr milFunType tyExpr <*>
               codeGenDoBlock tyStmts milFunType

    BindS _ varBinder tyExpr -> do
      milVarType <- srcTypeToMilType $ getBinderType varBinder
      MIL.LetE (MIL.VarBinder (varMil (getVar $ getBinderVar varBinder), milVarType)) <$>
               codeGenExpr milFunType tyExpr <*>
               codeGenDoBlock tyStmts milFunType

    ReturnS _ tyExpr ->
      MIL.LetE (MIL.VarBinder (MIL.Var "_", MIL.TyTypeCon (MIL.TypeName "Unit"))) <$>  -- TODO
               codeGenExpr milFunType tyExpr <*>
               codeGenDoBlock tyStmts milFunType

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

    (TypeName "State", [stateResultType]) ->
      MIL.TyApp (MIL.TyMonad $ MIL.MTyMonad MIL.State) (typeMil stateResultType)
    (TypeName "State", _) -> error "State type is ill-formed"

    _ -> foldl' (\mt t -> MIL.TyApp mt (typeMil t))
           (MIL.TyTypeCon $ typeNameMil typeName)
           typeArgs
typeMil (TyForAll typeVar t) = MIL.TyForAll (typeVarMil typeVar) (typeMil t)

-- | Source type representation transformation.
-- Monadic types are transformed in different cases depending on their kind.
-- * IO and State have kind `* => *` so it is caught in 'SrcTyCon'.
srcTypeToMilType :: SrcType -> CodeGenM MIL.Type
srcTypeToMilType (SrcTyCon srcTypeName) =
  case getTypeName srcTypeName of
    TypeName "IO" -> return $ MIL.TyMonad (MIL.MTyMonad MIL.IO)
    TypeName "State" -> return $ MIL.TyMonad (MIL.MTyMonad MIL.State)
    typeName -> do
      -- 'SrcTyCon' can represent both type names and type variables, so we
      -- need to distinguish between them in order to generate correct MIL
      -- code.
      dataTypeEnv <- asks getDataTypeEnv
      if isTypeDefined typeName dataTypeEnv
        then return $ MIL.TyTypeCon (typeNameMil typeName)
        else return $ MIL.TyVar (typeVarMil $ typeNameToTypeVar typeName)
srcTypeToMilType (SrcTyApp _ st1 st2) =
  MIL.TyApp <$> srcTypeToMilType st1 <*> srcTypeToMilType st2
srcTypeToMilType (SrcTyArrow _ st1 st2) =
  MIL.TyArrow <$> srcTypeToMilType st1 <*> srcTypeToMilType st2
srcTypeToMilType (SrcTyForAll _ stv st) =
  MIL.TyForAll (typeVarMil $ getTypeVar stv) <$> srcTypeToMilType st
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

