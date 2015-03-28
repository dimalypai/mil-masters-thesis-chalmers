-- | Helper functions for working with AST.
module MIL.AST.Helpers where

import MIL.AST
import MIL.AST.PrettyPrinter

-- * Getters

getMilTypeDefs :: Program v ct mt t -> [TypeDef t]
getMilTypeDefs (Program (typeDefs, _)) = typeDefs

getMilFunDefs :: Program v ct mt t -> [FunDef v ct mt t]
getMilFunDefs (Program (_, funDefs)) = funDefs

getBinderVar :: VarBinder t -> Var
getBinderVar (VarBinder (v,_)) = v

getBinderType :: VarBinder t -> t
getBinderType (VarBinder (_,t)) = t

-- | For monadic type `m a` returns a result type `a`.
-- Note: Unsafe. Make sure you pass a monadic type.
getMonadResultType :: Type -> Type
getMonadResultType (TyApp (TyMonad _) t) = t
getMonadResultType t = error $ "Type '" ++ show t ++ "' is not monadic"

-- | For monadic type `m a` returns a monad type `m`.
-- Note: Unsafe. Make sure you pass a monadic type.
getMonadTypeFromApp :: Type -> MonadType
getMonadTypeFromApp (TyApp (TyMonad mt) _) = mt
getMonadTypeFromApp t = error $ "Type '" ++ show t ++ "' is not monadic"

getSrcResultType :: SrcType -> SrcType
getSrcResultType (SrcTyApp _ t2) = t2
getSrcResultType t = error $ "Type '" ++ prPrint t ++ "' is not an application"

-- * Predicates

isTypeVar :: Type -> Bool
isTypeVar (TyVar _) = True
isTypeVar         _ = False

isTupleType :: Type -> Bool
isTupleType (TyTuple _) = True
isTupleType           _ = False

-- * Conversions

varToFunName :: Var -> FunName
varToFunName (Var varName) = FunName varName

typeVarToTypeName :: TypeVar -> TypeName
typeVarToTypeName (TypeVar typeVarName) = TypeName typeVarName

typeNameToTypeVar :: TypeName -> TypeVar
typeNameToTypeVar (TypeName typeName) = TypeVar typeName

funNameToVar :: FunName -> Var
funNameToVar (FunName funName) = Var funName

