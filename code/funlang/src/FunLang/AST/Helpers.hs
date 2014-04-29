-- | Helper functions for working with AST.
module FunLang.AST.Helpers where

import FunLang.AST

-- * Getters

getFunEqBody :: FunEq v s -> Expr v s
getFunEqBody (FunEq _ _ _ bodyExpr) = bodyExpr

getVarTyVar :: VarTy -> Var
getVarTyVar (VarTy (var, _)) = var

getVarTyType :: VarTy -> Type
getVarTyType (VarTy (_, varType)) = varType

getBinderVar :: VarBinder s -> VarS s
getBinderVar (VarBinder _ srcVar _) = srcVar

getBinderType :: VarBinder s -> TypeS s
getBinderType (VarBinder _ _ srcType) = srcType

-- | Unsafe. Make sure that you pass 'TyApp'.
getTyAppTypeName :: Type -> TypeName
getTyAppTypeName (TyApp typeName _) = typeName
getTyAppTypeName _ = error "getTyAppTypeName: not a type application"

-- | For the monadic type `m a` returns a type representing m.
-- Unsafe. Make sure that you pass 'TyApp' with at least one type argument.
getMonadType :: Type -> Type
getMonadType (TyApp typeName typeArgs) = TyApp typeName (init typeArgs)
getMonadType _ = error "getMonadType: not a monadic type"

-- | For the type `m a` where m is a monad, returns a.
-- Unsafe. Make sure that you pass 'TyApp' with at least one type argument.
getMonadResultType :: Type -> Type
getMonadResultType (TyApp _ typeArgs) = last typeArgs
getMonadResultType _ = error "getMonadResultType: not a monadic type"

-- * Synonyms

getLiteral :: LiteralS s -> Literal
getLiteral = fst

getBinOp :: BinOpS s -> BinOp
getBinOp = fst

getVar :: VarS s -> Var
getVar = fst

getTypeVar :: TypeVarS s -> TypeVar
getTypeVar = fst

getTypeName :: TypeNameS s -> TypeName
getTypeName = fst

getConName :: ConNameS s -> ConName
getConName = fst

getFunName :: FunNameS s -> FunName
getFunName = fst

-- * Conversions

varToFunName :: Var -> FunName
varToFunName (Var varName) = FunName varName

typeVarToTypeName :: TypeVar -> TypeName
typeVarToTypeName (TypeVar typeVar) = TypeName typeVar

typeNameToTypeVar :: TypeName -> TypeVar
typeNameToTypeVar (TypeName typeName) = TypeVar typeName

srcTypeNameToTypeVar :: TypeNameS s -> TypeVarS s
srcTypeNameToTypeVar (typeName, s) = (typeNameToTypeVar typeName, s)

-- * Constructors

mkTypeVar :: String -> Type
mkTypeVar = TyVar . TypeVar

mkSimpleType :: String -> Type
mkSimpleType typeName = TyApp (TypeName typeName) []

-- | Constructs a kind from an integer that denotes the number of parameters of
-- a type constructor
mkKind :: Int -> Kind
mkKind 0 = StarK
mkKind n = StarK :=>: mkKind (n - 1)

-- | Takes a type representing monad m and some type a and return a type
-- representing `m a`.
-- Unsafe. Make sure that you pass 'TyApp'.
applyMonadType :: Type -> Type -> Type
applyMonadType (TyApp typeName typeArgs) resType = TyApp typeName (typeArgs ++ [resType])
applyMonadType _ _ = error "applyMonadType: not a monadic type"

-- * Type predicates

isTypeVar :: Type -> Bool
isTypeVar (TyVar _) = True
isTypeVar         _ = False

isAtomicType :: Type -> Bool
isAtomicType TyArrow   {} = False
isAtomicType (TyApp _ []) = True
isAtomicType TyApp     {} = False
isAtomicType TyForAll  {} = False
isAtomicType            _ = True

-- * Type precedences

getTypePrec :: Type -> Int
getTypePrec TyVar    {} = 4
getTypePrec TyArrow  {} = 2
getTypePrec TyApp    {} = 3
getTypePrec TyForAll {} = 1

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
--
-- Note: It is reflexive: t `typeHasLowerPrec` t ==> True
typeHasLowerPrec :: Type -> Type -> Bool
typeHasLowerPrec t1 t2 = getTypePrec t1 <= getTypePrec t2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
-- This version can be used with associative type operators, for example:
-- arrow, type application. See "FunLang.AST.PrettyPrinter".
--
-- Note: It is *not* reflexive: t `typeHasLowerPrecAssoc` t ==> False
typeHasLowerPrecAssoc :: Type -> Type -> Bool
typeHasLowerPrecAssoc t1 t2 = getTypePrec t1 < getTypePrec t2

-- * Parsing helpers

isTypeDef :: TopDef v s -> Bool
isTypeDef (TopTypeDef _) = True
isTypeDef              _ = False

isFunDef :: TopDef v s -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

