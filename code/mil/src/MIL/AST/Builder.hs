-- | Functions for working with AST construction.
module MIL.AST.Builder where

import MIL.AST

mkTypeVar :: String -> Type
mkTypeVar = TyVar . TypeVar

mkSimpleType :: String -> Type
mkSimpleType = TyTypeCon . TypeName

mkSimpleSrcType :: String -> SrcType
mkSimpleSrcType = SrcTyTypeCon . TypeName

-- | Constructs a kind from an integer that denotes the number of parameters of
-- a type constructor
mkKind :: Int -> Kind
mkKind 0 = StarK
mkKind n = StarK :=>: mkKind (n - 1)

-- | Applies a monad type given as a first argument to the "return type"
-- (right-most type of the type arrow) of the type given as a second argument.
monadReturnType :: MonadType -> Type -> Type
monadReturnType mt (TyArrow t1 t2) = TyArrow t1 (monadReturnType mt t2)
monadReturnType mt t = TyApp (TyMonad mt) t

applyMonadType :: MonadType -> Type -> Type
applyMonadType mt t = TyApp (TyMonad mt) t

