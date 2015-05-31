-- | Functions for working with AST construction.
module MIL.AST.Builder where

import MIL.AST

mkSrcFunDef :: String -> SrcType -> SrcExpr -> SrcFunDef
mkSrcFunDef funNameStr funType bodyExpr =
  FunDef (FunName funNameStr) funType bodyExpr

mkCharLit :: Char -> Expr v ct mt t
mkCharLit = LitE . CharLit

mkSrcVar :: String -> SrcExpr
mkSrcVar = VarE . Var

mkSrcLambda :: Var -> SrcType -> SrcExpr -> SrcExpr
mkSrcLambda var varType bodyExpr =
  LambdaE (VarBinder (var, varType)) bodyExpr

mkSrcConName :: String -> SrcExpr
mkSrcConName conNameStr = ConNameE (ConName conNameStr) ()

mkSrcLet :: Var -> SrcType -> SrcExpr -> SrcExpr -> SrcExpr
mkSrcLet var varType bindExpr bodyExpr =
  LetE (VarBinder (var, varType)) bindExpr bodyExpr

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

