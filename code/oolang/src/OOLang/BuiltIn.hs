module OOLang.BuiltIn where

import OOLang.AST

-- Built-in types

typeOfLiteral :: Literal -> Type
typeOfLiteral UnitLit        = TyPure TyUnit
typeOfLiteral (BoolLit   {}) = TyPure TyBool
typeOfLiteral (IntLit    {}) = TyPure TyInt
typeOfLiteral (FloatLit  {}) = TyPure TyFloat
typeOfLiteral (StringLit {}) = TyPure TyString

