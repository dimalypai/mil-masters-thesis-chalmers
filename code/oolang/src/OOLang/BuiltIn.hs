module OOLang.BuiltIn where

import OOLang.AST

-- Built-in types

typeOfLiteral :: Literal -> Type
typeOfLiteral UnitLit        = TyUnit
typeOfLiteral (BoolLit   {}) = TyBool
typeOfLiteral (IntLit    {}) = TyInt
typeOfLiteral (FloatLit  {}) = TyFloat
typeOfLiteral (StringLit {}) = TyString

