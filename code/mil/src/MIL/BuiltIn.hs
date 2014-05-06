module MIL.BuiltIn where

import MIL.AST

-- * Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
  , (TypeName "String", StarK)
  ]

unitType :: Type
unitType = mkSimpleType "Unit"

intType :: Type
intType = mkSimpleType "Int"

floatType :: Type
floatType = mkSimpleType "Float"

stringType :: Type
stringType = mkSimpleType "String"

ioType :: Type -> Type
ioType t = TyApp (TyMonad $ MTyMonad IO) t

typeOfLiteral :: Literal -> Type
typeOfLiteral UnitLit      = unitType
typeOfLiteral IntLit    {} = intType
typeOfLiteral FloatLit  {} = floatType
typeOfLiteral StringLit {} = stringType

-- * Built-in functions

builtInFunctions :: [(FunName, Type)]
builtInFunctions =
  [
  -- IO functions
    (FunName "printString", TyArrow stringType (ioType unitType))
  , (FunName "printInt",    TyArrow intType    (ioType unitType))
  , (FunName "readInt",     ioType intType)
  , (FunName "printFloat",  TyArrow floatType  (ioType unitType))
  , (FunName "readFloat",   ioType floatType)
  ]

