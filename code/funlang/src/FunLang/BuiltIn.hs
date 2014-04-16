module FunLang.BuiltIn where

import FunLang.AST

-- Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
  , (TypeName "String", StarK)
  , (TypeName "IO",     StarK :=>: StarK)
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
ioType t = TyApp (TypeName "IO") [t]

