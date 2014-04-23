module FunLang.BuiltIn where

import qualified Data.Set as Set

import FunLang.AST
import FunLang.AST.Helpers

-- Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
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

typeOfLiteral :: Literal -> Type
typeOfLiteral UnitLit      = unitType
typeOfLiteral IntLit    {} = intType
typeOfLiteral FloatLit  {} = floatType
typeOfLiteral StringLit {} = stringType

-- Monads

monadKind :: Kind
monadKind = StarK :=>: StarK

monadTypes :: Set.Set TypeName
monadTypes = Set.fromList [ TypeName "IO" ]

