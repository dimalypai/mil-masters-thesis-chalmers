module FunLang.BuiltIn where

import qualified Data.Set as Set

import FunLang.AST
import FunLang.AST.Helpers

-- * Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
  , (TypeName "IO",     StarK :=>: StarK)
  , (TypeName "State",  StarK :=>: (StarK :=>: StarK))
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

stateType :: Type -> Type -> Type
stateType s a = TyApp (TypeName "State") [s, a]

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
  -- Monadic run functions
  {-
  , (FunName "runState",  TyForAll (TypeVar "S") $ TyForAll (TypeVar "A") $
                            TyArrow (stateType (mkTypeVar "S") (mkTypeVar "A"))
                                    (TyArrow (mkTypeVar "S") undefined))
  -}
  , (FunName "evalState", TyForAll (TypeVar "S") $ TyForAll (TypeVar "A") $
                            TyArrow (stateType (mkTypeVar "S") (mkTypeVar "A"))
                                    (TyArrow (mkTypeVar "S") (mkTypeVar "A")))
  , (FunName "execState", TyForAll (TypeVar "S") $ TyForAll (TypeVar "A") $
                            TyArrow (stateType (mkTypeVar "S") (mkTypeVar "A"))
                                    (TyArrow (mkTypeVar "S") (mkTypeVar "S")))
  -- Monadic operations
  , (FunName "get",    TyForAll (TypeVar "S") $ stateType (mkTypeVar "S") (mkTypeVar "S"))
  , (FunName "put",    TyForAll (TypeVar "S") $ TyArrow (mkTypeVar "S")
                                                        (stateType (mkTypeVar "S") unitType))
  , (FunName "modify", TyForAll (TypeVar "S") $ TyArrow (TyArrow (mkTypeVar "S") (mkTypeVar "S"))
                                                        (stateType (mkTypeVar "S") unitType))
  ]

-- * Monads

monadKind :: Kind
monadKind = StarK :=>: StarK

monadTypes :: Set.Set TypeName
monadTypes = Set.fromList
  [ TypeName "IO"
  , TypeName "State"
  ]

