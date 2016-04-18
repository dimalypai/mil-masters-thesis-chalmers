module FunLang.BuiltIn where

import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)

import FunLang.AST
import FunLang.AST.Helpers

-- * Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Bool",   StarK)
  , (TypeName "Pair",   StarK :=>: (StarK :=>: StarK))
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
  , (TypeName "String", StarK)
  , (TypeName "IO",     StarK :=>: StarK)
  , (TypeName "State",  StarK :=>: (StarK :=>: StarK))
  ]

builtInDataCons :: [(ConName, (Type, TypeName))]
builtInDataCons =
  [ (ConName "True", (boolType, TypeName "Bool"))
  , (ConName "False", (boolType, TypeName "Bool"))
  , (ConName "MkPair", (TyForAll (TypeVar "A_") $
                          TyForAll (TypeVar "B_") $
                            TyArrow (mkTypeVar "A_")
                              (TyArrow (mkTypeVar "B_")
                                 (TyApp (TypeName "Pair") [mkTypeVar "A_", mkTypeVar "B_"])), TypeName "Pair"))
  ]

unitType :: Type
unitType = mkSimpleType "Unit"

boolType :: Type
boolType = mkSimpleType "Bool"

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

typeOfLiteral :: Literal t s -> Type
typeOfLiteral UnitLit   {} = unitType
typeOfLiteral IntLit    {} = intType
typeOfLiteral FloatLit  {} = floatType
typeOfLiteral StringLit {} = stringType

-- * Built-in functions

-- | Type variable names are mangled a bit to decrease a chance of collision
-- with type names in the source code, since we don't allow shadowing.
builtInFunctions :: [(FunName, Type)]
builtInFunctions =
  [
  -- IO functions
    (FunName "printString", TyArrow stringType (ioType unitType))
  , (FunName "readString",  ioType stringType)
  , (FunName "printInt",    TyArrow intType    (ioType unitType))
  , (FunName "readInt",     ioType intType)
  , (FunName "printFloat",  TyArrow floatType  (ioType unitType))
  , (FunName "readFloat",   ioType floatType)
  -- Monadic run functions
  , (FunName "runState",  TyForAll (TypeVar "S_") $ TyForAll (TypeVar "A_") $
                            TyArrow (stateType (mkTypeVar "S_") (mkTypeVar "A_"))
                                    (TyArrow (mkTypeVar "S_") (TyApp (TypeName "Pair") [mkTypeVar "A_", mkTypeVar "S_"])))
  , (FunName "evalState", TyForAll (TypeVar "S_") $ TyForAll (TypeVar "A_") $
                            TyArrow (stateType (mkTypeVar "S_") (mkTypeVar "A_"))
                                    (TyArrow (mkTypeVar "S_") (mkTypeVar "A_")))
  , (FunName "execState", TyForAll (TypeVar "S_") $ TyForAll (TypeVar "A_") $
                            TyArrow (stateType (mkTypeVar "S_") (mkTypeVar "A_"))
                                    (TyArrow (mkTypeVar "S_") (mkTypeVar "S_")))
  -- Monadic operations
  , (FunName "get",    TyForAll (TypeVar "S_") $ stateType (mkTypeVar "S_") (mkTypeVar "S_"))
  , (FunName "put",    TyForAll (TypeVar "S_") $ TyArrow (mkTypeVar "S_")
                                                        (stateType (mkTypeVar "S_") unitType))
  , (FunName "modify", TyForAll (TypeVar "S_") $ TyArrow (TyArrow (mkTypeVar "S_") (mkTypeVar "S_"))
                                                        (stateType (mkTypeVar "S_") unitType))
  ]

isBuiltInFunction :: FunName -> Bool
isBuiltInFunction funName = isJust $ lookup funName builtInFunctions

-- | Unsafe. Make sure that there exists such a built-in function.
getBuiltInFunctionType :: FunName -> Type
getBuiltInFunctionType funName = fromJust $ lookup funName builtInFunctions

-- * Monads

monadKind :: Kind
monadKind = StarK :=>: StarK

monadTypes :: Set.Set TypeName
monadTypes = Set.fromList
  [ TypeName "IO"
  , TypeName "State"
  ]

