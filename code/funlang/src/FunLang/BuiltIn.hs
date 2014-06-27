module FunLang.BuiltIn where

import qualified Data.Set as Set
import Data.Maybe (isJust)

import FunLang.AST
import FunLang.AST.Helpers

import qualified MIL.AST as MIL
import qualified MIL.BuiltIn as MIL

-- * Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",  StarK)
  , (TypeName "Bool",  StarK)
  , (TypeName "Int",   StarK)
  , (TypeName "Float", StarK)
  , (TypeName "IO",    StarK :=>: StarK)
  , (TypeName "State", StarK :=>: (StarK :=>: StarK))
  ]

builtInDataCons :: [(ConName, (Type, TypeName))]
builtInDataCons =
  [ (ConName "True", (boolType, TypeName "Bool"))
  , (ConName "False", (boolType, TypeName "Bool"))
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

isBuiltInFunction :: FunName -> Bool
isBuiltInFunction funName = isJust $ lookup funName builtInFunctions

builtInFunctionsMil :: [(MIL.FunName, MIL.Type)]
builtInFunctionsMil =
  [ (MIL.FunName "read_string", MIL.TyApp (MIL.TyMonad ioMonadMil) MIL.stringType)
  , (MIL.FunName "read_int",    MIL.TyApp (MIL.TyMonad ioMonadMil) MIL.intType)
  , (MIL.FunName "read_float",  MIL.TyApp (MIL.TyMonad ioMonadMil) MIL.floatType)
  ]

-- | Unsafe. Make sure that there exists such a built-in function.
getMilBuiltInFunctionType :: MIL.FunName -> MIL.Type
getMilBuiltInFunctionType milFunName = MIL.getBuiltInFunctionType milFunName builtInFunctionsMil

-- * Monads

monadKind :: Kind
monadKind = StarK :=>: StarK

monadTypes :: Set.Set TypeName
monadTypes = Set.fromList
  [ TypeName "IO"
  , TypeName "State"
  ]

pureMonadMilName :: MIL.TypeName
pureMonadMilName = MIL.TypeName "Pure_M"

pureMonadMil :: MIL.TypeM
pureMonadMil = MIL.MTyAlias pureMonadMilName

pureMonadMilType :: MIL.TypeM
pureMonadMilType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonad MIL.NonTerm

ioMonadMilName :: MIL.TypeName
ioMonadMilName = MIL.TypeName "IO_M"

ioMonadMil :: MIL.TypeM
ioMonadMil = MIL.MTyAlias ioMonadMilName

ioMonadMilType :: MIL.TypeM
ioMonadMilType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonadCons MIL.NonTerm $
      MIL.MTyMonad MIL.IO

exceptionType :: MIL.Type
exceptionType = MIL.unitType

