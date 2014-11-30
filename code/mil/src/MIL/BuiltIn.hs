{-# LANGUAGE TupleSections #-}

module MIL.BuiltIn where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import MIL.AST

-- * Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Bool",   StarK)
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
  , (TypeName "Char",   StarK)
  , (TypeName "Ref",    StarK :=>: StarK)
  ]

builtInDataCons :: [(ConName, (Type, TypeName))]
builtInDataCons =
  [ (ConName "True", (mkSimpleType "Bool", TypeName "Bool"))
  , (ConName "False", (mkSimpleType "Bool", TypeName "Bool"))
  ]

builtInMonads :: Map.Map TypeName Kind
builtInMonads = Map.fromList
  [ (TypeName "Id",      StarK :=>: StarK)
  , (TypeName "State",   StarK :=>: StarK)
  , (TypeName "Error",   StarK :=>: (StarK :=>: StarK))
  , (TypeName "NonTerm", StarK :=>: StarK)
  , (TypeName "IO",      StarK :=>: StarK)
  ]

isBuiltInMonad :: TypeName -> Bool
isBuiltInMonad typeName = Map.member typeName builtInMonads

-- | Unsafe. Make sure that there exists such a built-in monad.
getBuiltInMonadKind :: TypeName -> Kind
getBuiltInMonadKind typeName = fromJust $ Map.lookup typeName builtInMonads

mkBuiltInMonad :: TypeName -> MilMonad
mkBuiltInMonad (TypeName typeName) = read typeName

builtInMonadTypeName :: MilMonad -> TypeName
builtInMonadTypeName = TypeName . show

unitType :: Type
unitType = mkSimpleType "Unit"

boolType :: Type
boolType = mkSimpleType "Bool"

intType :: Type
intType = mkSimpleType "Int"

floatType :: Type
floatType = mkSimpleType "Float"

charType :: Type
charType = mkSimpleType "Char"

refType :: Type -> Type
refType t = TyApp (TyTypeCon $ TypeName "Ref") t

ioType :: Type -> Type
ioType t = TyApp (TyMonad $ MTyMonad $ SinMonad IO) t

stateType :: Type -> Type
stateType t = TyApp (TyMonad $ MTyMonad $ SinMonad State) t

typeOfLiteral :: Literal -> Type
typeOfLiteral UnitLit      = unitType
typeOfLiteral IntLit    {} = intType
typeOfLiteral FloatLit  {} = floatType
typeOfLiteral CharLit   {} = charType

-- * Built-in functions

builtInFunctions :: [(FunName, Type)]
builtInFunctions =
  [
  -- IO functions
    (FunName "print_int",    TyArrow intType    (ioType unitType))
  , (FunName "print_float",  TyArrow floatType  (ioType unitType))
  , (FunName "print_char",   TyArrow charType   (ioType unitType))
  , (FunName "read_char",    ioType  charType)
  -- Ref functions
  , (FunName "new_ref",   TyForAll (TypeVar "A")
      (TyArrow (mkTypeVar "A") (stateType (refType $ mkTypeVar "A"))))
  , (FunName "read_ref",  TyForAll (TypeVar "A")
      (TyArrow (refType $ mkTypeVar "A") (stateType $ mkTypeVar "A")))
  , (FunName "write_ref", TyForAll (TypeVar "A")
      (TyArrow (refType $ mkTypeVar "A") (TyArrow (mkTypeVar "A") (stateType unitType))))
  ]

-- Unsafe. Make sure that there exists such a built-in function.
getBuiltInFunctionType :: FunName -> Type
getBuiltInFunctionType funName =
  fromJust $ lookup funName builtInFunctions

