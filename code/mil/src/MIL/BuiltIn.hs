module MIL.BuiltIn where

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

ioType :: Type -> Type
ioType t = TyApp (TyMonad $ MTyMonad IO) t

stateType :: Type -> Type
stateType t = TyApp (TyMonad $ MTyMonad State) t

refType :: Type -> Type
refType t = TyApp (TyTypeCon $ TypeName "Ref") t

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

-- | It takes a list of additional built-in functions to search in as well.
-- This is done, because MIL can get additional supply of them from the source
-- language.
-- Unsafe. Make sure that there exists such a built-in function.
getBuiltInFunctionType :: FunName -> [(FunName, Type)] -> Type
getBuiltInFunctionType funName addBuiltInFuns =
  fromJust $ lookup funName (builtInFunctions ++ addBuiltInFuns)

