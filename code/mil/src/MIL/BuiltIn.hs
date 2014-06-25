module MIL.BuiltIn where

import MIL.AST

-- * Built-in types

builtInDataTypes :: [(TypeName, Kind)]
builtInDataTypes =
  [ (TypeName "Unit",   StarK)
  , (TypeName "Bool",   StarK)
  , (TypeName "Int",    StarK)
  , (TypeName "Float",  StarK)
  , (TypeName "String", StarK)
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

stringType :: Type
stringType = mkSimpleType "String"

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
  -- Ref functions
  , (FunName "new_ref",   TyForAll (TypeVar "A")
      (TyArrow (mkTypeVar "A") (stateType (refType $ mkTypeVar "A"))))
  , (FunName "read_ref",  TyForAll (TypeVar "A")
      (TyArrow (refType $ mkTypeVar "A") (stateType $ mkTypeVar "A")))
  , (FunName "write_ref", TyForAll (TypeVar "A")
      (TyArrow (refType $ mkTypeVar "A") (TyArrow (mkTypeVar "A") (stateType unitType))))
  ]

