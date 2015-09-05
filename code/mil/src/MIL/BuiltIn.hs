{-# LANGUAGE TupleSections #-}

module MIL.BuiltIn where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import MIL.AST
import MIL.AST.Builder

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

errorType :: Type -> Type -> Type
errorType = errorTypeParam defaultMonadError

errorTypeParam :: (Type -> MonadType) -> Type -> Type -> Type
errorTypeParam monadError et at = TyApp (TyMonad $ monadError et) at

defaultMonadError :: Type -> MonadType
defaultMonadError = MTyMonad . SinMonadApp (SinMonad Error)

-- * Built-in functions

builtInFunctions :: [Type -> MonadType] -> [(FunName, Type)]
builtInFunctions monadTypeCons =
  [
  -- IO functions
    (FunName "print_char", TyArrow charType   (ioType unitType))
  , (FunName "read_char",  ioType  charType)
  -- Ref functions
  , (FunName "new_ref",   TyForAll (TypeVar "A")
      (TyArrow (mkTypeVar "A") (stateType (refType $ mkTypeVar "A"))))
  , (FunName "read_ref",  TyForAll (TypeVar "A")
      (TyArrow (refType $ mkTypeVar "A") (stateType $ mkTypeVar "A")))
  , (FunName "write_ref", TyForAll (TypeVar "A")
      (TyArrow (refType $ mkTypeVar "A") (TyArrow (mkTypeVar "A") (stateType unitType))))
  -- Error functions
  , (FunName "throw_error", TyForAll (TypeVar "E") $ TyForAll (TypeVar "A")
      (TyArrow (mkTypeVar "E") (errorType (mkTypeVar "E") (mkTypeVar "A"))))
  , (FunName "catch_error_1", TyForAll (TypeVar "E") $ TyForAll (TypeVar "A")
      (TyArrow (errorTypeParam (monadTypeCons !! 0) (mkTypeVar "E") (mkTypeVar "A"))
               (TyArrow (TyArrow (mkTypeVar "E") (errorTypeParam (monadTypeCons !! 0) (mkTypeVar "E") (mkTypeVar "A")))
                        (errorTypeParam (monadTypeCons !! 0) (mkTypeVar "E") (mkTypeVar "A")))))
  , (FunName "catch_error_2", TyForAll (TypeVar "E") $ TyForAll (TypeVar "A")
      (TyArrow (errorTypeParam (monadTypeCons !! 1) (mkTypeVar "E") (mkTypeVar "A"))
               (TyArrow (TyArrow (mkTypeVar "E") (errorTypeParam (monadTypeCons !! 1) (mkTypeVar "E") (mkTypeVar "A")))
                        (errorTypeParam (monadTypeCons !! 1) (mkTypeVar "E") (mkTypeVar "A")))))
  -- Arithmetic functions
  , (FunName "add_int",   TyArrow intType (TyArrow intType intType))
  , (FunName "add_float", TyArrow floatType (TyArrow floatType floatType))
  , (FunName "sub_int",   TyArrow intType (TyArrow intType intType))
  , (FunName "sub_float", TyArrow floatType (TyArrow floatType floatType))
  , (FunName "mul_int",   TyArrow intType (TyArrow intType intType))
  , (FunName "mul_float", TyArrow floatType (TyArrow floatType floatType))
  , (FunName "div_int",   TyArrow intType (TyArrow intType (errorType unitType intType)))
  , (FunName "div_float", TyArrow floatType (TyArrow floatType (errorType unitType intType)))
  -- Comparison functions
  , (FunName "eq_int",   TyArrow intType (TyArrow intType boolType))
  , (FunName "eq_float", TyArrow floatType (TyArrow floatType boolType))
  , (FunName "eq_char",  TyArrow charType (TyArrow charType boolType))
  ]

