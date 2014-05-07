{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
-- This module is mostly for working with type environment (querying, adding)
-- via monadic wrappers for pure functions from "FunLang.TypeChecker.TypeEnv".
-- Nothing smart should happen here.
module FunLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM

  , isTypeDefinedM
  , addTypeM
  , getDataTypeKindM

  , isDataConDefinedM
  , addDataConM
  , getDataConTypeInfoM

  , isFunctionDefinedM
  , addFunctionM
  , getFunTypeInfoM

  , isVarBoundM
  , isTypeVarBoundM
  , getVarTypeM
  , locallyWithEnvM

  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import FunLang.AST
import FunLang.AST.Helpers
import FunLang.TypeChecker.TypeEnv
import FunLang.TypeChecker.TcError
import FunLang.Utils

-- | Type checking monad. Uses 'StateT' for type environment, 'ErrorT' for
-- error handling and 'Reader' for local type environment.
newtype TypeCheckM a = TC { runTC :: StateT TypeEnv (ErrorT TcError (Reader LocalTypeEnv)) a }
  deriving (Monad, MonadState TypeEnv, MonadError TcError, MonadReader LocalTypeEnv, Functor, Applicative)

-- | Main entry point to the type checking monad.
-- Get a type checking computation and a type environment to begin with.
runTypeCheckM :: TypeCheckM a -> TypeEnv -> Either TcError (a, TypeEnv)
runTypeCheckM tcm typeEnv = runReaderFrom emptyLocalTypeEnv $
                            runErrorT $
                            runStateTFrom typeEnv $
                            runTC tcm

-- * Data type environment

getDataTypeEnvM :: TypeCheckM DataTypeEnv
getDataTypeEnvM = gets getDataTypeEnv

modifyDataTypeEnv :: (DataTypeEnv -> DataTypeEnv) -> TypeCheckM ()
modifyDataTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnvM <*> getDataConTypeEnvM <*> getFunTypeEnvM
  put $ mkTypeEnv (f dataTypeEnv) dataConTypeEnv funTypeEnv

isTypeDefinedM :: TypeName -> TypeCheckM Bool
isTypeDefinedM typeName = gets (isTypeDefined typeName . getDataTypeEnv)

-- | Doesn't check if the type is already in the environment.
-- Will overwrite it in this case.
--
-- Note: data constructors are not available at this point.
addTypeM :: TypeName -> Kind -> TypeCheckM ()
addTypeM typeName kind = modifyDataTypeEnv $ addType typeName kind

-- | Returns a kind of the data type.
--
-- Note: Unsafe. Should be used only after check that data type is defined.
getDataTypeKindM :: TypeName -> TypeCheckM Kind
getDataTypeKindM typeName = gets (dtiKind . getDataTypeInfo typeName . getDataTypeEnv)

-- * Data constructor type environment

getDataConTypeEnvM :: TypeCheckM DataConTypeEnv
getDataConTypeEnvM = gets getDataConTypeEnv

modifyDataConTypeEnv :: (DataConTypeEnv -> DataConTypeEnv) -> TypeCheckM ()
modifyDataConTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnvM <*> getDataConTypeEnvM <*> getFunTypeEnvM
  put $ mkTypeEnv dataTypeEnv (f dataConTypeEnv) funTypeEnv

isDataConDefinedM :: ConName -> TypeCheckM Bool
isDataConDefinedM conName = gets (isDataConDefined conName . getDataConTypeEnv)

-- | Doesn't check if the constructor is already in the environment.
-- Will overwrite it in this case.
addDataConM :: ConName -> Type -> TypeName -> TypeCheckM ()
addDataConM conName conType typeName =
  modifyDataConTypeEnv $ addDataCon conName conType typeName

-- | Returns all information about the data constructor from the environment.
--
-- Note: Unsafe. Should be used only after check that data constructor is defined.
getDataConTypeInfoM :: ConName -> TypeCheckM DataConTypeInfo
getDataConTypeInfoM conName = gets (getDataConTypeInfo conName . getDataConTypeEnv)

-- * Function type environment

getFunTypeEnvM :: TypeCheckM FunTypeEnv
getFunTypeEnvM = gets getFunTypeEnv

modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnvM <*> getDataConTypeEnvM <*> getFunTypeEnvM
  put $ mkTypeEnv dataTypeEnv dataConTypeEnv (f funTypeEnv)

isFunctionDefinedM :: FunName -> TypeCheckM Bool
isFunctionDefinedM funName = gets (isFunctionDefined funName . getFunTypeEnv)

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunctionM :: FunName -> Type -> SrcType -> TypeCheckM ()
addFunctionM funName funType funSrcType =
  modifyFunTypeEnv $ addFunction funName funType funSrcType

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfoM :: FunName -> TypeCheckM FunTypeInfo
getFunTypeInfoM funName = gets (getFunTypeInfo funName . getFunTypeEnv)

-- * Local type environment

-- | Check whether a variable is in scope. It can be either local variable name
-- or a function name.
isVarBoundM :: Var -> TypeCheckM Bool
isVarBoundM var = do
  isLocalVar <- asks (isVarBound var)
  isFunction <- isFunctionDefinedM (varToFunName var)
  return (isLocalVar || isFunction)

-- | Check whether a type variable is in scope.
isTypeVarBoundM :: TypeVar -> TypeCheckM Bool
isTypeVarBoundM typeVar = asks (isTypeVarBound typeVar)

-- | Returns variable type. First looks for locals and then for functions.
--
-- Note: Unsafe. Should be used only after check that the variable is bound.
getVarTypeM :: Var -> TypeCheckM Type
getVarTypeM var = do
  mVarType <- asks (getVarType var)
  case mVarType of
    Just varType -> return varType
    Nothing -> ftiType <$> getFunTypeInfoM (varToFunName var)

-- | Takes a separate local type environment and merges it with what is already
-- in the local type environment and performs a given computation in this new
-- environment. Then restores the local environment.
-- Should be safe, since we don't allow shadowing.
locallyWithEnvM :: LocalTypeEnv -> TypeCheckM a -> TypeCheckM a
locallyWithEnvM localTypeEnv = local (locallyWithEnv localTypeEnv)

