{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
module FunLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM
  , TypeEnv
  , initTypeEnv
  , mkTypeEnv
  , DataTypeEnv
  , getDataTypeEnv
  , modifyDataTypeEnv
  , isTypeDefined
  , addType
  , FunTypeEnv
  , ftiType
  , ftiSrcType
  , ftiSrcFunName
  , getFunTypeEnv
  , modifyFunTypeEnv
  , isFunctionDefined
  , addFunction
  , getFunTypeInfo
  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import FunLang.AST
import FunLang.TypeChecker.TcError

-- | Type checking monad. Uses 'StateT' for type environment and 'ErrorT' for
-- error handling.
newtype TypeCheckM a = TC { runTC :: StateT TypeEnv (ErrorT TcError Identity) a }
  deriving (Monad, MonadState TypeEnv, MonadError TcError, Functor, Applicative)

-- | Main entry point to the type checking monad.
-- Get a type checking computation and a type environment to begin with.
runTypeCheckM :: TypeCheckM a -> TypeEnv -> Either TcError (a, TypeEnv)
runTypeCheckM tcm typeEnv = runIdentity $
                            runErrorT $
                            runStateTFrom typeEnv $
                            runTC tcm

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (DataTypeEnv, DataConTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv (Map.empty, Map.empty, Map.empty)

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: DataTypeEnv -> DataConTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv dataTypeEnv dataConTypeEnv funTypeEnv =
  TypeEnv (dataTypeEnv, dataConTypeEnv, funTypeEnv)

-- Data type environment

type DataTypeEnv = Map.Map TypeName DataTypeInfo

data DataTypeInfo = DataTypeInfo
  Kind
  [ConName]
  SrcTypeName

getDataTypeEnv :: TypeCheckM DataTypeEnv
getDataTypeEnv = do
  (dataTypeEnv, _, _) <- gets unTypeEnv
  return dataTypeEnv

modifyDataTypeEnv :: (DataTypeEnv -> DataTypeEnv) -> TypeCheckM ()
modifyDataTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv (f dataTypeEnv) dataConTypeEnv funTypeEnv

isTypeDefined :: TypeName -> TypeCheckM Bool
isTypeDefined typeName = do
  dataTypeEnv <- getDataTypeEnv
  return $ Map.member typeName dataTypeEnv

-- | Doesn't check if the type is already in the environment.
-- Will overwrite it in this case.
addType :: SrcTypeName -> Kind -> TypeCheckM ()
addType srcTypeName kind = do
  let typeName = getTypeName srcTypeName
  modifyDataTypeEnv $ Map.insert typeName (DataTypeInfo kind dataConsStub srcTypeName)

-- | A placeholder for data constructors in the DataTypeEnv.  There are moments
-- during the type checking when this information is not available yet.
dataConsStub :: [ConName]
dataConsStub = error "Data constructors are not available yet"

-- Data constructor type environment

type DataConTypeEnv = Map.Map ConName DataConTypeInfo

data DataConTypeInfo = DataConTypeInfo
  Type
  TypeName
  SrcConName

getDataConTypeEnv :: TypeCheckM DataConTypeEnv
getDataConTypeEnv = do
  (_, dataConTypeEnv, _) <- gets unTypeEnv
  return dataConTypeEnv

-- Function type environment

type FunTypeEnv = Map.Map FunName FunTypeInfo

data FunTypeInfo = FunTypeInfo
  {
    -- | Internal representation of the function type. Becomes available after type
    -- checking of the function is completed.
    ftiType       :: Type
    -- | Source representation of function type. Gets transformed into the
    -- internal representation as the type checking procedes. May be useful for
    -- error messages.
  , ftiSrcType    :: SrcType
  , ftiSrcFunName :: SrcFunName  -- ^ Source name. For error messages.
  }

getFunTypeEnv :: TypeCheckM FunTypeEnv
getFunTypeEnv = do
  (_, _, funTypeEnv) <- gets unTypeEnv
  return funTypeEnv

modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv dataTypeEnv dataConTypeEnv (f funTypeEnv)

isFunctionDefined :: FunName -> TypeCheckM Bool
isFunctionDefined funName = do
  funTypeEnv <- getFunTypeEnv
  return $ Map.member funName funTypeEnv

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: SrcFunName -> SrcType -> TypeCheckM ()
addFunction srcFunName funSrcType = do
  let funName = getFunName srcFunName
  modifyFunTypeEnv $ Map.insert funName (FunTypeInfo funTypeStub funSrcType srcFunName)

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfo :: FunName -> TypeCheckM FunTypeInfo
getFunTypeInfo funName = do
  funTypeEnv <- getFunTypeEnv
  return $ fromJust $ Map.lookup funName funTypeEnv  -- fromJust may fail

funTypeStub :: Type
funTypeStub = error "Internal representation of the function type is not available yet"

-- Utils

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT
