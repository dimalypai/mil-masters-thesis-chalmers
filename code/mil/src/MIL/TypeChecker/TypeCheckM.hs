{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
-- This module mostly is for working with type environment (querying, adding).
-- Nothing smart should happen here.
module MIL.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM

  , TypeEnv
  , initTypeEnv

  , dtiKind
  , getDataTypeInfo
  , isTypeDefined
  , addType

  , dcontiType
  , dcontiTypeName
  , getDataConTypeInfo
  , isDataConDefined
  , addDataCon

  , isAliasDefined
  , addAlias
  , getAliasType

  , isTypeOrAliasDefined

  , getFunType
  , isFunctionDefined
  , addFunction

  , LocalTypeEnv
  , emptyLocalTypeEnv
  , isVarBound
  , isTypeVarBound
  , isVarInLocalEnv
  , isTypeVarInLocalEnv
  , getVarType
  , addLocalVar
  , addLocalTypeVar
  , locallyWithEnv

  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
-- 'first' and 'second' are used just to transform components of a pair
import Control.Arrow (first, second)

import MIL.AST
import MIL.TypeChecker.TcError
import MIL.BuiltIn

-- | Type checking monad. Uses 'StateT' for type environment, 'ErrorT' for
-- error handling and 'Reader' for local type environment.
newtype TypeCheckM a = TC { runTC :: StateT TypeEnv (ErrorT TcError (Reader LocalTypeEnv)) a }
  deriving (Monad, MonadState TypeEnv, MonadError TcError, MonadReader LocalTypeEnv, Functor, Applicative)

-- | Main entry point to the type checking monad.
-- Get a type checking computation and a type environment to begin with.
runTypeCheckM :: TypeCheckM a -> TypeEnv -> Either TcError TypeEnv
runTypeCheckM tcm typeEnv = runReaderFrom emptyLocalTypeEnv $
                            runErrorT $
                            execStateTFrom typeEnv $
                            runTC tcm

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (DataTypeEnv, DataConTypeEnv, AliasTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = mkTypeEnv (Map.fromList $ map (second builtInDataTypeInfo) builtInDataTypes)
                        (Map.fromList $ map (second $ uncurry DataConTypeInfo) builtInDataCons)
                        Map.empty
                        (Map.fromList builtInFunctions)

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: DataTypeEnv -> DataConTypeEnv -> AliasTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv dataTypeEnv dataConTypeEnv aliasTypeEnv funTypeEnv =
  TypeEnv (dataTypeEnv, dataConTypeEnv, aliasTypeEnv, funTypeEnv)

-- Data type environment

type DataTypeEnv = Map.Map TypeName DataTypeInfo

data DataTypeInfo = DataTypeInfo
  { dtiKind        :: Kind
  , dtiCons        :: [ConName]
  }

getDataTypeEnv :: TypeCheckM DataTypeEnv
getDataTypeEnv = do
  (dataTypeEnv, _, _, _) <- gets unTypeEnv
  return dataTypeEnv

-- | Returns all information about the data type from the environment.
--
-- Note: Unsafe. Should be used only after check that data type is defined.
getDataTypeInfo :: TypeName -> TypeCheckM DataTypeInfo
getDataTypeInfo typeName = do
  dataTypeEnv <- getDataTypeEnv
  return $ fromJust $ Map.lookup typeName dataTypeEnv  -- fromJust may fail

modifyDataTypeEnv :: (DataTypeEnv -> DataTypeEnv) -> TypeCheckM ()
modifyDataTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, aliasTypeEnv, funTypeEnv) <-
    (,,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getAliasTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv (f dataTypeEnv) dataConTypeEnv aliasTypeEnv funTypeEnv

isTypeDefined :: TypeName -> TypeCheckM Bool
isTypeDefined typeName = do
  dataTypeEnv <- getDataTypeEnv
  return $ Map.member typeName dataTypeEnv

-- | Doesn't check if the type is already in the environment.
-- Will overwrite it in this case.
--
-- Note: data constructors are not available at this point.
addType :: TypeName -> Kind -> TypeCheckM ()
addType typeName kind =
  modifyDataTypeEnv $ Map.insert typeName (DataTypeInfo kind dataConsStub)

builtInDataTypeInfo :: Kind -> DataTypeInfo
builtInDataTypeInfo kind = DataTypeInfo kind undefined

-- | A placeholder for data constructors in the DataTypeEnv.  There are moments
-- during the type checking when this information is not available yet.
dataConsStub :: [ConName]
dataConsStub = error "Data constructors are not available yet"

-- Data constructor type environment

type DataConTypeEnv = Map.Map ConName DataConTypeInfo

data DataConTypeInfo = DataConTypeInfo
  { dcontiType     :: Type      -- ^ Function type of the data constructor.
  , dcontiTypeName :: TypeName  -- ^ Type name of the data type constructor is defined in.
  }

getDataConTypeEnv :: TypeCheckM DataConTypeEnv
getDataConTypeEnv = do
  (_, dataConTypeEnv, _, _) <- gets unTypeEnv
  return dataConTypeEnv

-- | Returns all information about the data constructor from the environment.
--
-- Note: Unsafe. Should be used only after check that data constructor is defined.
getDataConTypeInfo :: ConName -> TypeCheckM DataConTypeInfo
getDataConTypeInfo conName = do
  dataConTypeEnv <- getDataConTypeEnv
  return $ fromJust $ Map.lookup conName dataConTypeEnv  -- fromJust may fail

modifyDataConTypeEnv :: (DataConTypeEnv -> DataConTypeEnv) -> TypeCheckM ()
modifyDataConTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, aliasTypeEnv, funTypeEnv) <-
    (,,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getAliasTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv dataTypeEnv (f dataConTypeEnv) aliasTypeEnv funTypeEnv

isDataConDefined :: ConName -> TypeCheckM Bool
isDataConDefined conName = do
  dataConTypeEnv <- getDataConTypeEnv
  return $ Map.member conName dataConTypeEnv

-- | Doesn't check if the constructor is already in the environment.
-- Will overwrite it in this case.
addDataCon :: ConName -> Type -> TypeName -> TypeCheckM ()
addDataCon conName conType typeName =
  modifyDataConTypeEnv $ Map.insert conName (DataConTypeInfo conType typeName)

-- Type alias type environment

-- | Maps type alias names to the types they denote.
type AliasTypeEnv = Map.Map TypeName Type

getAliasTypeEnv :: TypeCheckM AliasTypeEnv
getAliasTypeEnv = do
  (_, _, aliasTypeEnv, _) <- gets unTypeEnv
  return aliasTypeEnv

modifyAliasTypeEnv :: (AliasTypeEnv -> AliasTypeEnv) -> TypeCheckM ()
modifyAliasTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, aliasTypeEnv, funTypeEnv) <-
    (,,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getAliasTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv dataTypeEnv dataConTypeEnv (f aliasTypeEnv) funTypeEnv

isAliasDefined :: TypeName -> TypeCheckM Bool
isAliasDefined typeName = do
  aliasTypeEnv <- getAliasTypeEnv
  return $ Map.member typeName aliasTypeEnv

-- | Doesn't check if the type alias is already in the environment.
-- Will overwrite it in this case.
addAlias :: TypeName -> Type -> TypeCheckM ()
addAlias typeName t = modifyAliasTypeEnv $ Map.insert typeName t

-- | Returns the aliased type from the environment.
--
-- Note: Unsafe. Should be used only after check that alias is defined.
getAliasType :: TypeName -> TypeCheckM Type
getAliasType typeName = do
  aliasTypeEnv <- getAliasTypeEnv
  return $ fromJust $ Map.lookup typeName aliasTypeEnv  -- fromJust may fail

isTypeOrAliasDefined :: TypeName -> TypeCheckM Bool
isTypeOrAliasDefined typeName = do
  typeDefined <- isTypeDefined typeName
  aliasDefined <- isAliasDefined typeName
  return (typeDefined || aliasDefined)

-- Function type environment

type FunTypeEnv = Map.Map FunName Type

getFunTypeEnv :: TypeCheckM FunTypeEnv
getFunTypeEnv = do
  (_, _, _, funTypeEnv) <- gets unTypeEnv
  return funTypeEnv

-- | Returns the function type from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunType :: FunName -> TypeCheckM Type
getFunType funName = do
  funTypeEnv <- getFunTypeEnv
  return $ fromJust $ Map.lookup funName funTypeEnv  -- fromJust may fail

modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (dataTypeEnv, dataConTypeEnv, aliasTypeEnv, funTypeEnv) <-
    (,,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getAliasTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv dataTypeEnv dataConTypeEnv aliasTypeEnv (f funTypeEnv)

isFunctionDefined :: FunName -> TypeCheckM Bool
isFunctionDefined funName = do
  funTypeEnv <- getFunTypeEnv
  return $ Map.member funName funTypeEnv

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: FunName -> Type -> TypeCheckM ()
addFunction funName funType = modifyFunTypeEnv $ Map.insert funName funType

-- | Local type environment (inside functions, lambdas etc.).
-- Consists of variables with their types and a set of type variables (from
-- type lambdas) in scope.
type LocalTypeEnv = (Map.Map Var Type, Set.Set TypeVar)

type LocalVars = Map.Map Var Type

type LocalTypeVars = Set.Set TypeVar

getLocalVars :: LocalTypeEnv -> LocalVars
getLocalVars = fst

getLocalTypeVars :: LocalTypeEnv -> LocalTypeVars
getLocalTypeVars = snd

modifyLocalVars :: (LocalVars -> LocalVars) -> (LocalTypeEnv -> LocalTypeEnv)
modifyLocalVars = first

modifyLocalTypeVars :: (LocalTypeVars -> LocalTypeVars) -> (LocalTypeEnv -> LocalTypeEnv)
modifyLocalTypeVars = second

-- | Empty local type environment.
emptyLocalTypeEnv :: LocalTypeEnv
emptyLocalTypeEnv = (Map.empty, Set.empty)

-- | Check whether a variable is in scope. It can be either local variable name
-- or a function name.
isVarBound :: Var -> TypeCheckM Bool
isVarBound var = do
  isLocalVar <- asks (Map.member var . getLocalVars)
  isFunction <- isFunctionDefined (varToFunName var)
  return (isLocalVar || isFunction)

-- | Check whether a type variable is in scope.
isTypeVarBound :: TypeVar -> TypeCheckM Bool
isTypeVarBound typeVar = asks (Set.member typeVar . getLocalTypeVars)

-- | Pure function for querying variables in local type environment.
isVarInLocalEnv :: Var -> LocalTypeEnv -> Bool
isVarInLocalEnv var = Map.member var . getLocalVars

-- | Pure function for querying type variables in local type environment.
isTypeVarInLocalEnv :: TypeVar -> LocalTypeEnv -> Bool
isTypeVarInLocalEnv typeVar = Set.member typeVar . getLocalTypeVars

-- | Returns variable type. First looks for locals and then for functions.
--
-- Note: Unsafe. Should be used only after check that the variable is bound.
getVarType :: Var -> TypeCheckM Type
getVarType var = do
  mVarType <- asks (Map.lookup var . getLocalVars)
  case mVarType of
    Just varType -> return varType
    Nothing -> getFunType (varToFunName var)

-- | Extends local type environment (variables part).
-- Pure (meaning, not a 'TypeCheckM' function).
addLocalVar :: Var -> Type -> LocalTypeEnv -> LocalTypeEnv
addLocalVar var varType = modifyLocalVars (Map.insert var varType)

-- | Extends local type environment (type variables part).
-- Pure (meaning, not a 'TypeCheckM' function).
addLocalTypeVar :: TypeVar -> LocalTypeEnv -> LocalTypeEnv
addLocalTypeVar typeVar = modifyLocalTypeVars (Set.insert typeVar)

-- | Takes a separate local type environment and merges it with what is already
-- in the local type environment and performs a given computation in this new
-- environment. Then restores the local environment.
-- Should be safe, since we don't allow shadowing.
locallyWithEnv :: LocalTypeEnv -> TypeCheckM a -> TypeCheckM a
locallyWithEnv localTypeEnv =
  local (\(vars, typeVars) -> ( Map.union (getLocalVars localTypeEnv) vars
                              , Set.union (getLocalTypeVars localTypeEnv) typeVars))

-- Utils

-- | Convenient version of 'execStateT' with the arguments flipped.
execStateTFrom :: Monad m => s -> StateT s m a -> m s
execStateTFrom = flip execStateT

-- | Convenient version of 'runReader' with the arguments flipped.
runReaderFrom :: r -> Reader r a -> a
runReaderFrom = flip runReader

