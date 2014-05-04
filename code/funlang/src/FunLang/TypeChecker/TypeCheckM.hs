{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
-- This module mostly is for working with type environment (querying, adding).
-- Nothing smart should happen here.
module FunLang.TypeChecker.TypeCheckM
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

  , ftiType
  , ftiSrcType
  , getFunTypeInfo
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

import FunLang.AST
import FunLang.AST.Helpers
import FunLang.TypeChecker.TcError
import FunLang.BuiltIn

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

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (DataTypeEnv, DataConTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = mkTypeEnv (Map.fromList $ map (second builtInDataTypeInfo) builtInDataTypes)
                        Map.empty
                        (Map.fromList $ map (second builtInFunTypeInfo) builtInFunctions)

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: DataTypeEnv -> DataConTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv dataTypeEnv dataConTypeEnv funTypeEnv =
  TypeEnv (dataTypeEnv, dataConTypeEnv, funTypeEnv)

-- Data type environment

type DataTypeEnv = Map.Map TypeName DataTypeInfo

data DataTypeInfo = DataTypeInfo
  { dtiKind        :: Kind
  , dtiCons        :: [ConName]
  , dtiSrcTypeName :: SrcTypeName  -- ^ Source name. For error messages.
  }

getDataTypeEnv :: TypeCheckM DataTypeEnv
getDataTypeEnv = do
  (dataTypeEnv, _, _) <- gets unTypeEnv
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
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv (f dataTypeEnv) dataConTypeEnv funTypeEnv

isTypeDefined :: TypeName -> TypeCheckM Bool
isTypeDefined typeName = do
  dataTypeEnv <- getDataTypeEnv
  return $ Map.member typeName dataTypeEnv

-- | Doesn't check if the type is already in the environment.
-- Will overwrite it in this case.
--
-- Note: data constructors are not available at this point.
addType :: SrcTypeName -> Kind -> TypeCheckM ()
addType srcTypeName kind = do
  let typeName = getTypeName srcTypeName
  modifyDataTypeEnv $ Map.insert typeName (DataTypeInfo kind dataConsStub srcTypeName)

builtInDataTypeInfo :: Kind -> DataTypeInfo
builtInDataTypeInfo kind = DataTypeInfo kind undefined undefined

-- | A placeholder for data constructors in the DataTypeEnv.  There are moments
-- during the type checking when this information is not available yet.
dataConsStub :: [ConName]
dataConsStub = error "Data constructors are not available yet"

-- Data constructor type environment

type DataConTypeEnv = Map.Map ConName DataConTypeInfo

data DataConTypeInfo = DataConTypeInfo
  { dcontiType     :: Type  -- ^ Function type of the data constructor.
  , dcontiTypeName :: TypeName  -- ^ Type name of the data type constructor is defined in.
  }

getDataConTypeEnv :: TypeCheckM DataConTypeEnv
getDataConTypeEnv = do
  (_, dataConTypeEnv, _) <- gets unTypeEnv
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
  (dataTypeEnv, dataConTypeEnv, funTypeEnv) <-
    (,,) <$> getDataTypeEnv <*> getDataConTypeEnv <*> getFunTypeEnv
  put $ mkTypeEnv dataTypeEnv (f dataConTypeEnv) funTypeEnv

isDataConDefined :: ConName -> TypeCheckM Bool
isDataConDefined conName = do
  dataConTypeEnv <- getDataConTypeEnv
  return $ Map.member conName dataConTypeEnv

-- | Doesn't check if the constructor is already in the environment.
-- Will overwrite it in this case.
addDataCon :: SrcConName -> Type -> TypeName -> TypeCheckM ()
addDataCon srcConName conType typeName = do
  let conName = getConName srcConName
  modifyDataConTypeEnv $ Map.insert conName (DataConTypeInfo conType typeName)

-- Function type environment

type FunTypeEnv = Map.Map FunName FunTypeInfo

data FunTypeInfo = FunTypeInfo
  {
    -- | Internal representation of the function type.
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

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfo :: FunName -> TypeCheckM FunTypeInfo
getFunTypeInfo funName = do
  funTypeEnv <- getFunTypeEnv
  return $ fromJust $ Map.lookup funName funTypeEnv  -- fromJust may fail

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
addFunction :: SrcFunName -> Type -> SrcType -> TypeCheckM ()
addFunction srcFunName funType funSrcType = do
  let funName = getFunName srcFunName
  modifyFunTypeEnv $ Map.insert funName (FunTypeInfo funType funSrcType srcFunName)

builtInFunTypeInfo :: Type -> FunTypeInfo
builtInFunTypeInfo funType = FunTypeInfo funType undefined undefined

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
    Nothing -> do
      funTypeInfo <- getFunTypeInfo (varToFunName var)
      return $ ftiType funTypeInfo

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

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

-- | Convenient version of 'runReader' with the arguments flipped.
runReaderFrom :: r -> Reader r a -> a
runReaderFrom = flip runReader

