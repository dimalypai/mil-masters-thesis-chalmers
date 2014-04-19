{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
-- This module mostly is for working with type environment (querying, adding).
-- Nothing smart should happen here.
module OOLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM

  , TypeEnv
  , initTypeEnv

  , ctiMSuperClassName
  , ctiSrcClassName
  , ctiMSuperSrcClassName
  , getClassesAssoc
  , isClassDefined
  , addClass
  , getSuperClass

  , ftiType
  , ftiSrcFunName
  , ftiSrcFunType
  , isFunctionDefined
  , addFunction
  , getFunTypeInfo

  , emptyLocalTypeEnv
  , isVarBound
  , isVarInLocalEnv
  , addLocalVar
  , locallyWithEnv

  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
-- 'first' and 'second' are used just to transform components of a pair
import Control.Arrow (first, second)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import OOLang.AST
import OOLang.TypeChecker.TcError

-- | Type checking monad. Uses 'StateT' for type environment and 'ErrorT' for
-- error handling.
newtype TypeCheckM a = TC { runTC :: StateT TypeCheckMState (ErrorT TcError Identity) a }
  deriving (Monad, MonadState TypeCheckMState, MonadError TcError, Functor, Applicative)

type TypeCheckMState = (TypeEnv, LocalTypeEnv)

-- | Main entry point to the type checking monad.
-- Get a type checking computation and a type environment to begin with.
runTypeCheckM :: TypeCheckM a -> TypeEnv -> Either TcError (a, TypeEnv)
runTypeCheckM tcm typeEnv = fmap dropLocalTypeEnv $
                            runIdentity $
                            runErrorT $
                            runStateTFrom (typeEnv, emptyLocalTypeEnv) $
                            runTC tcm

getTypeEnv :: TypeCheckMState -> TypeEnv
getTypeEnv = fst

getLocalTypeEnv :: TypeCheckMState -> LocalTypeEnv
getLocalTypeEnv = snd

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> TypeCheckM ()
modifyTypeEnv = modify . first

modifyLocalTypeEnv :: (LocalTypeEnv -> LocalTypeEnv) -> TypeCheckM ()
modifyLocalTypeEnv = modify . second

dropLocalTypeEnv :: (a, (TypeEnv, LocalTypeEnv))
                 -> (a, TypeEnv)
dropLocalTypeEnv = second getTypeEnv

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (ClassTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = mkTypeEnv Map.empty Map.empty

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: ClassTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv classTypeEnv funTypeEnv = TypeEnv (classTypeEnv, funTypeEnv)

-- Class type environment

type ClassTypeEnv = Map.Map ClassName ClassTypeInfo

-- | All the information about classes that we store in the type environment.
-- Some of the fields are kept just for error messages.
data ClassTypeInfo = ClassTypeInfo
  { ctiMSuperClassName    :: Maybe ClassName     -- ^ Name of the super class, if it has one.
  , ctiSrcClassName       :: SrcClassName        -- ^ Source name. For error messages.
  , ctiMSuperSrcClassName :: Maybe SrcClassName  -- ^ Super class source name. For error messages.
  }

-- | 'ClassTypeEnv' getter.
getClassTypeEnv :: TypeCheckM ClassTypeEnv
getClassTypeEnv = gets (fst . unTypeEnv . getTypeEnv)

-- | Get class type environment as an associative list.
getClassesAssoc :: TypeCheckM [(ClassName, ClassTypeInfo)]
getClassesAssoc = fmap Map.assocs getClassTypeEnv

-- | Modification function for 'ClassTypeEnv'.
modifyClassTypeEnv :: (ClassTypeEnv -> ClassTypeEnv) -> TypeCheckM ()
modifyClassTypeEnv f = do
  (classTypeEnv, funTypeEnv) <- (,) <$> getClassTypeEnv <*> getFunTypeEnv
  modifyTypeEnv (const $ mkTypeEnv (f classTypeEnv) funTypeEnv)

isClassDefined :: ClassName -> TypeCheckM Bool
isClassDefined className = do
  classTypeEnv <- getClassTypeEnv
  return $ Map.member className classTypeEnv

-- | Doesn't check if the class is already in the environment.
-- Will overwrite it in this case.
addClass :: SrcClassName -> Maybe SrcClassName -> TypeCheckM ()
addClass srcClassName mSuperSrcClassName = do
  let className = getClassName srcClassName
      mSuperClassName = getClassName <$> mSuperSrcClassName
  modifyClassTypeEnv $ Map.insert className (ClassTypeInfo mSuperClassName srcClassName mSuperSrcClassName)

-- | Returns a super class of the given class if it has one.
--
-- Note: Unsafe. Should be used only after check that class is defined.
getSuperClass :: ClassName -> TypeCheckM (Maybe ClassName)
getSuperClass className = do
  classTypeEnv <- getClassTypeEnv
  return $ ctiMSuperClassName $ fromJust $ Map.lookup className classTypeEnv  -- fromJust may fail

-- Function type environment

type FunTypeEnv = Map.Map FunName FunTypeInfo

-- | All the information about functions that we store in the type environment.
-- Some of the fields are kept just for error messages.
data FunTypeInfo = FunTypeInfo
  { ftiType       :: Type        -- ^ Function type.
  , ftiSrcFunName :: SrcFunName  -- ^ Source name. For error messages.
  , ftiSrcFunType :: SrcFunType  -- ^ Source type. For error messages.
  }

-- | 'FunTypeEnv' getter.
getFunTypeEnv :: TypeCheckM FunTypeEnv
getFunTypeEnv = gets (snd . unTypeEnv . getTypeEnv)

-- | Modification function for 'FunTypeEnv'.
modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (classTypeEnv, funTypeEnv) <- (,) <$> getClassTypeEnv <*> getFunTypeEnv
  modifyTypeEnv (const $ mkTypeEnv classTypeEnv (f funTypeEnv))

isFunctionDefined :: FunName -> TypeCheckM Bool
isFunctionDefined funName = do
  funTypeEnv <- getFunTypeEnv
  return $ Map.member funName funTypeEnv

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: SrcFunName -> Type ->SrcFunType -> TypeCheckM ()
addFunction srcFunName funType srcFunType = do
  let funName = getFunName srcFunName
  modifyFunTypeEnv $ Map.insert funName (FunTypeInfo funType srcFunName srcFunType)

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfo :: FunName -> TypeCheckM FunTypeInfo
getFunTypeInfo funName = do
  funTypeEnv <- getFunTypeEnv
  return $ fromJust $ Map.lookup funName funTypeEnv  -- fromJust may fail

-- Local type environment

-- | Local variables, parameters etc. and their types.
type LocalTypeEnv = Map.Map Var Type

emptyLocalTypeEnv :: LocalTypeEnv
emptyLocalTypeEnv = Map.empty

-- | Check whether a variable is in scope. It can be either local variable name
-- or a function name.
isVarBound :: Var -> TypeCheckM Bool
isVarBound var = do
  isLocalVar <- gets (Map.member var . getLocalTypeEnv)
  isFunction <- isFunctionDefined (varToFunName var)
  return (isLocalVar || isFunction)

-- | Pure function for querying local type environment.
isVarInLocalEnv :: Var -> LocalTypeEnv -> Bool
isVarInLocalEnv var = Map.member var

-- | Extends local type environment. Pure (meaning, not a 'TypeCheckM' function).
addLocalVar :: Var -> Type -> LocalTypeEnv -> LocalTypeEnv
addLocalVar = Map.insert

-- | Takes a separate local type environment and merges it with what is already
-- in the local type environment and performs a given computation in this new
-- environment. Then restores the local environment.
-- Should be safe, since we don't allow shadowing.
locallyWithEnv :: LocalTypeEnv -> TypeCheckM a -> TypeCheckM a
locallyWithEnv localTypeEnv tcm = do
  currentLocalTypeEnv <- gets getLocalTypeEnv
  modifyLocalTypeEnv (Map.union localTypeEnv)
  a <- tcm
  modifyLocalTypeEnv (const currentLocalTypeEnv)
  return a

-- Utils

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

