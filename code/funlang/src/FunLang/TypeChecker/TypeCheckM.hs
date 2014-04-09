{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
module FunLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM
  , TypeEnv
  , initTypeEnv
  , mkTypeEnv
  , DataTypeEnv
  , FunTypeEnv
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
newtype TypeEnv = TypeEnv { unTypeEnv :: (DataTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv (Map.empty, Map.empty)

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: DataTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv dataTypeEnv funTypeEnv = TypeEnv (dataTypeEnv, funTypeEnv)

-- Data type environment

type DataTypeEnv = Map.Map TypeName TypeName

-- Function type environment

type FunTypeEnv = Map.Map FunName FunName

-- Utils

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

