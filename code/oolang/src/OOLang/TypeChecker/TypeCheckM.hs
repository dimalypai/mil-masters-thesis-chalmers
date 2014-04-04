{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
module OOLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM
  , TypeEnv
  , initTypeEnv
  , mkTypeEnv
  , FunTypeEnv
  , getFunTypeEnv
  , modifyFunTypeEnv
  , ClassTypeEnv
  , getClassTypeEnv
  , modifyClassTypeEnv
  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative

import OOLang.AST
import OOLang.TypeChecker.TcError

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
newtype TypeEnv = TypeEnv { unTypeEnv :: (FunTypeEnv, ClassTypeEnv) }

type FunTypeEnv = [FunName]
type ClassTypeEnv = [ClassName]

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv ([], [])

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: FunTypeEnv -> ClassTypeEnv -> TypeEnv
mkTypeEnv funTypeEnv classTypeEnv = TypeEnv (funTypeEnv, classTypeEnv)

-- | 'FunTypeEnv' getter.
getFunTypeEnv :: TypeCheckM FunTypeEnv
getFunTypeEnv = gets (fst . unTypeEnv)

-- | Modification function for 'FunTypeEnv'.
modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (funTypeEnv, classTypeEnv) <- (,) <$> getFunTypeEnv <*> getClassTypeEnv
  put $ mkTypeEnv (f funTypeEnv) classTypeEnv

-- | 'ClassTypeEnv' getter.
getClassTypeEnv :: TypeCheckM ClassTypeEnv
getClassTypeEnv = gets (snd . unTypeEnv)

-- | Modification function for 'ClassTypeEnv'.
modifyClassTypeEnv :: (ClassTypeEnv -> ClassTypeEnv) -> TypeCheckM ()
modifyClassTypeEnv f = do
  (funTypeEnv, classTypeEnv) <- (,) <$> getFunTypeEnv <*> getClassTypeEnv
  put $ mkTypeEnv funTypeEnv (f classTypeEnv)

-- Utils

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

