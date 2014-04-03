{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OOLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM
  , TypeEnv
  , emptyTypeEnv
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

--newtype TypeCheckM a = TC { runTC :: ErrorT TcError (State TypeEnv) a }
newtype TypeCheckM a = TC { runTC :: StateT TypeEnv (ErrorT TcError Identity) a }
  deriving (Monad, MonadState TypeEnv, MonadError TcError, Functor, Applicative)

runTypeCheckM :: TypeCheckM a -> TypeEnv -> Either TcError (a, TypeEnv)
runTypeCheckM tcm typeEnv = runIdentity $
                            runErrorT $
                            runStateTFrom typeEnv $
                            runTC tcm

newtype TypeEnv = TypeEnv { unTypeEnv :: (FunTypeEnv, ClassTypeEnv) }

type FunTypeEnv = [FunName]
type ClassTypeEnv = [ClassName]

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv ([], [])

mkTypeEnv :: FunTypeEnv -> ClassTypeEnv -> TypeEnv
mkTypeEnv funTypeEnv classTypeEnv = TypeEnv (funTypeEnv, classTypeEnv)

getFunTypeEnv :: TypeCheckM FunTypeEnv
getFunTypeEnv = gets (fst . unTypeEnv)

modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (funTypeEnv, classTypeEnv) <- (,) <$> getFunTypeEnv <*> getClassTypeEnv
  put $ mkTypeEnv (f funTypeEnv) classTypeEnv

getClassTypeEnv :: TypeCheckM ClassTypeEnv
getClassTypeEnv = gets (snd . unTypeEnv)

modifyClassTypeEnv :: (ClassTypeEnv -> ClassTypeEnv) -> TypeCheckM ()
modifyClassTypeEnv f = do
  (funTypeEnv, classTypeEnv) <- (,) <$> getFunTypeEnv <*> getClassTypeEnv
  put $ mkTypeEnv funTypeEnv (f classTypeEnv)

-- Utils

runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

