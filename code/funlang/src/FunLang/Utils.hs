-- | Common utility functions.
module FunLang.Utils where

import Control.Monad.State
import Control.Monad.Reader

-- | Monadic version of 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM condM a = do
  cond <- condM
  when cond a

-- | Monadic version of 'unless'.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM a = do
  cond <- condM
  unless cond a

-- | Monadic version of if expression.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM condM th el = do
  cond <- condM
  if cond then th else el

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

-- | Convenient version of 'evalStateT' with the arguments flipped.
evalStateTFrom :: Monad m => s -> StateT s m a -> m a
evalStateTFrom = flip evalStateT

-- | Convenient version of 'runReader' with the arguments flipped.
runReaderFrom :: r -> Reader r a -> a
runReaderFrom = flip runReader

