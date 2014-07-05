-- | Common utility functions.
module MIL.Utils where

import Control.Monad (when, unless, liftM, filterM)
import Data.Maybe (listToMaybe)

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

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p xs = liftM listToMaybe (filterM p xs)

