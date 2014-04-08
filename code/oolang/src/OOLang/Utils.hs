-- | Common utility functions.
module OOLang.Utils where

import Control.Monad (when, unless)

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

