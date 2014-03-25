module FunLang.DebugShow
  ( module FunLang.DebugShow
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

class DebugShow a where
  showDebug :: a -> Doc

instance DebugShow a => DebugShow [a] where
  showDebug = brackets . vcat . punctuate comma . map showDebug

instance (DebugShow s, DebugShow a) => DebugShow (s, a) where
  showDebug (s, a) = showDebug a <+> showDebug s

renderDebug :: DebugShow a => a -> String
renderDebug = render . showDebug

indLvl :: Int
indLvl = 2

