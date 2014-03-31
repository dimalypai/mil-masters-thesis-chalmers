module OOLang.PrettyPrinter
  ( module OOLang.PrettyPrinter
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

class Pretty a where
  prPrn :: a -> Doc

prPrint :: Pretty a => a -> String
prPrint = render . prPrn

