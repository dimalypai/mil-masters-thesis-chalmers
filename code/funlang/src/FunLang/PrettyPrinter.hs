module FunLang.PrettyPrinter
  ( module FunLang.PrettyPrinter
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

class Pretty a where
  prPrn :: a -> Doc

prPrint :: Pretty a => a -> String
prPrint = render . prPrn

