module FunLang.PrettyPrinter where

import Text.PrettyPrint

class Pretty a where
  prPrint :: a -> Doc

