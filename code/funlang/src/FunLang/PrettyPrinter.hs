-- | Main pretty printing module. Defines an interface.
module FunLang.PrettyPrinter
  ( module FunLang.PrettyPrinter
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

-- | Pretty printing is used in outputing things to the user. For example, in
-- error messages, code output etc.  All data types that need to be pretty
-- printed need to be an instance of this class.
class Pretty a where
  prPrn :: a -> Doc

-- | Top-level pretty-printing function.
prPrint :: Pretty a => a -> String
prPrint = render . prPrn

