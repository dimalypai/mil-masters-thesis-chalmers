-- | Main pretty printing module. Defines an interface.
module MIL.PrettyPrinter
  ( module MIL.PrettyPrinter
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

-- | Pretty printing is used in outputing things to the user. For example, in
-- internal error messages, code output etc.  All data types that need to be
-- pretty printed need to be an instance of this class.
class Pretty a where
  prPrn :: a -> Doc

-- | Pretty-printing function that adds parentheses around depending on the
-- flag value. Convenient when dealing with precedences.
prPrnParens :: Pretty a => Bool -> a -> Doc
prPrnParens True  = parens . prPrn
prPrnParens False = prPrn

-- | Top-level pretty-printing function.
prPrint :: Pretty a => a -> String
prPrint = render . prPrn

-- Combining documents

-- | Missing from "Text.PrettyPrint". List version of '$+$'.
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- | 'vsep' with one more line in between documents.
vsepBig :: [Doc] -> Doc
vsepBig = foldr (\d acc -> d $+$ text "" $+$ acc) empty

-- | Prepends a document given as a first argument to all elements of the list
-- except for head.
intersperse :: Doc -> [Doc] -> [Doc]
intersperse _   []     = []
intersperse p (x:xs) = x : go xs
  where go []     = []
        go (z:zs) = (p <> z) : go zs

