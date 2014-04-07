module MIL.PrettyPrinter
  ( module MIL.PrettyPrinter
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

class Pretty a where
  prPrn :: a -> Doc

prPrnParens :: Pretty a => Bool -> a -> Doc
prPrnParens True  = parens . prPrn
prPrnParens False = prPrn

prPrint :: Pretty a => a -> String
prPrint = render . prPrn

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

vsepBig :: [Doc] -> Doc
vsepBig = foldr (\d acc -> d $+$ text "" $+$ acc) empty

intersperse :: Doc -> [Doc] -> [Doc]
intersperse _   []     = []
intersperse p (x:xs) = x : go xs
  where go []     = []
        go (z:zs) = (p <> z) : go zs

