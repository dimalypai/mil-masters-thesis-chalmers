module FunLang.Parser.ParseError
  ( ParseError(..)
  , module FunLang.PrettyPrinter
  ) where

import Control.Monad.Error
import FunLang.SrcSpan
import FunLang.PrettyPrinter

data ParseError = EmptyProgram String
                | TypeDefLowerId String SrcSpan
                | TypeDefNoCons String SrcSpan
                | GeneralError String  -- source position string

instance Error ParseError where
  strMsg = GeneralError

parseErrorHeader :: Doc
parseErrorHeader = text "FunLang parsing error at "

instance Pretty ParseError where
  prPrn (EmptyProgram fileName) = text fileName <+> text "contains an empty program"
  prPrn (TypeDefLowerId typeNameLower ss) =
    (parseErrorHeader <> prPrn ss <> colon) $$
    (nest indLvl $ text "Type name must begin with a capital letter:" <+> text typeNameLower)
  prPrn (TypeDefNoCons typeName ss) =
    (parseErrorHeader <> prPrn ss <> colon) $$
    (nest indLvl $ text "Definition of type" <+> text typeName <+> text "doesn't have any constructors")
  prPrn (GeneralError strPos) =
    parseErrorHeader <> text strPos

indLvl :: Int
indLvl = 2

