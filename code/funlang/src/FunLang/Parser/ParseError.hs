module FunLang.Parser.ParseError
  ( ParseError(..)
  , module FunLang.PrettyPrinter
  ) where

import Control.Monad.Error
import FunLang.PrettyPrinter

data ParseError = EmptyProgram
                | GeneralError String  -- source position string

instance Error ParseError where
  strMsg = GeneralError

parseErrorHeader :: Doc
parseErrorHeader = text "FunLang parsing error:"

instance Pretty ParseError where
  prPrn EmptyProgram = parseErrorHeader <+> text "empty program"
  prPrn (GeneralError strPos) =
    parseErrorHeader <> text strPos

