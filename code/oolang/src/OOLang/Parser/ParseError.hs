module OOLang.Parser.ParseError
  ( ParseError(..)
  , module OOLang.PrettyPrinter
  ) where

import Control.Monad.Error
import OOLang.SrcSpan()
import OOLang.PrettyPrinter

data ParseError =
    -- | Empty program error. Contains a file name.
    EmptyProgram String
    -- | General error. Contains a source position string.
  | GeneralError String

instance Error ParseError where
  strMsg = GeneralError

parseErrorHeader :: Doc
parseErrorHeader = text "OOLang parsing error at "

instance Pretty ParseError where
  prPrn (EmptyProgram fileName) = text fileName <+> text "contains an empty program"
  prPrn (GeneralError strPos) =
    parseErrorHeader <> text strPos

indLvl :: Int
indLvl = 2

