-- | Module for parsing errors and their pretty printing.
module FunLang.Parser.ParseError
  ( ParseError(..)
  , prPrint
  ) where

import Control.Monad.Error
import FunLang.SrcSpan
import FunLang.PrettyPrinter

-- | All specific parsing errors go here.
-- Non-specific parsing errors are represented using 'GeneralError'.
data ParseError =
    -- | Empty program error. Contains a file name.
    EmptyProgram String
    -- | Lower case instead of upper case type name.
    -- Contains a type name (starting with a lower case letter) and a source span of the name.
  | TypeDefLowerId String SrcSpan
    -- | Zero constructors in the type definition.
    -- Contains a type name and a source span of the type definition.
  | TypeDefNoCons String SrcSpan
    -- | General error. Contains a source position string.  
  | GeneralError String

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

-- | Indentation level for error messages.
indLvl :: Int
indLvl = 2

