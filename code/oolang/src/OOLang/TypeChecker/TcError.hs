module OOLang.TypeChecker.TcError
  ( TcError(..)
  , prPrint
  ) where

import Control.Monad.Error

import OOLang.SrcSpan
import OOLang.PrettyPrinter

data TcError = OtherError String

instance Error TcError where
  strMsg = OtherError

tcErrorHeader :: Doc
tcErrorHeader = text "OOLang type checking error"

tcErrorHeaderSpan :: Doc
tcErrorHeaderSpan = tcErrorHeader <+> text "at "

instance Pretty TcError where
  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

indLvl :: Int
indLvl = 2

