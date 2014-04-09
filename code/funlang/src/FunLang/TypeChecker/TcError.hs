-- | Module for type checking errors and their pretty printing.
module FunLang.TypeChecker.TcError
  ( TcError(..)
  , prPrint
  ) where

import Control.Monad.Error

import FunLang.AST
import FunLang.SrcSpan()
import FunLang.AST.SrcAnnotated
import FunLang.PrettyPrinter

-- | Data type containing all possible type checking errors and 'OtherError'.
data TcError =
  OtherError String  -- ^ Contains error message.

instance Error TcError where
  strMsg = OtherError

tcErrorHeader :: Doc
tcErrorHeader = text "FunLang type checking error"

-- | Error header for error that specify the source span.
tcErrorHeaderSpan :: Doc
tcErrorHeaderSpan = tcErrorHeader <+> text "at "

instance Pretty TcError where
  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

-- | Indentation level for error messages.
indLvl :: Int
indLvl = 2

