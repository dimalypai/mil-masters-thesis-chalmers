-- | Module for type checking errors and their pretty printing.
module FunLang.TypeChecker.TcError
  ( TcError(..)
  , prPrint
  ) where

import Control.Monad.Error

import FunLang.AST
import FunLang.SrcSpan
import FunLang.AST.SrcAnnotated
import FunLang.AST.PrettyPrinter()
import FunLang.PrettyPrinter

-- | Data type containing all possible type checking errors and 'OtherError'.
data TcError =
    TypeAlreadyDefined SrcTypeName
  | FunctionAlreadyDefined SrcFunName
  | MainNotDefined
  | MainWrongType SrcType
  | IllFormedType SrcType
  | OtherError String  -- ^ Contains error message.

instance Error TcError where
  strMsg = OtherError

tcErrorHeader :: Doc
tcErrorHeader = text "FunLang type checking error"

-- | Error header for error that specify the source span.
tcErrorHeaderSpan :: Doc
tcErrorHeaderSpan = tcErrorHeader <+> text "at "

instance Pretty TcError where
  prPrn (TypeAlreadyDefined srcTypeName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeName) <> colon $+$
    nest indLvl (text "Type" <+> quotes (prPrn $ getTypeName srcTypeName) <+> text "is already defined")

  prPrn (FunctionAlreadyDefined srcFunName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcFunName) <> colon $+$
    nest indLvl (text "Function" <+> quotes (prPrn $ getFunName srcFunName) <+> text "is already defined")

  prPrn MainNotDefined = tcErrorHeader <> colon <+> text "Function 'main' is not defined"

  prPrn (MainWrongType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Function 'main' has to have type" <+> quotes (prPrn (TyApp (TypeName "IO") [TyApp (TypeName "Unit") []])) <>
      text ", but it has type" <+> quotes (prPrn srcType))

  prPrn (IllFormedType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Type" <+> quotes (prPrn srcType) <+> text "is ill-formed")

  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

-- | Indentation level for error messages.
indLvl :: Int
indLvl = 2

