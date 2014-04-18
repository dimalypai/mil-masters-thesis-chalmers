-- | Module for type checking errors and their pretty printing.
module OOLang.TypeChecker.TcError
  ( TcError(..)
  , prPrint
  ) where

import Control.Monad.Error

import OOLang.AST
import OOLang.AST.PrettyPrinter
import OOLang.SrcSpan()
import OOLang.AST.SrcAnnotated
import OOLang.PrettyPrinter

-- | Data type containing all possible type checking errors and 'OtherError'.
data TcError =
    ClassAlreadyDefined SrcClassName
  | ClassNotDefined SrcClassName
  | FunctionAlreadyDefined SrcFunName
  | MainNotDefined
  | MainIncorrectType SrcFunType Type
  | MainPure SrcFunName
  | InheritanceCycle
  | VarShadowing SrcVar
  | FunctionNotPure SrcFunName SrcStmt
  | OtherError String  -- ^ Contains error message.

instance Error TcError where
  strMsg = OtherError

tcErrorHeader :: Doc
tcErrorHeader = text "OOLang type checking error"

-- | Error header for error that specify the source span.
tcErrorHeaderSpan :: Doc
tcErrorHeaderSpan = tcErrorHeader <+> text "at "

instance Pretty TcError where
  prPrn (ClassAlreadyDefined srcClassName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcClassName) <> colon $+$
    nest indLvl (text "Class" <+> quotes (prPrn $ getClassName srcClassName) <+> text "is already defined")

  prPrn (ClassNotDefined srcClassName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcClassName) <> colon $+$
    nest indLvl (text "Class" <+> quotes (prPrn $ getClassName srcClassName) <+> text "is not defined")

  prPrn (FunctionAlreadyDefined srcFunName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcFunName) <> colon $+$
    nest indLvl (text "Function" <+> quotes (prPrn $ getFunName srcFunName) <+> text "is already defined")

  prPrn MainNotDefined = tcErrorHeader <> colon <+> text "Function 'main' is not defined"

  prPrn (MainIncorrectType srcFunType funType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcFunType) <> colon $+$
    nest indLvl (text "Function 'main' has to have type" <+> quotes (prPrn TyUnit) <>
      text ", but it has type" <+> quotes (prPrn funType))

  prPrn (MainPure srcFunName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcFunName) <> colon $+$
    nest indLvl (text "Function 'main' is declared as pure")

  prPrn InheritanceCycle = tcErrorHeader <> colon <+> text "Inheritance cycle was detected"

  prPrn (VarShadowing srcVar) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcVar) <> colon $+$
    nest indLvl (text "Variable binding for" <+> quotes (prPrn $ getVar srcVar) <+> text "shadows an existing variable or function")

  prPrn (FunctionNotPure srcFunName srcStmt) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcStmt) <> colon $+$
    nest indLvl (text "Function" <+> quotes (prPrn $ getFunName srcFunName) <+>
      text "is declared as pure, but it contains impure statement")

  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

-- | Indentation level for error messages.
indLvl :: Int
indLvl = 2

