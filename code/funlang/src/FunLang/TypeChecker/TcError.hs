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
  | TypeNotDefined SrcTypeName
  | TypeConIncorrectApp SrcTypeName Kind Kind
  | ConAlreadyDefined SrcConName
  | ConNotDefined SrcConName
  | FunctionAlreadyDefined SrcFunName
  | MainNotDefined
  | MainIncorrectType SrcType
  | IllFormedType SrcType
  | TypeParamAlreadyDefined SrcTypeVar
  | FunEqIncorrectName SrcFunName FunName
  | FunEqBodyIncorrectType SrcExpr FunName Type Type
  | TypeVarApp SrcTypeVar
  | TypeVarShadowsType SrcTypeVar
  | TypeVarShadowsTypeVar SrcTypeVar
  | NotMonad SrcType
  | IncorrectFunArgType TyExpr Type Type
  | NotFunctionType TyExpr Type
  | VarNotBound Var SrcSpan
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

  prPrn (TypeNotDefined srcTypeName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeName) <> colon $+$
    nest indLvl (text "Type" <+> quotes (prPrn $ getTypeName srcTypeName) <+> text "is not defined")

  prPrn (TypeConIncorrectApp srcTypeName defKind useKind) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeName) <> colon $+$
    nest indLvl (text "Type constructor" <+> quotes (prPrn $ getTypeName srcTypeName) <+>
      text "has kind" <+> quotes (prPrn defKind) <> text ", but its usage assumes it has kind" <+> quotes (prPrn useKind))

  prPrn (ConAlreadyDefined srcConName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcConName) <> colon $+$
    nest indLvl (text "Data constructor" <+> quotes (prPrn $ getConName srcConName) <+> text "is already defined")

  prPrn (ConNotDefined srcConName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcConName) <> colon $+$
    nest indLvl (text "Data constructor" <+> quotes (prPrn $ getConName srcConName) <+> text "is not defined")

  prPrn (FunctionAlreadyDefined srcFunName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcFunName) <> colon $+$
    nest indLvl (text "Function" <+> quotes (prPrn $ getFunName srcFunName) <+> text "is already defined")

  prPrn MainNotDefined = tcErrorHeader <> colon <+> text "Function 'main' is not defined"

  prPrn (MainIncorrectType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Function 'main' has to have type" <+> quotes (prPrn (TyApp (TypeName "IO") [TyApp (TypeName "Unit") []])) <>
      text ", but it has type" <+> quotes (prPrn srcType))

  prPrn (IllFormedType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Type" <+> quotes (prPrn srcType) <+> text "is ill-formed")

  prPrn (TypeParamAlreadyDefined srcTypeVar) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeVar) <> colon $+$
    nest indLvl (text "Type parameter" <+> quotes (prPrn $ getTypeVar srcTypeVar) <+> text "is already defined")

  prPrn (FunEqIncorrectName funEqSrcName funName) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 funEqSrcName) <> colon $+$
    nest indLvl (text "Function equation has the name" <+> quotes (prPrn $ getFunName funEqSrcName) <>
      text ", but it should be" <+> quotes (prPrn funName))

  prPrn (FunEqBodyIncorrectType srcBodyExpr funName funType bodyType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcBodyExpr) <> colon $+$
    nest indLvl (text "Function equation body of the function" <+> quotes (prPrn funName) <+>
      text "has to have type" <+> quotes (prPrn funType) <> text ", but it has type" <+> quotes (prPrn bodyType))

  prPrn (TypeVarApp srcTypeVar) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeVar) <> colon $+$
    nest indLvl (text "Type variable" <+> quotes (prPrn $ getTypeVar srcTypeVar) <+> text "can not be applied (type variables have kind '*')")

  prPrn (TypeVarShadowsType srcTypeVar) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeVar) <> colon $+$
    nest indLvl (text "Type variable" <+> quotes (prPrn $ getTypeVar srcTypeVar) <+> text "shadows existing type")

  prPrn (TypeVarShadowsTypeVar srcTypeVar) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 srcTypeVar) <> colon $+$
    nest indLvl (text "Type variable" <+> quotes (prPrn $ getTypeVar srcTypeVar) <+> text "shadows another type variable")

  prPrn (NotMonad srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Type" <+> quotes (prPrn srcType) <+> text "is not a monad")

  prPrn (IncorrectFunArgType tyArgExpr expType actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 tyArgExpr) <> colon $+$
    nest indLvl (text "Incorrect type of the argument. The expected type is" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType))

  prPrn (NotFunctionType funExpr actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan2 funExpr) <> colon $+$
    nest indLvl (text "The expression needs to have a function type, but it has type" <+> quotes (prPrn actType))

  prPrn (VarNotBound var srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Name" <+> quotes (prPrn var) <+> text "is not bound")

  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

-- | Indentation level for error messages.
indLvl :: Int
indLvl = 2

