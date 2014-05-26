-- | Module for type checking errors and their pretty printing.
module MIL.TypeChecker.TcError
  ( TcError(..)
  , prPrint
  ) where

import Control.Monad.Error

import MIL.AST
import MIL.AST.PrettyPrinter()
import MIL.PrettyPrinter

-- | Data type containing all possible type checking errors and 'OtherError'.
data TcError =
    TypeAlreadyDefined TypeName
  | TypeParamAlreadyDefined TypeVar
  | TypeOrAliasAlreadyDefined TypeName
  | FunctionAlreadyDefined FunName
  | ConAlreadyDefined ConName
  | TypeNotDefined TypeName
  | TypeConIncorrectApp TypeName Kind Kind
  | TypeVarNotInScope TypeVar
  | TypeVarShadowsType TypeVar
  | TypeVarShadowsTypeVar TypeVar
  | TypeVarApp TypeVar
  | IllFormedType Type
  | FunBodyIncorrectType FunName Type Type
  | VarNotBound Var
  | VarIncorrectType Var Type Type
  | VarShadowing Var
  | IncorrectFunArgType Type Type
  | NotFunctionType Type
  | NotForallTypeApp Type
  | ConNotDefined ConName
  | ConIncorrectType ConName Type Type
  | NotMonadicType Type
  | IncorrectExprType Type Type
  | IncorrectMonad TypeM TypeM
  | IncorrectLifting TypeM TypeM
  {-
  | MainNotDefined
  | MainIncorrectType SrcType
  | FunEqIncorrectName SrcFunName FunName
  | DoBlockNotMonad Type SrcSpan
  | BindLastStmt SrcSpan
  | CaseAltIncorrectType Type Type SrcSpan
  | PatternIncorrectType Type Type SrcSpan
  | ConPatternIncorrectNumberOfFields Int Int SrcSpan-}
  | OtherError String  -- ^ Contains error message.

instance Error TcError where
  strMsg = OtherError

tcErrorHeader :: Doc
tcErrorHeader = text "MIL type checking error:"

instance Pretty TcError where
  prPrn (TypeAlreadyDefined typeName) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn typeName) <+> text "is already defined"

  prPrn (TypeParamAlreadyDefined typeVar) =
    tcErrorHeader <+> text "Type parameter" <+> quotes (prPrn typeVar) <+> text "is already defined"

  prPrn (TypeOrAliasAlreadyDefined typeName) =
    tcErrorHeader <+> text "Type or type alias" <+> quotes (prPrn typeName) <+> text "is already defined"

  prPrn (FunctionAlreadyDefined funName) =
    tcErrorHeader <+> text "Function" <+> quotes (prPrn funName) <+> text "is already defined"

  prPrn (ConAlreadyDefined conName) =
    tcErrorHeader <+> text "Data constructor" <+> quotes (prPrn conName) <+> text "is already defined"

  prPrn (TypeNotDefined typeName) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn typeName) <+> text "is not defined"

  prPrn (TypeConIncorrectApp typeName defKind useKind) =
    tcErrorHeader <+> text "Type constructor" <+> quotes (prPrn typeName) <+>
      text "has kind" <+> quotes (prPrn defKind) <> text ", but its usage assumes it has kind" <+> quotes (prPrn useKind)

  prPrn (TypeVarNotInScope typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "is not in scope"

  prPrn (TypeVarShadowsType typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "shadows existing type or type alias"

  prPrn (TypeVarShadowsTypeVar typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "shadows another type variable"

  prPrn (TypeVarApp typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "can not be applied (type variables have kind '*')"

  prPrn (IllFormedType t) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn t) <+> text "is ill-formed"

  prPrn (FunBodyIncorrectType funName funType bodyType) =
    tcErrorHeader <+> text "Function body of the function" <+> quotes (prPrn funName) <+>
      text "has to have type" <+> quotes (prPrn funType) <> text ", but it has type" <+> quotes (prPrn bodyType)

  prPrn (VarNotBound var) =
    tcErrorHeader <+> text "Name" <+> quotes (prPrn var) <+> text "is not bound"

  prPrn (VarIncorrectType var expType actType) =
    tcErrorHeader <+> text "Name" <+> quotes (prPrn var) <+> text "has type" <+> quotes (prPrn expType) <+>
      text "in the environment, but it is annotated with type" <+> quotes (prPrn actType)

  prPrn (VarShadowing var) =
    tcErrorHeader <+> text "Variable binding for" <+> quotes (prPrn var) <+> text "shadows an existing variable or function"

  prPrn (IncorrectFunArgType expType actType) =
    tcErrorHeader <+> text "Incorrect type of the argument. The expected type is" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType)

  prPrn (NotFunctionType actType) =
    tcErrorHeader <+> text "The expression needs to have a function type, but it has type" <+> quotes (prPrn actType)

  prPrn (NotForallTypeApp appType) =
    tcErrorHeader <+> text "The expression needs to have a polymorphic (forall) type, but it has type" <+> quotes (prPrn appType)

  prPrn (ConNotDefined conName) =
    tcErrorHeader <+> text "Data constructor" <+> quotes (prPrn conName) <+> text "is not defined"

  prPrn (ConIncorrectType conName expType actType) =
    tcErrorHeader <+> text "Data constructor" <+> quotes (prPrn conName) <+> text "has type" <+> quotes (prPrn expType) <+>
      text "in the environment, but it is annotated with type" <+> quotes (prPrn actType)

  prPrn (NotMonadicType exprType) =
    tcErrorHeader <+> text "The expression needs to have a monadic type, but it has type" <+> quotes (prPrn exprType)

  prPrn (IncorrectExprType expType actType) =
    tcErrorHeader <+> text "The expression needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType)

  prPrn (IncorrectMonad expType actType) =
    tcErrorHeader <+> text "The expression needs to operate in the" <+> quotes (prPrn expType) <+>
      text "monad, but it operates in the" <+> quotes (prPrn actType) <+> text "monad"

  prPrn (IncorrectLifting tm1 tm2) =
    tcErrorHeader <+> text "Incorrect lifting. Can not lift from" <+> quotes (prPrn tm1) <+> text "to" <+> quotes (prPrn tm2) <> text "."
{-
  prPrn MainNotDefined = tcErrorHeader <> colon <+> text "Function 'main' is not defined"

  prPrn (MainIncorrectType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Function 'main' has to have type" <+> quotes (prPrn (TyApp (TypeName "IO") [TyApp (TypeName "Unit") []])) <>
      text ", but it has type" <+> quotes (prPrn srcType))

  prPrn (DoBlockNotMonad t srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Do-blocks can be used only in functions with monadic types. Type" <+> quotes (prPrn t) <+> text "is not monadic")

  prPrn (BindLastStmt srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Bind can not be the last statement in the do-block. The last statement has to return a value")

  prPrn (CaseAltIncorrectType expType actType srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "The case alternative expression needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType))

  prPrn (PatternIncorrectType expType actType srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "The pattern needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType))

  prPrn (ConPatternIncorrectNumberOfFields expNum actNum srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "The constructor pattern needs to have" <+> int expNum <+> text "argument(s)" <>
      text ", but it was given" <+> int actNum)
-}
  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

