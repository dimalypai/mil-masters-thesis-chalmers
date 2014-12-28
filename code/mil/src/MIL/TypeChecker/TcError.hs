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
  | MainNotDefined FunName
  | FunctionAlreadyDefined FunName
  | ConAlreadyDefined ConName
  | TypeNotDefined TypeName
  | TypeConIncorrectApp TypeName Kind Kind
  | TypeIncorrectKind Type Kind Kind
  | SrcTypeIncorrectKind SrcType Kind Kind
  | TypeVarNotInScope TypeVar
  | TypeParamShadowsType TypeVar
  | TypeVarShadowsType TypeVar
  | TypeVarShadowsTypeVar TypeVar
  | TypeVarApp TypeVar
  | IllFormedType Type
  | IllFormedSrcType SrcType
  | FunBodyIncorrectType FunName Type Type
  | VarNotBound Var
  | VarIncorrectType Var Type Type
  | VarShadowing Var
  | IncorrectFunArgType Type Type
  | NotFunctionType Type
  | NotForallTypeApp Type
  | NotMonadicType Type
  | NotMonadicSrcType SrcType
  | ConNotDefined ConName
  | ConIncorrectType ConName Type Type
  | ExprHasNonMonadicType Type
  | IncorrectExprType Type Type
  | IncorrectMonad MonadType MonadType
  | IncorrectLifting MonadType MonadType
  | CaseAltIncorrectType Type Type
  | PatternIncorrectType Type Type
  | ConPatternIncorrectNumberOfFields Int Int
  | PatternTupleType Type
  | TuplePatternIncorrectNumberOfElements Int Int
  | MonadConsOnTheLeft SrcType
  | OtherError String  -- ^ Contains error message.
  deriving (Show, Eq)

instance Error TcError where
  strMsg = OtherError

tcErrorHeader :: Doc
tcErrorHeader = text "MIL type checking error:"

instance Pretty TcError where
  prPrn (TypeAlreadyDefined typeName) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn typeName) <+> text "is already defined"

  prPrn (TypeParamAlreadyDefined typeVar) =
    tcErrorHeader <+> text "Type parameter" <+> quotes (prPrn typeVar) <+> text "is already defined"

  prPrn (MainNotDefined funName) =
    tcErrorHeader <+> text "Function" <+> quotes (prPrn funName) <+> text "is not defined"

  prPrn (FunctionAlreadyDefined funName) =
    tcErrorHeader <+> text "Function" <+> quotes (prPrn funName) <+> text "is already defined"

  prPrn (ConAlreadyDefined conName) =
    tcErrorHeader <+> text "Data constructor" <+> quotes (prPrn conName) <+> text "is already defined"

  prPrn (TypeNotDefined typeName) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn typeName) <+> text "is not defined"

  prPrn (TypeConIncorrectApp typeName defKind useKind) =
    tcErrorHeader <+> text "Type constructor" <+> quotes (prPrn typeName) <+>
      text "has kind" <+> quotes (prPrn defKind) <> text ", but its usage assumes it has kind" <+> quotes (prPrn useKind)

  prPrn (TypeIncorrectKind t defKind useKind) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn t) <+>
      text "has kind" <+> quotes (prPrn defKind) <> text ", but its usage assumes it has kind" <+> quotes (prPrn useKind)

  prPrn (SrcTypeIncorrectKind st defKind useKind) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn st) <+>
      text "has kind" <+> quotes (prPrn defKind) <> text ", but its usage assumes it has kind" <+> quotes (prPrn useKind)

  prPrn (TypeVarNotInScope typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "is not in scope"

  prPrn (TypeParamShadowsType typeVar) =
    tcErrorHeader <+> text "Type parameter" <+> quotes (prPrn typeVar) <+> text "shadows existing type"

  prPrn (TypeVarShadowsType typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "shadows existing type"

  prPrn (TypeVarShadowsTypeVar typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "shadows another type variable"

  prPrn (TypeVarApp typeVar) =
    tcErrorHeader <+> text "Type variable" <+> quotes (prPrn typeVar) <+> text "can not be applied (type variables have kind '*')"

  prPrn (IllFormedType t) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn t) <+> text "is ill-formed"

  prPrn (IllFormedSrcType st) =
    tcErrorHeader <+> text "Type" <+> quotes (prPrn st) <+> text "is ill-formed"

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

  prPrn (NotMonadicType t) =
    tcErrorHeader <+> text "Type" <+> prPrn t <+> text "needs to be monadic"

  prPrn (NotMonadicSrcType st) =
    tcErrorHeader <+> text "Type" <+> prPrn st <+> text "needs to be monadic"

  prPrn (ConNotDefined conName) =
    tcErrorHeader <+> text "Data constructor" <+> quotes (prPrn conName) <+> text "is not defined"

  prPrn (ConIncorrectType conName expType actType) =
    tcErrorHeader <+> text "Data constructor" <+> quotes (prPrn conName) <+> text "has type" <+> quotes (prPrn expType) <+>
      text "in the environment, but it is annotated with type" <+> quotes (prPrn actType)

  prPrn (ExprHasNonMonadicType exprType) =
    tcErrorHeader <+> text "The expression needs to have a monadic type, but it has type" <+> quotes (prPrn exprType)

  prPrn (IncorrectExprType expType actType) =
    tcErrorHeader <+> text "The expression needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType)

  prPrn (IncorrectMonad expType actType) =
    tcErrorHeader <+> text "The expression needs to operate in the" <+> quotes (prPrn expType) <+>
      text "monad, but it operates in the" <+> quotes (prPrn actType) <+> text "monad"

  prPrn (IncorrectLifting tm1 tm2) =
    tcErrorHeader <+> text "Incorrect lifting. Can not lift from" <+> quotes (prPrn tm1) <+> text "to" <+> quotes (prPrn tm2) <> text "."

  prPrn (CaseAltIncorrectType expType actType) =
    tcErrorHeader <+> text "The case alternative expression needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType)

  prPrn (PatternIncorrectType expType actType) =
    tcErrorHeader <+> text "The pattern needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType)

  prPrn (ConPatternIncorrectNumberOfFields expNum actNum) =
    tcErrorHeader <+> text "The constructor pattern needs to have" <+> int expNum <+> text "argument(s)" <>
      text ", but it was given" <+> int actNum

  prPrn (PatternTupleType expType) =
    tcErrorHeader <+> text "The pattern needs to have type" <+> quotes (prPrn expType) <>
      text ", but it has tuple type"

  prPrn (TuplePatternIncorrectNumberOfElements expNum actNum) =
    tcErrorHeader <+> text "The tuple pattern needs to have" <+> int expNum <+> text "element(s)" <>
      text ", but it has" <+> int actNum

  prPrn (MonadConsOnTheLeft st) =
    tcErrorHeader <+> text "Monad cons cannot contain another monad cons as a left component: " <+> prPrn st

  prPrn (OtherError errMsg) = tcErrorHeader <+> text errMsg

