-- | Module for type checking errors and their pretty printing.
module OOLang.TypeChecker.TcError
  ( TcError(..)
  , prPrint
  ) where

import Control.Monad.Error

import OOLang.AST
import OOLang.AST.Helpers
import OOLang.AST.PrettyPrinter
import OOLang.SrcSpan
import OOLang.AST.SrcAnnotated
import OOLang.PrettyPrinter

-- | Data type containing all possible type checking errors and 'OtherError'.
data TcError =
    ClassAlreadyDefined SrcClassName
  | ClassNotDefined SrcClassName
  | FunctionAlreadyDefined SrcFunName
  | MainNotDefined
  | MainIncorrectType SrcFunType Type
  | InheritanceCycle
  | VarShadowing SrcVar
  | VarNotBound Var SrcSpan
  | FunctionNotPure SrcFunName TyStmt
  | FunIncorrectReturnType SrcFunName TyStmt Type Type
  | IncorrectFunArgType TyExpr Type Type
  | NotFunctionType TyExpr Type
  | PureValue SrcType
  | MutableOrRefNested SrcType
  | PureFunParam SrcType
  | MutableFunParam SrcType
  | MutableFunReturnType SrcType
  | DeclInitIncorrectType SrcInit Type Type
  | AssignIncorrectType TyExpr Type Type
  | IncorrectImmutableOpUsage SrcSpan
  | IncorrectMutableOpUsage SrcSpan
  | IncorrectRefOpUsage SrcSpan
  | NonMaybeVarNotInit Var SrcSpan
  | IncorrectNothingType SrcType
  | SelfMemberName SrcSpan
  | SuperMemberName SrcSpan
  | NewMemberName SrcSpan
  | FieldInitNotPure Var SrcSpan
  | MemberAlreadyDefined MemberName SrcSpan
  | NotMember MemberName ClassName SrcExpr
  | NotObject SrcExpr Type
  | MemberAccessWithMaybe SrcExpr Type
  | ClassAccessNotNew SrcSpan
  | AssignToFunction SrcSpan
  | AssignNotField Var ClassName SrcSpan
  | IncorrectAssignLeft SrcSpan
  | OutsideFieldAccess SrcSpan
  | NestedRefCreation SrcSpan
  | NonRefDeref SrcSpan
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

  prPrn InheritanceCycle = tcErrorHeader <> colon <+> text "Inheritance cycle was detected"

  prPrn (VarShadowing srcVar) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcVar) <> colon $+$
    nest indLvl (text "Variable binding for" <+> quotes (prPrn $ getVar srcVar) <+> text "shadows an existing variable or function")

  prPrn (VarNotBound var srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Name" <+> quotes (prPrn var) <+> text "is not bound")

  prPrn (FunctionNotPure srcFunName tyStmt) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan tyStmt) <> colon $+$
    nest indLvl (text "Function/method" <+> quotes (prPrn $ getFunName srcFunName) <+>
      text "has Pure return type, but it contains an impure statement")

  prPrn (FunIncorrectReturnType srcFunName tyStmt expType actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan tyStmt) <> colon $+$
    nest indLvl (text "Function/method" <+> quotes (prPrn $ getFunName srcFunName) <+>
      text "has to return a value of type" <+> quotes (prPrn expType) <>
      text ", but it returns a value of type" <+> quotes (prPrn actType))

  prPrn (IncorrectFunArgType tyArgExpr expType actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan tyArgExpr) <> colon $+$
    nest indLvl (text "Incorrect type of the argument. The expected type is" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType))

  prPrn (NotFunctionType funExpr actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan funExpr) <> colon $+$
    nest indLvl (text "The expression has to have a function type, but it has type" <+> quotes (prPrn actType))

  prPrn (PureValue srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Pure type can be used only in the return type of a function/method")

  prPrn (MutableOrRefNested srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Incorrect nesting of Maybe/Mutable/Ref types")

  prPrn (PureFunParam srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Function/method parameter can not have Pure type")

  prPrn (MutableFunParam srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Function/method parameter can not have Mutable type")

  prPrn (MutableFunReturnType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "Function/method can not have Mutable return type")

  prPrn (DeclInitIncorrectType srcInit expType actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcInit) <> colon $+$
    nest indLvl (text "Incorrect type of initialisation expression. The expected type is" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType))

  prPrn (AssignIncorrectType tyExpr expType actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan tyExpr) <> colon $+$
    nest indLvl (text "Incorrect type of assignable expression. The expected type is" <+> quotes (prPrn expType) <>
      text ", but it has type" <+> quotes (prPrn actType))

  prPrn (IncorrectImmutableOpUsage srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Incorrect usage of '=' operator. It can be used only with immutable (default) and Ref variables/fields")

  prPrn (IncorrectMutableOpUsage srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Incorrect usage of '<-' operator. It can be used only with Mutable variables/fields")

  prPrn (IncorrectRefOpUsage srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Incorrect usage of ':=' operator. It can be used only with Ref variables/fields")

  prPrn (NonMaybeVarNotInit var srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Variable/field" <+> quotes (prPrn var) <+> text "with non-Maybe type is not initialised")

  prPrn (IncorrectNothingType srcType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcType) <> colon $+$
    nest indLvl (text "'nothing' literal has to have Maybe type, but it has type" <+> quotes (prPrn srcType))

  prPrn (SelfMemberName srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Class member can not use a special name 'self'")

  prPrn (SuperMemberName srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Class member can not use a special name 'super'")

  prPrn (NewMemberName srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Class member can not use a special name 'new'")

  prPrn (FieldInitNotPure fieldName srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Initialisation expression of the field" <+> quotes (prPrn fieldName) <+> text "is not pure")

  prPrn (MemberAlreadyDefined memberName srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Class member with the name" <+> quotes (prPrn memberName) <+> text "is already defined")

  prPrn (NotMember memberName className srcExpr) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcExpr) <> colon $+$
    nest indLvl (text "Class member with the name" <+> quotes (prPrn memberName) <+>
      text "is not defined in the class" <+> quotes (prPrn className))

  prPrn (NotObject srcExpr actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcExpr) <> colon $+$
    nest indLvl (text "The expression has to be an object (have a class type), but it has type" <+> quotes (prPrn actType))

  prPrn (MemberAccessWithMaybe srcExpr actType) =
    tcErrorHeaderSpan <> prPrn (getSrcSpan srcExpr) <> colon $+$
    nest indLvl (text "Incorrect usage of the member access operator (`.`) with the expression of type" <+> quotes (prPrn actType) <>
      text ". Try using the `?` operator")

  prPrn (ClassAccessNotNew srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Class access can only be used for object construction with 'new'")

  prPrn (AssignToFunction srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Global function can not be a left-hand side of the assignment")

  prPrn (AssignNotField fieldName className srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "There is no field with the name" <+> quotes (prPrn fieldName) <+> text "in the class" <+> quotes (prPrn className) <>
      text ". Method can not be a left-hand side of the assignment")

  prPrn (IncorrectAssignLeft srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Incorrect left-hand side of the assignment. It can only be a variable or a class field")

  prPrn (OutsideFieldAccess srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Class fields can not be accessed outside of the class and its subclasses")

  prPrn (NestedRefCreation srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Nested references can not be created")

  prPrn (NonRefDeref srcSpan) =
    tcErrorHeaderSpan <> prPrn srcSpan <> colon $+$
    nest indLvl (text "Incorrect usage of '!' operator. It can be used only with Ref variables/fields")

  prPrn (OtherError errMsg) = tcErrorHeader <> colon <+> text errMsg

-- | Indentation level for error messages.
indLvl :: Int
indLvl = 2

