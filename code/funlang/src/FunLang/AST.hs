module FunLang.AST where

import FunLang.SrcSpan

data Program a = Program a [TypeDef a] [FunDef a]
  deriving Show

type SrcProgram = Program SrcSpan
type TyProgram  = Program (SrcSpan, Type)

instance Annotated Program where
  ann (Program a _ _) = a

data TypeDef a = TypeDef a TypeName [TypeVar] [ConDef]
  deriving Show

type SrcTypeDef = TypeDef SrcSpan
type TyTypeDef  = TypeDef (SrcSpan, Type)

instance Annotated TypeDef where
  ann (TypeDef a _ _ _) = a

data ConDef = ConDef ConName [Type]
  deriving Show

data FunDef a = FunDef a FunName Type [FunEq]
  deriving Show

type SrcFunDef = FunDef SrcSpan
type TyFunDef  = FunDef (SrcSpan, Type)

instance Annotated FunDef where
  ann (FunDef a _ _ _) = a

data FunEq = FunEq FunName [Pattern] Expr
  deriving Show

data Pattern = LitP Literal
             | VarP Var
             | ConP [Pattern]
             | DefaultP
  deriving Show

data Expr = LitE Literal
          | VarE Var
          | LambdaE [VarBinder] Expr
          | TypeLambdaE [TypeVar] Expr
          | TypeAppE Expr Type
          | ConNameE ConName
          | CaseE Expr [CaseAlt]
          | LetE [(VarBinder, Expr)] Expr
          | DoE [Stmt]
          | BinOpE BinOp Expr Expr
  deriving Show

data Literal = UnitLit
             | IntLit Int
  deriving Show

newtype CaseAlt = CaseAlt (Pattern, Expr)
  deriving Show

data Stmt = ExprS Expr
          | BindS VarBinder Expr
          | ReturnS Expr
  deriving Show

data BinOp = App
           | Add
           | Sub
           | Mul
           | Div
           | Equal
           | NotEq
           | Less
           | Greater
           | LessEq
           | GreaterEq
  deriving Show

data Type = TyUnit
          | TyInt
          | TyTypeCon TypeName
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type
  deriving Show

data Kind = StarK
          | Kind :=>: Kind
  deriving Show

newtype Var = Var String
  deriving Show

newtype VarBinder = VarBinder (Var, Type)
  deriving Show

newtype TypeVar = TypeVar String
  deriving Show

newtype TypeName = TypeName String
  deriving Show

newtype ConName = ConName String
  deriving Show

newtype FunName = FunName String
  deriving Show

-- Parsing helpers
data TopDef a = TopTypeDef { getTypeDef :: TypeDef a }
              | TopFunDef  { getFunDef  :: FunDef  a }
  deriving Show

isTypeDef :: TopDef a -> Bool
isTypeDef (TopTypeDef _) = True
isTypeDef              _ = False

isFunDef :: TopDef a -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

type SrcTopDef = TopDef SrcSpan
type TyTopDef  = TopDef (SrcSpan, Type)

instance Annotated TopDef where
  ann (TopTypeDef td) = ann td
  ann (TopFunDef fd) = ann fd

class Annotated ast where
  ann :: ast a -> a

getSrcSpan :: Annotated ast => ast SrcSpan -> SrcSpan
getSrcSpan = ann

