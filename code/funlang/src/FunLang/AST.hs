module FunLang.AST where

import FunLang.SrcSpan

data Program s v = Program s [TypeDef s] [FunDef s v]

type SrcProgram = Program SrcSpan Var
type TyProgram  = Program SrcSpan VarTy

data TypeDef s = TypeDef s (SrcTypeName s) [SrcTypeVar s] [ConDef s]

type SrcTypeDef = TypeDef SrcSpan

data ConDef s = ConDef s (SrcConName s) [SrcType s]

type SrcConDef = ConDef SrcSpan

data FunDef s v = FunDef s (SrcFunName s) (SrcType s) [FunEq s v]

type SrcFunDef = FunDef SrcSpan Var
type TyFunDef  = FunDef SrcSpan VarTy

data FunEq s v = FunEq s (SrcFunName s) [Pattern s] (Expr s v)

data Pattern s = LitP (SrcLiteral s)
               | VarP (SrcVar s)  -- ???
               | ConP s [Pattern s]
               | DefaultP s

data Expr s v = LitE (SrcLiteral s)
              | VarE s v
              | LambdaE s [VarBinder s] (Expr s v)
              | TypeLambdaE s [SrcTypeVar s] (Expr s v)
              | TypeAppE s (Expr s v) (SrcType s)
              | ConNameE (SrcConName s)
              | CaseE s (Expr s v) [CaseAlt s v]
              | LetE s [(VarBinder s, Expr s v)] (Expr s v)
              | DoE s [Stmt s v]
              | BinOpE s (SrcBinOp s) (Expr s v) (Expr s v)
              | ParenE s (Expr s v) -- ???

data Literal = UnitLit
             | IntLit Int

type SrcLiteral s = (s, Literal)

data CaseAlt s v = CaseAlt s (Pattern s) (Expr s v)

data Stmt s v = ExprS (Expr s v)
              | BindS s (VarBinder s) (Expr s v)
              | ReturnS s (Expr s v)

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

type SrcBinOp s = (s, BinOp)

data Type = TyTypeCon TypeName Kind
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type

data SrcType s = SrcTyApp s (SrcTypeName s) [SrcType s]
               | SrcTyVar (SrcTypeVar s)
               | SrcTyArrow s (SrcType s) (SrcType s)
               | SrcTyForAll s (SrcTypeVar s) (SrcType s)

data Kind = StarK
          | Kind :=>: Kind

newtype Var = Var String

newtype VarTy = VarTy (Var, Type)

type SrcVar s = (s, Var)

data VarBinder s = VarBinder s (SrcVar s, SrcType s)

newtype TypeVar = TypeVar String
  deriving Show

type SrcTypeVar s = (s, TypeVar)

newtype TypeName = TypeName String
  deriving Show

type SrcTypeName s = (s, TypeName)

newtype ConName = ConName String

type SrcConName s = (s, ConName)

newtype FunName = FunName String

type SrcFunName s = (s, FunName)

-- Parsing helpers
data TopDef s v = TopTypeDef { getTypeDef :: TypeDef s   }
                | TopFunDef  { getFunDef  :: FunDef  s v }

type SrcTopDef = TopDef SrcSpan Var

isTypeDef :: TopDef s v -> Bool
isTypeDef (TopTypeDef _) = True
isTypeDef              _ = False

isFunDef :: TopDef s v -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

