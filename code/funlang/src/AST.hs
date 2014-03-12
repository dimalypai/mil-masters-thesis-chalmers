module FunLang.AST where

newtype Program = Program ([TypeDef], [FunDef])
  deriving Show

data TypeDef = TypeDef TypeName [TypeVar] [ConDef]
  deriving Show

data ConDef = ConDef ConName [Type]
  deriving Show

data FunDef = FunDef FunName Type [FunEq]
  deriving Show

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
          | AppTypeE Expr Type
          | ConNameE ConName
          | CaseE Expr [CaseAlt]
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
           | NotEqual
           | Less
           | Greater
           | LessEqual
           | GreaterEqual
  deriving Show

data Type = TyUnit
          | TyInt
          | TyType TypeName
          | TyTypeVar TypeVar
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

