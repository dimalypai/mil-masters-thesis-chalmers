module MIL.AST where

newtype Program = Program ([TypeDef], [FunDef])
  deriving Show

data TypeDef = TypeDef TypeName [TypeVar] [ConDef]
  deriving Show

data ConDef = ConDef ConName [Type]
  deriving Show

data FunDef = FunDef FunName Type Expr
  deriving Show

data Expr = LitE Literal
          | VarE Var
          | LambdaE VarBinder Expr
          | AppE Expr Expr
          | TypeLambdaE TypeVar Expr
          | TypeAppE Expr Type
          | ConNameE ConName
          | TupleE [Expr]
          | ProjE Expr Int
          | NewRefE Expr
          | DerefE Expr
          | AssignRefE Expr Expr
          | LetE VarBinder Expr Expr
          | ReturnE Expr MilMonad
          | LiftE Expr MilMonad MilMonad
          | LetRecE [(VarBinder, Expr)] Expr
          | CaseE Expr [CaseAlt]
  deriving Show

data Literal = UnitLit
             | IntLit Int
  deriving Show

newtype CaseAlt = CaseAlt (Pattern, Expr)
  deriving Show

data Pattern = LitP Literal
             | VarP VarBinder
             | ConP [VarBinder]
             | DefaultP
  deriving Show

data Type = TyMonad MilMonad Type
          | TyTypeCon TypeName Kind
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type
          | TyTuple [Type]
          | TyMonadCons MilMonad MilMonad
  deriving Show

data Kind = StarK
          | Kind :=>: Kind
  deriving Show

newtype VarBinder = VarBind (Var, Type)
  deriving Show

newtype Var = Var String
  deriving Show

newtype TypeVar = TypeVar String
  deriving Show

newtype TypeName = TypeName String
  deriving Show

newtype ConName = ConName String
  deriving Show

newtype FunName = FunName String
  deriving Show

data MilMonad = Id
              | State Type
              | Error Type
              | Lift
              | IO
  deriving Show

