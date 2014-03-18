{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

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
          | LetE VarBinder Expr MilMonad Expr MilMonad  -- maybe don't need them here (they are inside Expr types)
          | ReturnE Expr MilMonad  -- is it present in syntax? Do we need return or can express it with lift like (Id -> State Int)
          | LiftE Expr MilMonad MilMonad  -- present in syntax
          | LetRecE VarBinder Expr Expr
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
          | TyUnit
          | TyInt
          | TyTypeCon TypeName Kind
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type
          | TyTuple [Type]
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

data MilMonad = Id | State | Error | Lift | IO
  deriving Show

type Effects = [Effect] -- instead of MilMonad in Expr
--type EffectTypes = Map.Map String Type  -- functions, data constructors, type constructors
--type EffectTransformations = Map.Map String [Program -> Program]
--type EffectCodeGen = Map.Map Expr -> Code

data Id = I
data State = S
data Error = E
data Lift = L
data IO = MkIO

class Effectful a where
  transform :: a -> a --Program -> Program

instance Effectful Id where
  transform = id

instance Effectful State where
  transform = id

data Effect = forall a. (Effectful a) => Effect a

--getEffect :: Effectful a => Effect -> a
--getEffect (Effect a) = a

instance Show Effect where
  show = const ""

example :: [Effect]
example = [Effect I, Effect S]

--type Eff = State ::: (Error ::: (State ::: Id))
--data m1 ::: m2
--example2 :: State ::: (Error ::: Id)
--example2 = undefined

