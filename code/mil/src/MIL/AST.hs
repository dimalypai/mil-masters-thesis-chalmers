-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- We don't have source annotations for MIL because it is not user-facing
-- language.  When it comes to parsing MIL, we will probably need to change the
-- AST to make it parameterised over variable occurence type: just variable
-- after parsing and variable with its type after type checking. But for now,
-- we assume that source language compiler generates typed MIL and type
-- checking is only used to transformation checking.
--
-- newtypes are used quite extensively to have a strong distinction between
-- different types of names.
module MIL.AST where

-- | Program:
--
-- * list of type definitions
--
-- * list of function definitions
--
-- Note: they must be in the same order on the source level.
newtype Program = Program ([TypeDef], [FunDef])
  deriving Show

-- | Type definition:
--
-- * type name
--
-- * list of type variables (type parameters)
--
-- * list of constructor definitions
data TypeDef = TypeDef TypeName [TypeVar] [ConDef]
  deriving Show

-- | Constructor definition:
--
-- * constructor name
--
-- * constructor fields (expressed as types)
data ConDef = ConDef ConName [Type]
  deriving Show

-- | Function definition:
--
-- * function name
--
-- * function type
--
-- * expression (body)
--
-- Note: there is only one function equation and it is without patterns.
data FunDef = FunDef FunName Type Expr
  deriving Show

-- | Expression representation.
--
-- We have type (big) lambdas and type applications because we use System F as
-- a base for the type system.
--
-- 'ConNameE' stands on its own because constructors act as functions.
--
-- 'LetE' (bind), 'ReturnE' and 'LiftE' are monadic operations.
--
-- 'LetRecE' has a list of definitions in order to be able to handle mutually
-- recursive definitions.
--
-- 'CaseE' must have exhaustive patterns.
data Expr = LitE Literal
          | VarE VarBinder
          | LambdaE VarBinder Expr
          | AppE Expr Expr
          | TypeLambdaE TypeVar Expr
          | TypeAppE Expr Type
          | ConNameE ConName
          | NewRefE Expr
          | DerefE Expr
          | AssignRefE Expr Expr
          | LetE VarBinder Expr Expr
          | ReturnE Expr MilMonad
          | LiftE Expr MilMonad MilMonad
          | LetRecE [(VarBinder, Expr)] Expr
          | CaseE Expr [CaseAlt]
  deriving Show

-- | Literal constants.
data Literal = UnitLit
             | IntLit Int
  deriving Show

newtype CaseAlt = CaseAlt (Pattern, Expr)
  deriving Show

-- | Patterns.
data Pattern =
    -- | Literal pattern (constant).
    LitP Literal
    -- | Variable pattern. Needs to contain type.
  | VarP VarBinder
    -- | Constructor pattern. Can't be nested.
  | ConP [VarBinder]
    -- | Default alternative: underscore.
  | DefaultP
  deriving Show

-- | Types representation.
--
-- Note: we don't have data constructors for built-in types. They all are
-- handled uniformly with user-defined data types.
data Type = TyMonad MilMonad Type
          | TyTypeCon TypeName Kind
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type
          | TyMonadCons MilMonad MilMonad
  deriving Show

-- | "Type of the type".
-- This representation is more general that we allow in the language. Also note,
-- that we don't really support System F Omega: all type constructors must be
-- fully applied and all type variables are of kind *. There is no syntactic
-- representation of kinds in the source languages, they are used for type
-- (kind) checking.
data Kind = StarK
          | Kind :=>: Kind
  deriving Show

newtype VarBinder = VarBinder (Var, Type)
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

-- | Built-in monads (effects).
data MilMonad = Id
              | State Type
              | Error Type
              | Lift
              | IO
  deriving Show

-- | Type constructors and type variables are atomic types.
isAtomicType :: Type -> Bool
isAtomicType (TyTypeCon {}) = True
isAtomicType (TyVar     {}) = True
isAtomicType              _ = False

-- Precedences

getExprPrec :: Expr -> Int
getExprPrec (LitE    {}) = 5
getExprPrec (VarE    {}) = 5
getExprPrec (LambdaE {}) = 3
getExprPrec (AppE    {}) = 4

exprHasLowerPrec :: Expr -> Expr -> Bool
exprHasLowerPrec e1 e2 = getExprPrec e1 < getExprPrec e2

getTypePrec :: Type -> Int
getTypePrec (TyTypeCon {}) = 4
getTypePrec (TyVar     {}) = 4
getTypePrec (TyArrow   {}) = 2
getTypePrec (TyForAll  {}) = 1
getTypePrec (TyApp     {}) = 3

typeHasLowerPrec :: Type -> Type -> Bool
typeHasLowerPrec t1 t2 = getTypePrec t1 <= getTypePrec t2

-- Smart constructors

mkTypeVar :: String -> Type
mkTypeVar = TyVar . TypeVar

mkSimpleType :: String -> Type
mkSimpleType typeName = TyTypeCon (TypeName typeName) StarK

