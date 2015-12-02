{-# LANGUAGE DeriveDataTypeable #-}

-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- We don't have source annotations for MIL because it is not a user-facing
-- language.
--
-- AST is highly parameterised. Parameterised places are: variable occurence,
-- constructor type annotation, monadic type annotation and all other type
-- annotations. For the most parameterised type, look at the 'Expr' data type.
-- Naming convention for type parameters: v stands for Variable, ct - for
-- Constructor Type, mt - for Monad Type, t - for Type.
--
-- There are two different representations for types: 'SrcType' for the Parser
-- view of types and 'Type' for the types representation after the type
-- checking.
--
-- For most of the data types there are type synonyms: Src and Ty versions.
-- Src uses 'SrcType' for the majority of type annotation related parameters,
-- Ty mostly uses 'Type' there. Src variants result from parsing, Ty variants
-- result from type checking.
--
-- newtypes are used quite extensively to have a strong distinction between
-- different types of names.
--
-- Derived 'Eq' instances on AST nodes are used for testing purposes.
module MIL.AST where

import Data.Data
import Data.Generics.Uniplate.Data

-- | Program:
--
-- * list of type definitions
--
-- * list of function definitions
--
-- Note: they must be in the same order on the source level.
newtype Program v ct mt t = Program ([TypeDef t], [FunDef v ct mt t])
  deriving (Show, Eq)

type SrcProgram = Program Var () SrcType SrcType
type TyProgram  = Program TyVarBinder Type MonadType Type

-- | Type definition:
--
-- * type name
--
-- * list of type variables (type parameters)
--
-- * list of constructor definitions
data TypeDef t = TypeDef TypeName [TypeVar] [ConDef t]
  deriving (Show, Eq)

type SrcTypeDef = TypeDef SrcType
type TyTypeDef  = TypeDef Type

-- | Constructor definition:
--
-- * constructor name
--
-- * constructor fields (expressed as types)
data ConDef t = ConDef ConName [t]
  deriving (Show, Eq)

type SrcConDef = ConDef SrcType
type TyConDef  = ConDef Type

-- | Function definition:
--
-- * function name
--
-- * function type
--
-- * expression (body)
--
-- Note: there is only one function equation and it is without patterns.
data FunDef v ct mt t = FunDef FunName t (Expr v ct mt t)
  deriving (Show, Eq)

type SrcFunDef = FunDef Var () SrcType SrcType
type TyFunDef  = FunDef TyVarBinder Type MonadType Type

-- | Expression representation.
--
-- We have type (big) lambdas and type applications because we use System F as
-- a base for the type system.
--
-- 'LetE' (bind), 'ReturnE' and 'LiftE' are monadic operations.
--
-- Many things are represented differently after parsing and after type
-- checking:
-- * variable occurences
--
-- * constructor type annotations
--
-- * monad type annotations (for 'ReturnE' and 'LiftE')
--
-- * other type annotations ('TypeAppE' etc.)
--
-- For more on this, look at 'SrcExpr' and 'TyExpr'.
data Expr v ct mt t
  = LitE Literal
  | VarE v
  | LambdaE (VarBinder t) (Expr v ct mt t)
  | AppE (Expr v ct mt t) (Expr v ct mt t)
  | TypeLambdaE TypeVar (Expr v ct mt t)
  | TypeAppE (Expr v ct mt t) t
    -- | 'ConNameE' stands on its own because constructors act as
    -- functions.
  | ConNameE ConName ct
  | LetE (VarBinder t) (Expr v ct mt t) (Expr v ct mt t)
  | ReturnE mt (Expr v ct mt t)
  | LiftE (Expr v ct mt t) mt mt
    -- | 'LetRecE' has a list of definitions in order to be able to
    -- handle mutually recursive definitions.
  | LetRecE [(VarBinder t, Expr v ct mt t)] (Expr v ct mt t)
  | CaseE (Expr v ct mt t) [CaseAlt v ct mt t]
  | TupleE [Expr v ct mt t]
  deriving (Show, Eq, Data, Typeable)

type SrcExpr = Expr Var () SrcType SrcType
type TyExpr  = Expr TyVarBinder Type MonadType Type

-- | Literal constants.
data Literal = UnitLit
             | IntLit Int
             | FloatLit Double
             | CharLit Char
  deriving (Show, Eq, Data, Typeable)

newtype CaseAlt v ct mt t = CaseAlt (Pattern t, Expr v ct mt t)
  deriving (Show, Eq, Data, Typeable)

type SrcCaseAlt = CaseAlt Var () SrcType SrcType
type TyCaseAlt  = CaseAlt TyVarBinder Type MonadType Type

-- | Patterns.
data Pattern t
    -- | Literal pattern (constant).
  = LitP Literal
    -- | Variable pattern. Needs to contain type.
  | VarP (VarBinder t)
    -- | Constructor pattern. Can't be nested.
  | ConP ConName [VarBinder t]
    -- | Tuple pattern. Can't be nested.
  | TupleP [VarBinder t]
    -- | Default alternative: underscore.
  | DefaultP
  deriving (Show, Eq, Data, Typeable)

type SrcPattern = Pattern SrcType
type TyPattern  = Pattern Type

-- | Source representation of types. Very general.
-- 'SrcTyTypeCon' can represent type constructors, type variables, MIL monads.
-- 'SrcTyMonadCons' allows much more types to be represented, which
-- then will be rejected by the TypeChecker.
data SrcType
  = SrcTyTypeCon TypeName
  | SrcTyArrow SrcType SrcType
  | SrcTyForAll TypeVar SrcType
  | SrcTyApp SrcType SrcType
  | SrcTyTuple [SrcType]
  | SrcTyMonadCons SrcType SrcType
  deriving (Show, Eq)

-- | Types representation.
--
-- Note: we don't have data constructors for built-in types. They all are
-- handled uniformly with user-defined data types.
--
-- Monads are represented as follows:
--
-- * separately standing monad is the 'TyMonad'
--
-- * applied monadic type is 'TyApp' where the first component is 'TyMonad'
data Type
  = TyTypeCon TypeName
  | TyVar TypeVar
  | TyArrow Type Type
  | TyForAll TypeVar Type
  | TyApp Type Type
  | TyTuple [Type]
  | TyMonad MonadType
  deriving (Show, Eq, Data, Typeable)

-- | Monadic type. It is either a single monad or a monad on top of another
-- 'MonadType'. This represents a monad transformers stack, basically.
data MonadType
  = MTyMonad SingleMonad
  | MTyMonadCons SingleMonad MonadType
  deriving (Show, Eq, Data, Typeable)

-- | Single monad can be either just built-in monad or an application to some
-- type (relevant for parameterised monads, like 'Error').
data SingleMonad
  = SinMonad MilMonad
  | SinMonadApp SingleMonad Type
  deriving (Show, Eq, Data, Typeable)

-- | "Type of the type".
-- This representation is more general that we allow in the language. Also note,
-- that we don't really support System F Omega: all type constructors must be
-- fully applied and all type variables are of kind *. There is no syntactic
-- representation of kinds in the source languages, they are used for type
-- (kind) checking.
data Kind = StarK
          | Kind :=>: Kind
  deriving (Show, Eq)

newtype VarBinder t = VarBinder (Var, t)
  deriving (Show, Eq, Data, Typeable)

type SrcVarBinder = VarBinder SrcType
type TyVarBinder  = VarBinder Type

newtype Var = Var String
  deriving (Show, Eq, Ord, Data, Typeable)

newtype TypeVar = TypeVar String
  deriving (Show, Eq, Ord, Data, Typeable)

newtype TypeName = TypeName String
  deriving (Show, Eq, Ord, Data, Typeable)

newtype ConName = ConName String
  deriving (Show, Eq, Ord, Data, Typeable)

newtype FunName = FunName String
  deriving (Show, Eq, Ord)

-- | Built-in monads (effects).
data MilMonad = Id
              | State
              | Error
              | IO
  deriving (Show, Read, Eq, Data, Typeable)

-- * Precedences

getExprPrec :: Expr v ct mt t -> Int
getExprPrec LitE        {} = 6
getExprPrec VarE        {} = 6
getExprPrec LambdaE     {} = 3
getExprPrec AppE        {} = 5
getExprPrec TypeLambdaE {} = 3
getExprPrec TypeAppE    {} = 5
getExprPrec ConNameE    {} = 6
getExprPrec LetE        {} = 2
getExprPrec ReturnE     {} = 2
getExprPrec LiftE       {} = 2
getExprPrec CaseE       {} = 2
getExprPrec TupleE      {} = 6

-- | Returns whether the first expression has a lower precedence than the
-- second one. Convenient to use in infix form.
--
-- Note: It is reflexive: e `exprHasLowerPrec` e ==> True
exprHasLowerPrec :: Expr v ct mt t -> Expr v ct mt t -> Bool
exprHasLowerPrec e1 e2 = getExprPrec e1 <= getExprPrec e2

-- | Returns whether the first expression has a lower precedence than the
-- second one. Convenient to use in infix form.
-- This version can be used with associative operators, for example: arithmetic
-- operations, function application. See "MIL.AST.PrettyPrinter".
--
-- Note: It is *not* reflexive: e `exprHasLowerPrecAssoc` e ==> False
exprHasLowerPrecAssoc :: Expr v ct mt t -> Expr v ct mt t -> Bool
exprHasLowerPrecAssoc e1 e2 = getExprPrec e1 < getExprPrec e2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
--
-- Note: It is reflexive: t `typeHasLowerPrec` t ==> True
typeHasLowerPrec :: IsType t => t -> t -> Bool
typeHasLowerPrec t1 t2 = getTypePrec t1 <= getTypePrec t2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
-- This version can be used with associative type operators, for example:
-- arrow, type application. See "MIL.AST.PrettyPrinter".
--
-- Note: It is *not* reflexive: t `typeHasLowerPrecAssoc` t ==> False
typeHasLowerPrecAssoc :: IsType t => t -> t -> Bool
typeHasLowerPrecAssoc t1 t2 = getTypePrec t1 < getTypePrec t2

class IsType t where
  isAtomicType :: t -> Bool
  getTypePrec :: t -> Int

instance IsType SrcType where
  isAtomicType (SrcTyTypeCon {}) = True
  isAtomicType (SrcTyTuple   {}) = True
  isAtomicType                 _ = False

  getTypePrec (SrcTyTypeCon   {}) = 5
  getTypePrec (SrcTyArrow     {}) = 2
  getTypePrec (SrcTyForAll    {}) = 1
  getTypePrec (SrcTyApp       {}) = 4
  getTypePrec (SrcTyTuple     {}) = 5
  getTypePrec (SrcTyMonadCons {}) = 3

instance IsType Type where
  -- | Type constructors, type variables and tuple types are atomic types.
  isAtomicType (TyTypeCon {}) = True
  isAtomicType (TyVar     {}) = True
  isAtomicType (TyTuple   {}) = True
  isAtomicType              _ = False

  getTypePrec (TyTypeCon {}) = 5
  getTypePrec (TyVar     {}) = 5
  getTypePrec (TyArrow   {}) = 2
  getTypePrec (TyForAll  {}) = 1
  getTypePrec (TyApp     {}) = 4
  getTypePrec (TyTuple   {}) = 5
  -- Dirty hacking for nice pretty printing:
  -- Atomic monad doesn't get parentheses, but cons does.
  getTypePrec (TyMonad (MTyMonad {})) = 5
  getTypePrec (TyMonad (MTyMonadCons {})) = 3

