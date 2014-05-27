-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- AST is parameterised. Some of the data types have only one type parameter s
-- (stands for source) and some have two - t (stands for type) and s. The
-- reason is that for some syntax nodes (related to statements and expressions)
-- it makes sense to be annotated with the type, which becomes available during
-- the type checking (and before that it is just ()), hence the parameter. For
-- more on this look, for example, at the 'Expr' and 'Stmt' data types. Thus,
-- some of the data types eventually contain, for example, 'Expr' (and
-- therefore must have both t and s) and some of them don't (and have only s).
--
-- In general, sometimes there are two version of the data type, one of which
-- may have S suffix. This distinction is for source representation of the
-- program (S data types) and internal representation which is used in later
-- phases (after parsing). The most important is 'Type' and 'TypeS'.
--
-- Some of the data types (which have several data constructors and/or
-- recursive) have s fields wired-in, while others if possible have a type
-- synonym for a pair where the first component is unannotated data type and
-- the second one is of type s. Look at most of the *Name data types. Note:
-- this order of t and s (as well as when used as type parameters) is chosen
-- for convenience of working with 'SrcAnnotated' type class (so that s is the
-- last type parameter).
--
-- For most of the data types there are type synonyms: Src and Ty versions.
-- Src versions exist for all data types, Ty - only for data types with two
-- type parameters. Both use 'SrcSpan' as s and Src uses '()' as t, Ty uses
-- 'Type' as t. Src variants result from parsing, Ty variants result from type
-- checking.
--
-- newtypes are used quite extensively to have a strong distinction between
-- different types of names.
module FunLang.AST where

import FunLang.SrcSpan

-- | Program:
--
-- * source annotation
--
-- * list of type definitions
--
-- * list of function definitions
--
-- Note: on the source level these two may be in any order.
data Program t s = Program s [TypeDef s] [FunDef t s]
  deriving Show

type SrcProgram = Program ()   SrcSpan
type TyProgram  = Program Type SrcSpan

-- | Type definition:
--
-- * source annotation
--
-- * type name
--
-- * list of type variables (type parameters)
--
-- * list of constructor definitions
data TypeDef s = TypeDef s (TypeNameS s) [TypeVarS s] [ConDef s]
  deriving Show

type SrcTypeDef = TypeDef SrcSpan

-- | Constructor definition:
--
-- * source annotation
--
-- * constructor name
--
-- * constructor fields (expressed as types)
data ConDef s = ConDef s (ConNameS s) [TypeS s]
  deriving Show

type SrcConDef = ConDef SrcSpan

-- | Function definition:
--
-- * source annotation
--
-- * function name
--
-- * function type
--
-- * list of function equations
data FunDef t s = FunDef s (FunNameS s) (TypeS s) [FunEq t s]
  deriving Show

type SrcFunDef = FunDef ()   SrcSpan
type TyFunDef  = FunDef Type SrcSpan

-- | Function equation:
--
-- * source annotation
--
-- * function name
--
-- * list of patterns
--
-- * expression (body)
data FunEq t s = FunEq s (FunNameS s) [Pattern t s] (Expr t s)
  deriving Show

type SrcFunEq = FunEq ()   SrcSpan
type TyFunEq  = FunEq Type SrcSpan

-- | Patterns.
data Pattern t s =
    -- | Literal pattern (constant).
    LitP (Literal t s)
    -- | Variable pattern.
  | VarP (VarBinder t s)
    -- | Constructor pattern. May be nested. Annotated with a type.
  | ConP s t (ConNameS s) [Pattern t s]
    -- | Default alternative: underscore. Annotated with a type.
  | DefaultP s t
    -- | Pattern in parentheses. Used for better source spans and pretty
    -- printing.
  | ParenP s (Pattern t s)
  deriving Show

type SrcPattern = Pattern ()   SrcSpan
type TyPattern  = Pattern Type SrcSpan

-- | Expression representation.
--
-- Annotated with types, which become available after the type checking.
--
-- We have type (big) lambdas and type applications because we use System F as
-- a base for the type system.
--
-- 'ConNameE' stands on its own because constructors act as functions.
--
-- Function names are represented as 'VarE'.
data Expr t s = LitE (Literal t s)
              | VarE s t Var
                -- | List of variable binders is not empty.
              | LambdaE s t [VarBinder t s] (Expr t s)
                -- | List of type variables is not empty.
              | TypeLambdaE s t [TypeVarS s] (Expr t s)
              | TypeAppE s t (Expr t s) (TypeS s)
              | ConNameE t (ConNameS s)
                -- | List of case alternatives is not empty.
              | CaseE s t (Expr t s) [CaseAlt t s]
                -- | Represents both recursive and non-recursive let. So we
                -- have a list of definitions both for convenience and to be
                -- able to handle mutually recursive definitions.
              | LetE s [(VarBinder t s, Expr t s)] (Expr t s)
                -- | Represents simplified Haskell do-blocks for built-in
                -- monads. They can only appear at the top-level (not nested
                -- inside other expressions) as function equation body.
                -- List of statements is not empty.
              | DoE s t [Stmt t s]
              | BinOpE s t (BinOpS s) (Expr t s) (Expr t s)
                -- | Annotated with the type on the source level.
              | ThrowE s t (TypeS s)
                -- | Used for better source spans and pretty printing.
              | ParenE s (Expr t s)
  deriving Show

type SrcExpr = Expr ()   SrcSpan
type TyExpr  = Expr Type SrcSpan

-- | Literal constants.
data Literal t s = UnitLit s t
                 | IntLit s t Int
                 | FloatLit s t Double String  -- ^ The user string (for displaying).
                 | StringLit s t String
  deriving Show

type SrcLiteral = Literal ()   SrcSpan
type TyLiteral  = Literal Type SrcSpan

data CaseAlt t s = CaseAlt s (Pattern t s) (Expr t s)
  deriving Show

type SrcCaseAlt = CaseAlt ()   SrcSpan
type TyCaseAlt  = CaseAlt Type SrcSpan

-- | Statements are parts of the do-block and represent monadic code.
--
-- 'ReturnS' is separate because we don't have type classes like in Haskell. It
-- also has an explicit type annotation, because its type is different from the
-- expression it contains.
data Stmt t s = ExprS s (Expr t s)
              | BindS s (VarBinder t s) (Expr t s)
              | ReturnS s t (Expr t s)
  deriving Show

type SrcStmt = Stmt ()   SrcSpan
type TyStmt  = Stmt Type SrcSpan

-- | Binary operators are factored out from 'Expr'.
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
           | Catch
  deriving Show

type BinOpS s = (BinOp, s)
type SrcBinOp = BinOpS SrcSpan

-- | Internal representation of types. What types really represent.
--
-- Type constructors are fully applied. See also comment on 'Kind'.
--
-- Note: we don't have data constructors for built-in types. They all are
-- handled uniformly with user-defined data types with 'TyApp'. But function
-- arrows have a special treatment.
data Type = TyVar TypeVar  -- ^ Always of kind *
          | TyArrow Type Type
          | TyApp TypeName [Type]
          | TyForAll TypeVar Type
  deriving (Show, Eq, Ord)

-- | Source representation of types. How a user entered them.
--
-- We use binary application and 'SrcTyCon' as opposed to the 'Type' because of
-- a more convenient parsing. This is more general than we allow in the language.
--
-- 'SrcTyParen' is used for better source spans and pretty printing.
--
-- Note: during parsing we can't distinguish between type names (type
-- constructors) and type variables, therefore they all are handled with
-- 'SrcTyCon'.
data TypeS s = SrcTyCon (TypeNameS s)
             | SrcTyApp s (TypeS s) (TypeS s)
             | SrcTyArrow s (TypeS s) (TypeS s)
             | SrcTyForAll s (TypeVarS s) (TypeS s)
             | SrcTyParen s (TypeS s)
  deriving Show

type SrcType = TypeS SrcSpan

-- | \"Type of the type\".
-- This representation is more general that we allow in the language. Also note,
-- that we don't really support System F Omega: all type constructors must be
-- fully applied and all type variables are of kind *. There is no syntactic
-- representation of kinds in the source languages, they are used for type
-- (kind) checking.
data Kind = StarK
          | Kind :=>: Kind
  deriving (Show, Eq)

newtype Var = Var String
  deriving (Show, Eq, Ord)

type VarS s = (Var, s)
type SrcVar = VarS SrcSpan

-- | Var binder is a pair of variable name and a type (in their source
-- representations).
-- After the type checking becomes annotated with an internal type
-- representation.
data VarBinder t s = VarBinder s t (VarS s) (TypeS s)
  deriving Show

type SrcVarBinder = VarBinder ()   SrcSpan
type TyVarBinder  = VarBinder Type SrcSpan

newtype TypeVar = TypeVar String
  deriving (Show, Eq, Ord)

type TypeVarS s = (TypeVar, s)
type SrcTypeVar = TypeVarS SrcSpan

newtype TypeName = TypeName String
  deriving (Show, Eq, Ord)

type TypeNameS s = (TypeName, s)
type SrcTypeName = TypeNameS SrcSpan

newtype ConName = ConName String
  deriving (Show, Eq, Ord)

type ConNameS s = (ConName, s)
type SrcConName = ConNameS SrcSpan

newtype FunName = FunName String
  deriving (Show, Eq, Ord)

type FunNameS s = (FunName, s)
type SrcFunName = FunNameS SrcSpan

-- * Parsing helpers

-- | Used only in parsing to allow to mix type and function definitions and
-- then have them reordered in the AST.
data TopDef t s = TopTypeDef { getTypeDef :: TypeDef s   }
                | TopFunDef  { getFunDef  :: FunDef  t s }

type SrcTopDef = TopDef () SrcSpan

