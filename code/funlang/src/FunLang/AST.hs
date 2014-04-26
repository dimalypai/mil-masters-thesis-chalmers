-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- AST is parameterised. Some of the data types have only one type parameter s
-- (stands for source) and some have two - v (stands for variable) and s. The
-- reason is that variable occurences are parameterised and are represented
-- differently at different stages. For more on this look at the 'Expr' data
-- type (it is the only place where a field of type v is present). Thus, some
-- of the data types eventually contain 'Expr' (and therefore must have both v
-- and s) and some of them don't (and have only s).
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
-- this order of v and s (as well as when used as type parameters) is chosen
-- for convenience of working with 'SrcAnnotated' type class (so that s is the
-- last type parameter).
--
-- For most of the data types there are type synonyms: Src and Ty versions.
-- Src versions exist for all data types, Ty - only for data types with two
-- type parameters.  Both use 'SrcSpan' as s and Src uses 'Var' as v (or
-- nothing at all), Ty uses 'VarTy' as v. Src variants result from parsing, Ty
-- variants result from type checking.
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
data Program v s = Program s [TypeDef s] [FunDef v s]
  deriving Show

type SrcProgram = Program Var   SrcSpan
type TyProgram  = Program VarTy SrcSpan

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
data FunDef v s = FunDef s (FunNameS s) (TypeS s) [FunEq v s]
  deriving Show

type SrcFunDef = FunDef Var   SrcSpan
type TyFunDef  = FunDef VarTy SrcSpan

-- | Function equation:
--
-- * source annotation
--
-- * function name
--
-- * list of patterns
--
-- * expression (body)
data FunEq v s = FunEq s (FunNameS s) [Pattern s] (Expr v s)
  deriving Show

type SrcFunEq = FunEq Var   SrcSpan
type TyFunEq  = FunEq VarTy SrcSpan

-- | Patterns.
data Pattern s =
    -- | Literal pattern (constant).
    LitP (LiteralS s)
    -- | Variable pattern.
  | VarP (VarBinder s)
    -- | Constructor pattern. May be nested.
  | ConP s (ConNameS s) [Pattern s]
    -- | Default alternative: underscore.
  | DefaultP s
    -- | Pattern in parentheses. Used for better source spans and pretty
    -- printing.
  | ParenP s (Pattern s)
  deriving Show

type SrcPattern = Pattern SrcSpan

-- | Expression representation.
--
-- The most interesting case is 'VarE'. This is where v parameter comes from.
-- After parsing variable occurence contains only a name as a string and a
-- source annotation.  After type checking 'VarE' will have 'VarTy' at this
-- place instead of 'Var', which means that it is also annotated with the type
-- of this variable.
--
-- We have type (big) lambdas and type applications because we use System F as
-- a base for the type system.
--
-- 'ConNameE' stands on its own because constructors act as functions.
--
-- Function names are represented as 'VarE'.
--
-- 'LetE' represents both recursive and non-recursive let. So we have a list of
-- definitions both for convenience and to be able to handle mutually recursive
-- definitions.
--
-- 'ParenE' is used for better source spans and pretty printing.
data Expr v s = LitE (LiteralS s)
              | VarE s v
              | LambdaE s [VarBinder s] (Expr v s)  -- ^ Not empty
              | TypeLambdaE s [TypeVarS s] (Expr v s)  -- ^ Not empty
              | TypeAppE s (Expr v s) (TypeS s)
              | ConNameE (ConNameS s)
              | CaseE s (Expr v s) [CaseAlt v s]  -- ^ Not empty
              | LetE s [(VarBinder s, Expr v s)] (Expr v s)
                -- | Represents simplified Haskell do-blocks for built-in
                -- monads. They can only appear at the top-level (not nested
                -- inside other expressions) as function equation body.
                -- List of statements is not empty
              | DoE s [Stmt v s]
              | BinOpE s (BinOpS s) (Expr v s) (Expr v s)
              | ParenE s (Expr v s)
  deriving Show

type SrcExpr = Expr Var   SrcSpan
type TyExpr  = Expr VarTy SrcSpan

-- | Literal constants.
data Literal = UnitLit
             | IntLit Int
             | FloatLit Double String  -- ^ The user string (for displaying).
             | StringLit String
  deriving Show

type LiteralS s = (Literal, s)
type SrcLiteral = LiteralS SrcSpan

data CaseAlt v s = CaseAlt s (Pattern s) (Expr v s)
  deriving Show

type SrcCaseAlt = CaseAlt Var   SrcSpan
type TyCaseAlt  = CaseAlt VarTy SrcSpan

-- | Statements are parts of the do-block and represent monadic code.
--
-- 'ReturnS' is separate because we don't have type classes like in Haskell.
data Stmt v s = ExprS s (Expr v s)
              | BindS s (VarBinder s) (Expr v s)
              | ReturnS s (TypeS s) (Expr v s)  -- ^ Annotated with monad.
  deriving Show

type SrcStmt = Stmt Var   SrcSpan
type TyStmt  = Stmt VarTy SrcSpan

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

-- | Variable annotated with its type.
newtype VarTy = VarTy (Var, Type)
  deriving Show

-- | Var binder is a pair of variable name and a type (in their source representations).
data VarBinder s = VarBinder s (VarS s) (TypeS s)
  deriving Show

type SrcVarBinder = VarBinder SrcSpan

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

-- Parsing helpers

-- | Used only in parsing to allow to mix type and function definitions and
-- then have them reordered in the AST.
data TopDef v s = TopTypeDef { getTypeDef :: TypeDef s   }
                | TopFunDef  { getFunDef  :: FunDef  v s }

type SrcTopDef = TopDef Var SrcSpan

