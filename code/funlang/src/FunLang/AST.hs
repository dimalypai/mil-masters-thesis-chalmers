-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- AST is parameterised. Some of the data types have only one type parameter s
-- (stands for source) and some have two - s and v (stands for variable).
-- The reason is that variable occurences are parameterised and are represented
-- differently at different stages.  For more on this look at the 'Expr' data
-- type (it is the only place where a field of type v is present). Thus, some
-- of the data types eventually contain 'Expr' (and therefore must have both s
-- and v) and some of them don't (and have only s).
--
-- In general, sometimes there are two version of the data type, one of which
-- may have Src prefix. This distinction is for source representation of the
-- program (Src data types) and internal representation which is used in later
-- phases (after parsing). The most important is 'Type' and 'SrcType'.
--
-- Some of the data types (which have several data constructors and/or
-- recursive) have s fields wired-in, while others if possible have a type
-- synonym for a pair where the first component is s and the second is
-- unannotated data type. Look at most of the *Name data types.
--
-- For most of the data types with both s and v there are two type synonyms:
-- Src and Ty versions of it. Both use 'SrcSpan' as s and Src uses 'Var' as v,
-- Ty uses 'VarTy' as v. Src variants result from parsing, Ty variants result
-- from type checking.
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
data Program s v = Program s [TypeDef s] [FunDef s v]
  deriving Show

type SrcProgram = Program SrcSpan Var
type TyProgram  = Program SrcSpan VarTy

-- | Type definition:
--
-- * source annotation
--
-- * type name
--
-- * list of type variables (type parameters)
--
-- * list of constructor definitions
data TypeDef s = TypeDef s (SrcTypeName s) [SrcTypeVar s] [ConDef s]
  deriving Show

type SrcTypeDef = TypeDef SrcSpan

-- | Constructor definition:
--
-- * source annotation
--
-- * constructor name
--
-- * constructor fields (expressed as types)
data ConDef s = ConDef s (SrcConName s) [SrcType s]
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
data FunDef s v = FunDef s (SrcFunName s) (SrcType s) [FunEq s v]
  deriving Show

type SrcFunDef = FunDef SrcSpan Var
type TyFunDef  = FunDef SrcSpan VarTy

-- | Function equation:
--
-- * source annotation
--
-- * function name
--
-- * list of patterns
--
-- * expression (body)
data FunEq s v = FunEq s (SrcFunName s) [Pattern s] (Expr s v)
  deriving Show

-- | Patterns.
data Pattern s =
    -- | Literal pattern (constant).
    LitP (SrcLiteral s)
    -- | Variable pattern.
  | VarP (SrcVar s)  -- ???
    -- | Constructor pattern. May be nested.
  | ConP s [Pattern s]
    -- | Default alternative: underscore.
  | DefaultP s
  deriving Show

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
-- 'LetE' represents both recursive and non-recursive let. So we have a list of
-- definitions both for convenience and to be able to handle mutually recursive
-- definitions.
--
-- 'DoE' represents simplified Haskell do-blocks for built-in monads.
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
  deriving Show

-- | Literal constants.
data Literal = UnitLit
             | IntLit Int
             | FloatLit Double String  -- ^ The user string (for displaying).
             | StringLit String
  deriving Show

type SrcLiteral s = (s, Literal)

data CaseAlt s v = CaseAlt s (Pattern s) (Expr s v)
  deriving Show

-- | Statements are parts of the do-block and represent monadic code.
data Stmt s v = ExprS (Expr s v)
              | BindS s (VarBinder s) (Expr s v)
              | ReturnS s (Expr s v)
  deriving Show

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

type SrcBinOp s = (s, BinOp)

-- | Internal representation of types. What types really represent.
--
-- Note: we don't have data constructors for built-in types. They all are
-- handled uniformly with user-defined data types.
data Type = TyTypeCon TypeName Kind
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type

-- | Source representation of types. How a user entered them.
--
-- Note: during parsing we can't distinguish between type names (type
-- constructors) and type variables, therefore they all are handled with
-- SrcTyApp.
data SrcType s = SrcTyApp s (SrcTypeName s) [SrcType s]
               | SrcTyArrow s (SrcType s) (SrcType s)
               | SrcTyForAll s (SrcTypeVar s) (SrcType s)
  deriving Show

-- | \"Type of the type\".
-- This representation is more general that we allow in the language. Also note,
-- that we don't really support System F Omega: all type constructors must be
-- fully applied and all type variables are of kind *. There is no syntactic
-- representation of kinds in the source languages, they are used for type
-- (kind) checking.
data Kind = StarK
          | Kind :=>: Kind

newtype Var = Var String
  deriving Show

-- | Variable annotated with its type.
newtype VarTy = VarTy (Var, Type)

type SrcVar s = (s, Var)

-- | Var binder is a pair of variable name and a type (in their source representations).
data VarBinder s = VarBinder s (SrcVar s) (SrcType s)
  deriving Show

newtype TypeVar = TypeVar String
  deriving Show

type SrcTypeVar s = (s, TypeVar)

newtype TypeName = TypeName String
  deriving Show

type SrcTypeName s = (s, TypeName)

newtype ConName = ConName String
  deriving Show

type SrcConName s = (s, ConName)

newtype FunName = FunName String
  deriving Show

type SrcFunName s = (s, FunName)

-- Parsing helpers

-- | Used only in parsing to allow to mix type and function definitions and
-- then have them reordered in the AST.
data TopDef s v = TopTypeDef { getTypeDef :: TypeDef s   }
                | TopFunDef  { getFunDef  :: FunDef  s v }

type SrcTopDef = TopDef SrcSpan Var

isTypeDef :: TopDef s v -> Bool
isTypeDef (TopTypeDef _) = True
isTypeDef              _ = False

isFunDef :: TopDef s v -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

