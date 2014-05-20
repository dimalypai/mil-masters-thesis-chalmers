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
-- type parameters.  Both use 'SrcSpan' as s and Src uses '()' as t, Ty uses
-- 'Type' as t. Src variants result from parsing, Ty variants result from type
-- checking.
--
-- Some of the data types have explicit boolean purity indicators.
-- Conservatively, they are all False before the type checking. This is not so
-- pervasive to have an additional type parameter.
--
-- newtypes are used quite extensively to have a strong distinction between
-- different types of names.
module OOLang.AST where

import OOLang.SrcSpan

-- | Program:
--
-- * source annotation
--
-- * list of class definitions
--
-- * list of function definitions
--
-- Note: on the source level these two may be in any order.
data Program t s = Program s [ClassDef t s] [FunDef t s]
  deriving Show

type SrcProgram = Program ()   SrcSpan
type TyProgram  = Program Type SrcSpan

-- | Class definition:
--
-- * source annotation
--
-- * class name
--
-- * super class name (maybe)
--
-- * list of member declarations.
data ClassDef t s = ClassDef s (ClassNameS s) (Maybe (ClassNameS s)) [MemberDecl t s]
  deriving Show

type SrcClassDef = ClassDef ()   SrcSpan
type TyClassDef  = ClassDef Type SrcSpan

-- | Function definition:
--
-- * source annotation
--
-- * function name
--
-- * function type
--
-- * list of statements (body) - not empty
data FunDef t s = FunDef s (FunNameS s) (FunType s) [Stmt t s]
  deriving Show

type SrcFunDef = FunDef ()   SrcSpan
type TyFunDef  = FunDef Type SrcSpan

-- | Class member declaration. Either a field or a method.
data MemberDecl t s = FieldMemberDecl (FieldDecl t s)
                    | MethodMemberDecl (MethodDecl t s)
  deriving Show

type SrcMemberDecl = MemberDecl ()   SrcSpan
type TyMemberDecl  = MemberDecl Type SrcSpan

-- | Field declaration is just a declaration with modifiers (syntactically).
data FieldDecl t s = FieldDecl s (Declaration t s) [ModifierS s]
  deriving Show

type SrcFieldDecl = FieldDecl ()   SrcSpan
type TyFieldDecl  = FieldDecl Type SrcSpan

-- | Method declaration is just a function with modifiers (syntactically).
data MethodDecl t s = MethodDecl s (FunDef t s) [ModifierS s]
  deriving Show

type SrcMethodDecl = MethodDecl ()   SrcSpan
type TyMethodDecl  = MethodDecl Type SrcSpan

-- | Statement representation.
--
-- Annotated with types, which become available after the type checking.
--
-- Some of the statements have explicit purity indicators, for others - purity
-- can be inferred from their components.
data Stmt t s = DeclS s (Declaration t s)
              | ExprS s (Expr t s)
                -- | Left-hand side can only be 'VarE' or 'MemberAccessE'.
              | AssignS s t (AssignOpS s) (Expr t s) (Expr t s) Bool
              | WhileS s t (Expr t s) [Stmt t s]
              | WhenS s t (Expr t s) [Stmt t s] [Stmt t s]
              | ReturnS s (Expr t s)
  deriving Show

type SrcStmt = Stmt ()   SrcSpan
type TyStmt  = Stmt Type SrcSpan

-- | Expression representation.
--
-- Annotated with types, which become available after the type checking.
--
-- Function names are represented as 'VarE'.
--
-- 'ParenE' is used for better source spans and pretty printing.
--
-- Some of the expressions have explicit purity indicators, for others - purity
-- can be inferred from their components, or they are always pure (like
-- literals) or impure (like reference operations).
data Expr t s = LitE (Literal t s)
              | VarE s t Var Bool
              | LambdaE s t [VarBinder s] (Expr t s)  -- ^ Not empty.
              | MemberAccessE s t (Expr t s) (MemberNameS s) Bool
              | MemberAccessMaybeE s t (Expr t s) (MemberNameS s) Bool
                -- | Class access is just for new (constructor) right now.
              | ClassAccessE s t (ClassNameS s) (FunNameS s)
              | ClassAccessStaticE s t (ClassNameS s) (MemberNameS s)
              | NewRefE s t (Expr t s)
                -- | This operator produces a value of type A from a value of
                -- type Ref A.
              | DerefE s t (Expr t s)
              | BinOpE s t (BinOpS s) (Expr t s) (Expr t s) Bool
              | IfThenElseE s t (Expr t s) (Expr t s) (Expr t s)
              | JustE s t (Expr t s)
              | ParenE s (Expr t s)
  deriving Show

type SrcExpr = Expr ()   SrcSpan
type TyExpr  = Expr Type SrcSpan

-- | Literal constants.
--
-- Annotated with types, which become available after the type checking.
data Literal t s = UnitLit s t
                 | BoolLit s t Bool
                 | IntLit s t Int
                 | FloatLit s t Double String  -- ^ The user string (for displaying).
                 | StringLit s t String
                 | NothingLit s t (TypeS s)
  deriving Show

type SrcLiteral = Literal ()   SrcSpan
type TyLiteral  = Literal Type SrcSpan

-- | Binary operators are factored out from 'Expr'.
data BinOp = App
           | NothingCoalesce
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
-- For invariants see type transformations in the TypeChecker.
data Type = TyUnit
          | TyBool
          | TyInt
          | TyFloat
            -- | Strings are not first class citizens.
            -- They can only be used as literals.
          | TyString
          | TyClass ClassName
          | TyArrow Type Type
          | TyPure Type
          | TyMaybe Type
          | TyMutable Type
          | TyRef Type
  deriving (Show, Eq)

-- | Source representation of types. How a user entered them.
--
-- 'SrcTyParen' is used for better source spans and pretty printing.
data TypeS s = SrcTyUnit s
             | SrcTyBool s
             | SrcTyInt s
             | SrcTyFloat s
             | SrcTyClass (ClassNameS s)
             | SrcTyArrow s (TypeS s) (TypeS s)
             | SrcTyPure s (TypeS s)
             | SrcTyMaybe s (TypeS s)
             | SrcTyMutable s (TypeS s)
             | SrcTyRef s (TypeS s)
             | SrcTyParen s (TypeS s)
  deriving Show

type SrcType = TypeS SrcSpan

-- | Function type as it is represented in the source:
--
-- * source annotation
--
-- * list of parameters (var binders)
--
-- * return type
--
-- Note: used in parsing. Later will be transformed to the internal
-- representation (using 'Type').
data FunType s = FunType s [VarBinder s] (TypeS s)
  deriving Show

type SrcFunType = FunType SrcSpan

-- | Name (variable) declaration.
-- Consists of var binder and an optional initialiser.
-- Annotated with the type which becomes available after the type checking.
-- Has a purity indicator.
data Declaration t s = Decl s t (VarBinder s) (Maybe (Init t s)) Bool
  deriving Show

type SrcDeclaration = Declaration ()   SrcSpan
type TyDeclaration  = Declaration Type SrcSpan

-- | Initialiser expression. Uses different assignment operators.
data Init t s = Init s (InitOpS s) (Expr t s)
  deriving Show

type SrcInit = Init ()   SrcSpan
type TyInit  = Init Type SrcSpan

-- | Assignment operators are factored out.
--
-- * Mutable (<-) is for Mutable.
--
-- * Ref (:=) is used for Ref (references).
data AssignOp = AssignMut | AssignRef
  deriving Show

type AssignOpS s = (AssignOp, s)
type SrcAssignOp = AssignOpS SrcSpan

-- | Operators used in declaration statements.
--
-- * Equal is used for immutable (default) variables.
--
-- * Mutable (<-) is for Mutable.
--
-- There is no init operator for references, since they use (=).
data InitOp = InitEqual | InitMut
  deriving Show

type InitOpS s = (InitOp, s)
type SrcInitOp = InitOpS SrcSpan

-- | Modifiers are used in class member declarations.
data Modifier = Public | Private | Static
  deriving Show

type ModifierS s = (Modifier, s)
type SrcModifier = ModifierS SrcSpan

newtype Var = Var String
  deriving (Show, Eq, Ord)

type VarS s = (Var, s)
type SrcVar = VarS SrcSpan

-- | Var binder is a pair of variable name and a type (in their source representations).
-- Used in function parameters.
data VarBinder s = VarBinder s (VarS s) (TypeS s)
  deriving Show

type SrcVarBinder = VarBinder SrcSpan

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

type ClassNameS s = (ClassName, s)
type SrcClassName = ClassNameS SrcSpan

newtype FunName = FunName String
  deriving (Show, Eq, Ord)

type FunNameS s = (FunName, s)
type SrcFunName = FunNameS SrcSpan

-- | Denotes class field or method name.
newtype MemberName = MemberName String
  deriving Show

type MemberNameS s = (MemberName, s)
type SrcMemberName = MemberNameS SrcSpan

-- * Parsing helpers

-- | Used only in parsing to allow to mix class and function definitions and
-- then have them reordered in the AST.
data TopDef t s = TopClassDef { getClassDef :: ClassDef t s }
                | TopFunDef   { getFunDef   :: FunDef t s   }

type SrcTopDef = TopDef () SrcSpan

