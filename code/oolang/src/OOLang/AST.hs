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
data Program s v = Program s [ClassDef s v] [FunDef s v]
  deriving Show

type SrcProgram = Program SrcSpan Var
type TyProgram  = Program SrcSpan VarTy

-- | Class definition:
--
-- * source annotation
--
-- * class name
--
-- * super class name (maybe)
--
-- * list of member declarations.
data ClassDef s v = ClassDef s (SrcClassName s) (Maybe (SrcClassName s)) [MemberDecl s v]
  deriving Show

type SrcClassDef = ClassDef SrcSpan Var
type TyClassDef  = ClassDef SrcSpan VarTy

-- | Function definition:
--
-- * source annotation
--
-- * function name
--
-- * function type
--
-- * list of statements (body)
--
-- * is it marked as pure
data FunDef s v = FunDef s (SrcFunName s) (SrcFunType s) [Stmt s v] Bool
  deriving Show

type SrcFunDef = FunDef SrcSpan Var
type TyFunDef  = FunDef SrcSpan VarTy

-- | Class member declaration. Either field or method.
-- Field declaration is just a declaration with modifiers (syntactically).
-- Method declaration is just a function with modifiers (syntactically).
data MemberDecl s v = FieldDecl s (Declaration s v) [SrcModifier s]
                    | MethodDecl s (FunDef s v) [SrcModifier s]
  deriving Show

type SrcMemberDecl = MemberDecl SrcSpan Var
type TyMemberDecl  = MemberDecl SrcSpan VarTy

data Stmt s v = DeclS (Declaration s v)
              | ExprS (Expr s v)
              | AssignS s (SrcAssignOp s) (Expr s v) (Expr s v)
              | WhileS s (Expr s v) [Stmt s v]
              | WhenS s (Expr s v) [Stmt s v] [Stmt s v]
              | ReturnS s (Expr s v)
  deriving Show

type SrcStmt = Stmt SrcSpan Var
type TyStmt  = Stmt SrcSpan VarTy

-- | Expression representation.
--
-- The most interesting case is 'VarE'. This is where v parameter comes from.
-- After parsing variable occurence contains only a name as a string and a
-- source annotation.  After type checking 'VarE' will have 'VarTy' at this
-- place instead of 'Var', which means that it is also annotated with the type
-- of this variable.
--
-- 'FunNameE' stands on its own because OOLang has functions as first-class
-- values and supports currying.
data Expr s v = LitE (SrcLiteral s)
              | VarE s v
              | FunNameE (SrcFunName s)
              | LambdaE s [VarBinder s] (Expr s v)
              | ClassAccessE s (SrcClassName s) (Expr s v)
              | ClassAccessStaticE s (SrcClassName s) (Expr s v)
              | DerefE s (Expr s v)
              | BinOpE s (SrcBinOp s) (Expr s v) (Expr s v)
              | IfThenElseE s (Expr s v) (Expr s v) (Expr s v)
              | JustE s (Expr s v)
  deriving Show

type SrcExpr = Expr SrcSpan Var
type TyExpr  = Expr SrcSpan VarTy

-- | Literal constants.
data Literal = UnitLit
             | BoolLit Bool
             | IntLit Int
             | FloatLit Double String  -- ^ The user string (for displaying).
             | StringLit String
             | NothingLit
  deriving Show

type SrcLiteral s = (s, Literal)

-- | Binary operators are factored out from 'Expr'.
data BinOp = App
           | MemberAccess
           | MemberAccessMaybe
           | NullCoalesce
           | NewRef
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
data Type = TyUnit
          | TyBool
          | TyInt
          | TyClass ClassName
          | TyArrow Type Type
          | TyMaybe Type
          | TyMutable Type
          | TyRef Type
  deriving Show

-- | Source representation of types. How a user entered them.
data SrcType s = SrcTyUnit s
               | SrcTyBool s
               | SrcTyInt s
               | SrcTyClass (SrcClassName s)
               | SrcTyArrow s (SrcType s) (SrcType s)
               | SrcTyMaybe s (SrcType s)
               | SrcTyMutable s (SrcType s)
               | SrcTyRef s (SrcType s)
               | SrcTyParen s (SrcType s)
  deriving Show

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
data SrcFunType s = SrcFunType s [VarBinder s] (SrcType s)
  deriving Show

-- | Name (variable) declaration.
-- Consists of var binder and an optional initialiser.
data Declaration s v = Decl s (VarBinder s) (Maybe (Init s v))
  deriving Show

-- | Initialiser expression. Uses different assignment operators.
data Init s v = Init s (SrcAssignOp s) (Expr s v)
  deriving Show

-- | Assignment operators are factored out. Some of them are not really
-- assignments:
--
-- * Equal is used for immutable (default) variables.
--
-- * Mutable (<-) is for Mutable.
--
-- * Ref (:=) is used for Ref (references).
data AssignOp = AssignEqual | AssignMut | AssignRef
  deriving Show

type SrcAssignOp s = (s, AssignOp)

-- | Modifiers are used in class member declarations.
data Modifier = Public | Private | Static
  deriving Show

type SrcModifier s = (s, Modifier)

newtype Var = Var String
  deriving Show

-- | Variable annotated with its type.
newtype VarTy = VarTy (Var, Type)
  deriving Show

type SrcVar s = (s, Var)

-- | Var binder is a pair of variable name and a type (in their source representations).
-- Used in function parameters.
data VarBinder s = VarBinder s (SrcVar s) (SrcType s)
  deriving Show

newtype ClassName = ClassName String
  deriving (Show, Eq)

type SrcClassName s = (s, ClassName)

getClassName :: SrcClassName s -> ClassName
getClassName = snd

newtype FunName = FunName String
  deriving Show

type SrcFunName s = (s, FunName)

-- Parsing helpers

-- | Used only in parsing to allow to mix class and function definitions and
-- then have them reordered in the AST.
data TopDef s v = TopClassDef { getClassDef :: ClassDef s v }
                | TopFunDef   { getFunDef   :: FunDef s v   }

type SrcTopDef = TopDef SrcSpan Var

isClassDef :: TopDef s v -> Bool
isClassDef (TopClassDef _) = True
isClassDef               _ = False

isFunDef :: TopDef s v -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

