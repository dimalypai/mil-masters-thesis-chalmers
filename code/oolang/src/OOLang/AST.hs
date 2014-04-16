-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- AST is parameterised. Some of the data types have only one type parameter s
-- (stands for source) and some have two - s and v (stands for variable). The
-- reason is that variable occurences are parameterised and are represented
-- differently at different stages. For more on this look at the 'Expr' and
-- 'Stmt' data types (these are the only places where a field of type v is
-- present). Thus, some of the data types eventually contain 'Expr' or 'Stmt'
-- (and therefore must have both s and v) and some of them don't (and have only
-- s).
--
-- In general, sometimes there are two version of the data type, one of which
-- may have S suffix. This distinction is for source representation of the
-- program (S data types) and internal representation which is used in later
-- phases (after parsing). The most important is 'Type' and 'TypeS'.
--
-- Some of the data types (which have several data constructors and/or
-- recursive) have s fields wired-in, while others if possible have a type
-- synonym for a pair where the first component is s and the second is
-- unannotated data type. Look at most of the *Name data types.
--
-- For most of the data types there are type synonyms: Src and Ty versions.
-- Src versions exist for all data types, Ty - only for data types with two
-- type parameters.  Both use 'SrcSpan' as s and Src uses 'Var' as v (or
-- nothing at all), Ty uses 'VarTy' as v. Src variants result from parsing, Ty
-- variants result from type checking.
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
data ClassDef s v = ClassDef s (ClassNameS s) (Maybe (ClassNameS s)) [MemberDecl s v]
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
data FunDef s v = FunDef s (FunNameS s) (FunType s) [Stmt s v] Bool
  deriving Show

type SrcFunDef = FunDef SrcSpan Var
type TyFunDef  = FunDef SrcSpan VarTy

-- | Class member declaration. Either field or method.
-- Field declaration is just a declaration with modifiers (syntactically).
-- Method declaration is just a function with modifiers (syntactically).
data MemberDecl s v = FieldDecl s (Declaration s v) [ModifierS s]
                    | MethodDecl s (FunDef s v) [ModifierS s]
  deriving Show

type SrcMemberDecl = MemberDecl SrcSpan Var
type TyMemberDecl  = MemberDecl SrcSpan VarTy

data Stmt s v = DeclS s (Declaration s v)
              | ExprS s (Expr s v)
                -- | It uses type parameter v. We allow to assign only to
                -- variables. Unfortunately we have to explicitly use (s, v)
                -- here instead of 'VarS' for example, because we abstract over
                -- variables.
              | AssignS s (AssignOpS s) (s, v) (Expr s v)
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
-- Function names are represented as 'VarE'.
--
-- 'ParenE' is used for better source spans and pretty printing.
data Expr s v = LitE (LiteralS s)
              | VarE s v
              | LambdaE s [VarBinder s] (Expr s v)
                -- | We restrict member access to only functions.
              | MemberAccessE s (Expr s v) (FunNameS s)
              | MemberAccessMaybeE s (Expr s v) (FunNameS s)
                -- | Class access is just for new (constructor) right now.
              | ClassAccessE s (ClassNameS s) (FunNameS s)
              | ClassAccessStaticE s (ClassNameS s) (MemberNameS s)
              | NewRefE s (Expr s v)
                -- | This operator produces a value of type A from a value of
                -- type Ref A or Mutable A.
              | DerefE s (Expr s v)
              | BinOpE s (BinOpS s) (Expr s v) (Expr s v)
              | IfThenElseE s (Expr s v) (Expr s v) (Expr s v)
              | JustE s (Expr s v)
              | ParenE s (Expr s v)
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

type LiteralS s = (s, Literal)
type SrcLiteral = LiteralS SrcSpan

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

type BinOpS s = (s, BinOp)
type SrcBinOp = BinOpS SrcSpan

-- | Internal representation of types. What types really represent.
data Type = TyUnit
          | TyBool
          | TyInt
          | TyClass ClassName
          | TyArrow Type Type
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
             | SrcTyClass (ClassNameS s)
             | SrcTyArrow s (TypeS s) (TypeS s)
             | SrcTyMaybe s (TypeS s)
             | SrcTyMutable s (TypeS s)
             | SrcTyRef s (TypeS s)
             | SrcTyParen s (TypeS s)
  deriving Show

type SrcType = TypeS SrcSpan

-- | Converts a source representation of type to an internal one.
srcTypeToType :: TypeS s -> Type
srcTypeToType (SrcTyUnit _) = TyUnit
srcTypeToType (SrcTyBool _) = TyBool
srcTypeToType (SrcTyInt  _) = TyInt
srcTypeToType (SrcTyClass srcClassName) = TyClass $ getClassName srcClassName
srcTypeToType (SrcTyArrow _ st1 st2) = TyArrow (srcTypeToType st1) (srcTypeToType st2)

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

-- | Transforms function type which has variable binders and return type to one
-- big internal type (right associative type arrow without parameter names).
funTypeToType :: FunType s -> Type
funTypeToType (FunType _ varBinders srcRetType) = funTypeToType' varBinders
  where funTypeToType' []       = retType
        funTypeToType' (vb:vbs) = TyArrow (srcTypeToType $ getVarBinderType vb)
                                          (funTypeToType' vbs)
        retType = srcTypeToType srcRetType

-- | Name (variable) declaration.
-- Consists of var binder and an optional initialiser.
data Declaration s v = Decl s (VarBinder s) (Maybe (Init s v))
  deriving Show

type SrcDeclaration = Declaration SrcSpan Var
type TyDeclaration  = Declaration SrcSpan VarTy

-- | Initialiser expression. Uses different assignment operators.
data Init s v = Init s (InitOpS s) (Expr s v)
  deriving Show

type SrcInit = Init SrcSpan Var
type TyInit  = Init SrcSpan VarTy

-- | Assignment operators are factored out.
--
-- * Mutable (<-) is for Mutable.
--
-- * Ref (:=) is used for Ref (references).
data AssignOp = AssignMut | AssignRef
  deriving Show

type AssignOpS s = (s, AssignOp)
type SrcAssignOp = AssignOpS SrcSpan

-- | Operators used in declaration statements.
--
-- * Equal is used for immutable (default) variables.
--
-- * Mutable (<-) is for Mutable.
--
-- * Ref (:=) is used for Ref (references).
data InitOp = InitEqual | InitMut | InitRef
  deriving Show

type InitOpS s = (s, InitOp)
type SrcInitOp = InitOpS SrcSpan

-- | Modifiers are used in class member declarations.
data Modifier = Public | Private | Static
  deriving Show

type ModifierS s = (s, Modifier)
type SrcModifier = ModifierS SrcSpan

newtype Var = Var String
  deriving Show

-- | Variable annotated with its type.
newtype VarTy = VarTy (Var, Type)
  deriving Show

type VarS s = (s, Var)
type SrcVar = VarS SrcSpan

-- | Var binder is a pair of variable name and a type (in their source representations).
-- Used in function parameters.
data VarBinder s = VarBinder s (VarS s) (TypeS s)
  deriving Show

type SrcVarBinder = VarBinder SrcSpan

getVarBinderType :: VarBinder s -> TypeS s
getVarBinderType (VarBinder _ _ vbType) = vbType

setVarBinderAnn :: VarBinder s -> s -> VarBinder s
setVarBinderAnn (VarBinder _ v t) s = VarBinder s v t

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

type ClassNameS s = (s, ClassName)
type SrcClassName = ClassNameS SrcSpan

getClassName :: ClassNameS s -> ClassName
getClassName = snd

newtype FunName = FunName String
  deriving (Show, Eq, Ord)

type FunNameS s = (s, FunName)
type SrcFunName = FunNameS SrcSpan

getFunName :: FunNameS s -> FunName
getFunName = snd

-- | Denotes class field or method name.
newtype MemberName = MemberName String
  deriving Show

type MemberNameS s = (s, MemberName)
type SrcMemberName = MemberNameS SrcSpan

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

