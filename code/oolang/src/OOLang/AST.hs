-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- AST is parameterised. Some of the data types have only one type parameter s
-- (stands for source) and some have two - v (stands for variable) and s. The
-- reason is that variable occurences are parameterised and are represented
-- differently at different stages. For more on this look at the 'Expr' and
-- 'Stmt' data types (these are the only places where a field of type v is
-- present). Thus, some of the data types eventually contain 'Expr' or 'Stmt'
-- (and therefore must have both v and s) and some of them don't (and have only
-- s).
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
data Program v s = Program s [ClassDef v s] [FunDef v s]
  deriving Show

type SrcProgram = Program Var   SrcSpan
type TyProgram  = Program VarTy SrcSpan

-- | Class definition:
--
-- * source annotation
--
-- * class name
--
-- * super class name (maybe)
--
-- * list of member declarations.
data ClassDef v s = ClassDef s (ClassNameS s) (Maybe (ClassNameS s)) [MemberDecl v s]
  deriving Show

type SrcClassDef = ClassDef Var   SrcSpan
type TyClassDef  = ClassDef VarTy SrcSpan

-- | Function definition:
--
-- * source annotation
--
-- * function name
--
-- * function type
--
-- * list of statements (body) - not empty
data FunDef v s = FunDef s (FunNameS s) (FunType s) [Stmt v s]
  deriving Show

type SrcFunDef = FunDef Var   SrcSpan
type TyFunDef  = FunDef VarTy SrcSpan

-- | Class member declaration. Either field or method.
-- Field declaration is just a declaration with modifiers (syntactically).
-- Method declaration is just a function with modifiers (syntactically).
data MemberDecl v s = FieldDecl s (Declaration v s) [ModifierS s]
                    | MethodDecl s (FunDef v s) [ModifierS s]
  deriving Show

type SrcMemberDecl = MemberDecl Var   SrcSpan
type TyMemberDecl  = MemberDecl VarTy SrcSpan

data Stmt v s = DeclS s (Declaration v s)
              | ExprS s (Expr v s)
                -- | It uses type parameter v. We allow to assign only to
                -- variables. Unfortunately we have to explicitly use (v, s)
                -- here instead of 'VarS' for example, because we abstract over
                -- variables.
              | AssignS s (AssignOpS s) (v, s) (Expr v s)
              | WhileS s (Expr v s) [Stmt v s]
              | WhenS s (Expr v s) [Stmt v s] [Stmt v s]
              | ReturnS s (Expr v s)
  deriving Show

type SrcStmt = Stmt Var   SrcSpan
type TyStmt  = Stmt VarTy SrcSpan

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
data Expr v s = LitE (LiteralS s)
              | VarE s v
              | LambdaE s [VarBinder s] (Expr v s)  -- ^ Not empty.
                -- | We restrict member access to only functions.
              | MemberAccessE s (Expr v s) (FunNameS s)
              | MemberAccessMaybeE s (Expr v s) (FunNameS s)
                -- | Class access is just for new (constructor) right now.
              | ClassAccessE s (ClassNameS s) (FunNameS s)
              | ClassAccessStaticE s (ClassNameS s) (MemberNameS s)
              | NewRefE s (Expr v s)
                -- | This operator produces a value of type A from a value of
                -- type Ref A or Mutable A.
              | DerefE s (Expr v s)
              | BinOpE s (BinOpS s) (Expr v s) (Expr v s)
              | IfThenElseE s (Expr v s) (Expr v s) (Expr v s)
              | JustE s (Expr v s)
              | ParenE s (Expr v s)
  deriving Show

type SrcExpr = Expr Var   SrcSpan
type TyExpr  = Expr VarTy SrcSpan

-- | Literal constants.
data Literal = UnitLit
             | BoolLit Bool
             | IntLit Int
             | FloatLit Double String  -- ^ The user string (for displaying).
             | StringLit String
             | NothingLit
  deriving Show

type LiteralS s = (Literal, s)
type SrcLiteral = LiteralS SrcSpan

getLiteral :: LiteralS s -> Literal
getLiteral = fst

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

getBinOp :: BinOpS s -> BinOp
getBinOp = fst

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

-- | Entity of this type is either an already computed value or a global
-- function without arguments.
--
-- For example, function parameter of type Unit has value type (it can be only
-- unit value, since we are in a strict language) and global function main :
-- Unit has value type, but it denotes a computation returning unit, not an
-- already computed value.
isValueType :: Type -> Bool
isValueType TyArrow {} = False
isValueType _ = True

-- | It can be either a function which has type, for example Pure Int (it
-- doesn't have arguments and purely computes a value of type Int) or a
-- function with arguments like Int -> Float -> Pure Int, which takes arguments
-- and its return type signals that it is a pure function that delivers and
-- integer value.
isPureFunType :: Type -> Bool
isPureFunType (TyPure _) = True
isPureFunType (TyArrow _ t2) = isPureFunType t2
isPureFunType _ = False

isAtomicType :: Type -> Bool
isAtomicType TyArrow   {} = False
isAtomicType TyPure    {} = False
isAtomicType TyMaybe   {} = False
isAtomicType TyMutable {} = False
isAtomicType TyRef     {} = False
isAtomicType            _ = True

getTypePrec :: Type -> Int
getTypePrec TyUnit       = 3
getTypePrec TyBool       = 3
getTypePrec TyInt        = 3
getTypePrec TyFloat      = 3
getTypePrec TyString     = 3
getTypePrec TyClass   {} = 3
getTypePrec TyArrow   {} = 1
getTypePrec TyPure    {} = 2
getTypePrec TyMaybe   {} = 2
getTypePrec TyMutable {} = 2
getTypePrec TyRef     {} = 2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
--
-- Note: It is reflexive: t `typeHasLowerPrec` t ==> True
typeHasLowerPrec :: Type -> Type -> Bool
typeHasLowerPrec t1 t2 = getTypePrec t1 <= getTypePrec t2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
-- This version can be used with associative type operators, for example:
-- arrow, type application. See "MIL.AST.PrettyPrinter".
--
-- Note: It is *not* reflexive: t `typeHasLowerPrecAssoc` t ==> False
typeHasLowerPrecAssoc :: Type -> Type -> Bool
typeHasLowerPrecAssoc t1 t2 = getTypePrec t1 < getTypePrec t2

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

getFunParams :: FunType s -> [VarBinder s]
getFunParams (FunType _ varBinders _) = varBinders

getFunReturnType :: FunType s -> TypeS s
getFunReturnType (FunType _ _ retType) = retType

-- | Name (variable) declaration.
-- Consists of var binder and an optional initialiser.
data Declaration v s = Decl s (VarBinder s) (Maybe (Init v s))
  deriving Show

type SrcDeclaration = Declaration Var   SrcSpan
type TyDeclaration  = Declaration VarTy SrcSpan

-- | Initialiser expression. Uses different assignment operators.
data Init v s = Init s (InitOpS s) (Expr v s)
  deriving Show

type SrcInit = Init Var   SrcSpan
type TyInit  = Init VarTy SrcSpan

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
-- * Ref (:=) is used for Ref (references).
data InitOp = InitEqual | InitMut | InitRef
  deriving Show

type InitOpS s = (InitOp, s)
type SrcInitOp = InitOpS SrcSpan

getInitOp :: InitOpS s -> InitOp
getInitOp = fst

-- | Modifiers are used in class member declarations.
data Modifier = Public | Private | Static
  deriving Show

type ModifierS s = (Modifier, s)
type SrcModifier = ModifierS SrcSpan

newtype Var = Var String
  deriving (Show, Eq, Ord)

type VarS s = (Var, s)
type SrcVar = VarS SrcSpan

getVar :: VarS s -> Var
getVar = fst

varToFunName :: Var -> FunName
varToFunName (Var varName) = FunName varName

-- | Variable annotated with its type.
newtype VarTy = VarTy (Var, Type)
  deriving Show

-- | Var binder is a pair of variable name and a type (in their source representations).
-- Used in function parameters.
data VarBinder s = VarBinder s (VarS s) (TypeS s)
  deriving Show

type SrcVarBinder = VarBinder SrcSpan

getBinderVar :: VarBinder s -> VarS s
getBinderVar (VarBinder _ srcVar _ ) = srcVar

getBinderType :: VarBinder s -> TypeS s
getBinderType (VarBinder _ _ srcType) = srcType

setVarBinderAnn :: VarBinder s -> s -> VarBinder s
setVarBinderAnn (VarBinder _ v t) s = VarBinder s v t

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

type ClassNameS s = (ClassName, s)
type SrcClassName = ClassNameS SrcSpan

getClassName :: ClassNameS s -> ClassName
getClassName = fst

newtype FunName = FunName String
  deriving (Show, Eq, Ord)

type FunNameS s = (FunName, s)
type SrcFunName = FunNameS SrcSpan

getFunName :: FunNameS s -> FunName
getFunName = fst

-- | Denotes class field or method name.
newtype MemberName = MemberName String
  deriving Show

type MemberNameS s = (MemberName, s)
type SrcMemberName = MemberNameS SrcSpan

-- Parsing helpers

-- | Used only in parsing to allow to mix class and function definitions and
-- then have them reordered in the AST.
data TopDef v s = TopClassDef { getClassDef :: ClassDef v s }
                | TopFunDef   { getFunDef   :: FunDef v s   }

type SrcTopDef = TopDef Var SrcSpan

isClassDef :: TopDef v s -> Bool
isClassDef (TopClassDef _) = True
isClassDef               _ = False

isFunDef :: TopDef v s -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

