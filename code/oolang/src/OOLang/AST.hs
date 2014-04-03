module OOLang.AST where

import OOLang.SrcSpan

data Program s v = Program s [ClassDef s v] [FunDef s v]
  deriving Show

type SrcProgram = Program SrcSpan Var
type TyProgram  = Program SrcSpan VarTy

data ClassDef s v = ClassDef s (SrcClassName s) (Maybe (SrcClassName s)) [MemberDecl s v]
  deriving Show

type SrcClassDef = ClassDef SrcSpan Var
type TyClassDef  = ClassDef SrcSpan VarTy

data FunDef s v = FunDef s (SrcFunName s) (SrcFunType s) [Stmt s v] Bool
  deriving Show

type SrcFunDef = FunDef SrcSpan Var
type TyFunDef  = FunDef SrcSpan VarTy

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

data Literal = UnitLit
             | BoolLit Bool
             | IntLit Int
             | FloatLit Double String  -- the user string
             | StringLit String
             | NothingLit
  deriving Show

type SrcLiteral s = (s, Literal)

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

data Type = TyUnit
          | TyBool
          | TyInt
          | TyClass ClassName
          | TyArrow Type Type
          | TyMaybe Type
          | TyMutable Type
          | TyRef Type
  deriving Show

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

data SrcFunType s = SrcFunType s [VarBinder s] (SrcType s)
  deriving Show

data Declaration s v = Decl s (VarBinder s) (Maybe (Init s v))
  deriving Show

data Init s v = Init s (SrcAssignOp s) (Expr s v)
  deriving Show

data AssignOp = AssignEqual | AssignMut | AssignRef
  deriving Show

type SrcAssignOp s = (s, AssignOp)

data Modifier = Public | Private | Static
  deriving Show

type SrcModifier s = (s, Modifier)

newtype Var = Var String
  deriving Show

newtype VarTy = VarTy (Var, Type)
  deriving Show

type SrcVar s = (s, Var)

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

data TopDef s v = TopClassDef { getClassDef :: ClassDef s v }
                | TopFunDef   { getFunDef   :: FunDef s v   }

type SrcTopDef = TopDef SrcSpan Var

isClassDef :: TopDef s v -> Bool
isClassDef (TopClassDef _) = True
isClassDef               _ = False

isFunDef :: TopDef s v -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

