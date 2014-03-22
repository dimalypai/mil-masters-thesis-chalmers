module OOLang.AST where

newtype Program = Program ([ClassDef], [FunDef])
  deriving Show

data ClassDef = ClassDef ClassName (Maybe ClassName) [MemberDecl]
  deriving Show

data FunDef = FunDef FunName FunType [Stmt] Bool
  deriving Show

data MemberDecl = FieldDecl Declaration [Modifier]
                | MethodDecl FunDef [Modifier]
  deriving Show

data Stmt = DeclS Declaration
          | ExprS Expr
          | AssignS AssignOp Expr Expr
          | WhileS Expr [Stmt]
          | WhenS Expr [Stmt] [Stmt]
          | ReturnS Expr
  deriving Show

data Expr = LitE Literal
          | VarE Var
          | FunNameE FunName
          | LambdaE [VarBinder] Expr
          | ClassAccessE ClassName Expr
          | ClassAccessStaticE ClassName Expr
          | DerefE Expr
          | BinOpE BinOp Expr Expr
          | IfThenElseE Expr Expr Expr
          | JustE Expr
  deriving Show

data Literal = UnitLit
             | BoolLit Bool
             | IntLit Int
             | NothingLit
  deriving Show

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

data Type = TyUnit
          | TyBool
          | TyInt
          | TyClass ClassName
          | TyArrow Type Type
          | TyMaybe Type
          | TyMutable Type
          | TyRef Type
  deriving Show

data FunType = FunType [VarBinder] Type
  deriving Show

data Declaration = Decl VarBinder (Maybe Init)
  deriving Show

data Init = Init AssignOp Expr
  deriving Show

data AssignOp = AssignEqual | AssignMut | AssignRef
  deriving Show

data Modifier = Public | Private | Static
  deriving Show

newtype Var = Var String
  deriving Show

newtype VarBinder = VarBind (Var, Type)
  deriving Show

newtype ClassName = ClassName String
  deriving Show

newtype FunName = FunName String
  deriving Show

