-- | Main AST module. Defines data types and type synonyms representing syntax
-- tree and some helper functions.
--
-- We don't have source annotations for MIL because it is not user-facing
-- language.  When it comes to parsing MIL, we will probably need to change the
-- AST to make it parameterised over variable occurence type: just variable
-- after parsing and variable with its type after type checking. But for now,
-- we assume that source language compiler generates typed MIL and type
-- checking is only used to transformation checking.
--
-- newtypes are used quite extensively to have a strong distinction between
-- different types of names.
module MIL.AST where

-- | Program:
--
-- * list of type definitions
--
-- * list of type alias definitions
--
-- * list of function definitions
--
-- Note: they must be in the same order on the source level.
newtype Program = Program ([TypeDef], [AliasDef], [FunDef])
  deriving Show

getMilTypeDefs :: Program -> [TypeDef]
getMilTypeDefs (Program (typeDefs, _, _)) = typeDefs

getMilFunDefs :: Program -> [FunDef]
getMilFunDefs (Program (_, _, funDefs)) = funDefs

-- | Type definition:
--
-- * type name
--
-- * list of type variables (type parameters)
--
-- * list of constructor definitions
data TypeDef = TypeDef TypeName [TypeVar] [ConDef]
  deriving Show

-- | Constructor definition:
--
-- * constructor name
--
-- * constructor fields (expressed as types)
data ConDef = ConDef ConName [Type]
  deriving Show

-- | Type alias definition. Gives a name to a type.
data AliasDef = AliasDef TypeName Type
  deriving Show

-- | Function definition:
--
-- * function name
--
-- * function type
--
-- * expression (body)
--
-- Note: there is only one function equation and it is without patterns.
data FunDef = FunDef FunName Type Expr
  deriving Show

-- | Expression representation.
--
-- We have type (big) lambdas and type applications because we use System F as
-- a base for the type system.
--
-- 'LetE' (bind), 'ReturnE' and 'LiftE' are monadic operations.
--
-- Constructors and variables (functions) are annotated with their types.
data Expr = LitE Literal
          | VarE VarBinder
          | LambdaE VarBinder Expr
          | AppE Expr Expr
          | TypeLambdaE TypeVar Expr
          | TypeAppE Expr Type
            -- | 'ConNameE' stands on its own because constructors act as
            -- functions.
          | ConNameE ConName Type
          | NewRefE Expr
          | DerefE Expr
          | AssignRefE Expr Expr
          | LetE VarBinder Expr Expr
          | ReturnE TypeM Expr
          | LiftE Expr TypeM TypeM
            -- | 'LetRecE' has a list of definitions in order to be able to
            -- handle mutually recursive definitions.
          | LetRecE [(VarBinder, Expr)] Expr
            -- | Patterns must be exhaustive.
          | CaseE Expr [CaseAlt]
          | TupleE [Expr]
  deriving (Show, Eq)

-- | Literal constants.
data Literal = UnitLit
             | IntLit Int
             | FloatLit Double
               -- | Can be used only as global constants. Not first class.
             | StringLit String
  deriving (Show, Eq)

newtype CaseAlt = CaseAlt (Pattern, Expr)
  deriving (Show, Eq)

-- | Patterns.
data Pattern =
    -- | Literal pattern (constant).
    LitP Literal
    -- | Variable pattern. Needs to contain type.
  | VarP VarBinder
    -- | Constructor pattern. Can't be nested.
  | ConP [VarBinder]
    -- | Tuple pattern. Can't be nested.
  | TupleP [VarBinder]
    -- | Default alternative: underscore.
  | DefaultP
  deriving (Show, Eq)

-- | Types representation.
--
-- Note: we don't have data constructors for built-in types. They all are
-- handled uniformly with user-defined data types.
--
-- TODO: monads
data Type = -- | May refer to both data types and type aliases.
            TyTypeCon TypeName
          | TyVar TypeVar
          | TyArrow Type Type
          | TyForAll TypeVar Type
          | TyApp Type Type
          | TyTuple [Type]
          | TyMonad TypeM
  deriving (Show, Eq)

-- | Applies a monad type given as a first argument to the "return type"
-- (right-most type of the type arrow) of the type given as a second argument.
monadReturnType :: TypeM -> Type -> Type
monadReturnType tm (TyArrow t1 t2) = TyArrow t1 (monadReturnType tm t2)
monadReturnType tm t = TyApp (TyMonad tm) t

-- | For monadic type `m a` returns a result type `a`.
-- Note: Unsafe. Make sure you pass a monadic type.
getMonadResultType :: Type -> Type
getMonadResultType (TyApp (TyMonad {}) t) = t
getMonadResultType t = error $ "Type '" ++ show t ++ "' is not monadic"

applyMonadType :: TypeM -> Type -> Type
applyMonadType tm t = TyApp (TyMonad tm) t

-- | Monadic type. It is either a single monad or a monad on top of another
-- 'TypeM'. This represents a monad transformers stack, basically. Monad can
-- also be referred to using a type alias.
data TypeM = MTyMonad MilMonad
           | MTyMonadCons MilMonad TypeM
           | MTyAlias TypeName
  deriving (Show, Eq)

-- | "Type of the type".
-- This representation is more general that we allow in the language. Also note,
-- that we don't really support System F Omega: all type constructors must be
-- fully applied and all type variables are of kind *. There is no syntactic
-- representation of kinds in the source languages, they are used for type
-- (kind) checking.
data Kind = StarK
          | Kind :=>: Kind
  deriving (Show, Eq)

newtype VarBinder = VarBinder (Var, Type)
  deriving (Show, Eq)

getBinderVar :: VarBinder -> Var
getBinderVar (VarBinder (v,_)) = v

getBinderType :: VarBinder -> Type
getBinderType (VarBinder (_,t)) = t

newtype Var = Var String
  deriving (Show, Eq, Ord)

varToFunName :: Var -> FunName
varToFunName (Var varName) = FunName varName

newtype TypeVar = TypeVar String
  deriving (Show, Eq, Ord)

typeVarToTypeName :: TypeVar -> TypeName
typeVarToTypeName (TypeVar typeVarName) = TypeName typeVarName

newtype TypeName = TypeName String
  deriving (Show, Eq, Ord)

newtype ConName = ConName String
  deriving (Show, Eq, Ord)

newtype FunName = FunName String
  deriving (Show, Eq, Ord)

-- | Built-in monads (effects).
data MilMonad = Id
              | State
              | Error Type
              | NonTerm
              | IO
  deriving (Show, Eq)

-- | Type constructors, type variables and tuple types are atomic types.
isAtomicType :: Type -> Bool
isAtomicType (TyTypeCon {}) = True
isAtomicType (TyVar     {}) = True
isAtomicType (TyTuple   {}) = True
isAtomicType              _ = False

-- Precedences

getExprPrec :: Expr -> Int
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
getExprPrec NewRefE     {} = 6
getExprPrec DerefE      {} = 6
getExprPrec AssignRefE  {} = 4
getExprPrec TupleE      {} = 6

-- | Returns whether the first expression has a lower precedence than the
-- second one. Convenient to use in infix form.
--
-- Note: It is reflexive: e `exprHasLowerPrec` e ==> True
exprHasLowerPrec :: Expr -> Expr -> Bool
exprHasLowerPrec e1 e2 = getExprPrec e1 <= getExprPrec e2

-- | Returns whether the first expression has a lower precedence than the
-- second one. Convenient to use in infix form.
-- This version can be used with associative operators, for example: arithmetic
-- operations, function application. See "MIL.AST.PrettyPrinter".
--
-- Note: It is *not* reflexive: e `exprHasLowerPrecAssoc` e ==> False
exprHasLowerPrecAssoc :: Expr -> Expr -> Bool
exprHasLowerPrecAssoc e1 e2 = getExprPrec e1 < getExprPrec e2

getTypePrec :: Type -> Int
getTypePrec (TyTypeCon {}) = 5
getTypePrec (TyVar     {}) = 5
getTypePrec (TyArrow   {}) = 2
getTypePrec (TyForAll  {}) = 1
getTypePrec (TyApp     {}) = 4
getTypePrec (TyTuple   {}) = 5
-- Dirty hacking for nice pretty printing:
-- Atomic monad doesn't get parentheses, but cons does.
getTypePrec (TyMonad (MTyMonad {})) = 4
getTypePrec (TyMonad (MTyMonadCons {})) = 3
getTypePrec (TyMonad (MTyAlias {})) = 5

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

-- * Smart constructors

mkTypeVar :: String -> Type
mkTypeVar = TyVar . TypeVar

mkSimpleType :: String -> Type
mkSimpleType typeName = TyTypeCon (TypeName typeName)

-- | Constructs a kind from an integer that denotes the number of parameters of
-- a type constructor
mkKind :: Int -> Kind
mkKind 0 = StarK
mkKind n = StarK :=>: mkKind (n - 1)

