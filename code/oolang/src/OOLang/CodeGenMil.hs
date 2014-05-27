{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
--
-- All generated code operates in two monads: Pure_M for pure computations and
-- Impure_M for default (impure) computations.
-- Statement sequences are represented with monadic binds.
-- Basically, all expressions (even the simplest ones, like literals and
-- variables) result in some sequence of binds (possibly empty) and return. We
-- give fresh names to subexpressions and introduce sequencing.
module OOLang.CodeGenMil
  ( codeGen
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
-- 'second' is used just to transform components of a pair
import Control.Arrow (second)

import OOLang.AST
import OOLang.AST.TypeAnnotated
import OOLang.AST.Helpers
import OOLang.TypeChecker
import OOLang.TypeChecker.TypeEnv
import OOLang.Utils
import qualified MIL.AST as MIL
import qualified MIL.BuiltIn as MIL
import qualified MIL.Transformations.IdExprMonadElimination as MIL

-- | Entry point to the code generator.
-- Takes a type checked program in OOLang and a type environment and produces a
-- program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.Program
codeGen tyProgram typeEnv = runReaderFrom (typeEnv, Map.empty) $ evalStateTFrom 0 (runCG $ codeGenProgram tyProgram)

-- | Code generation monad. Uses 'StateT' for providing fresh variable names
-- and 'Reader' for querying the type environment and class types.
newtype CodeGenM a = CG { runCG :: StateT NameSupply (Reader (TypeEnv, ClassTypes)) a }
  deriving (Monad, MonadState NameSupply, MonadReader (TypeEnv, ClassTypes), Functor, Applicative)

-- | All field and method names accessible from the class and a MIL type of the
-- object representation.
type ClassTypes = Map.Map ClassName ([Var], [FunName], MIL.Type)

-- | A counter for generating unique variable names.
type NameSupply = Int

-- | Entry point into the type checking of the program.
-- There is a list of MIL functions generated for each class definition.
-- TODO: type aliases
-- There is an MIL function generated for each function definition.
-- All these definitions are then regrouped and the built-ins are added.
codeGenProgram :: TyProgram -> CodeGenM MIL.Program
codeGenProgram (Program _ tyClassDefs tyFunDefs) = do
  classTypes <- asks snd
  classTypes' <- foldM (\cts (ClassDef _ srcClassName _ _) ->
    collectClassTypes (getClassName srcClassName) cts)
    classTypes tyClassDefs
  local (second $ const classTypes') $ do
    classMilFunDefs <- concat <$> mapM codeGenClassDef tyClassDefs
    milFunDefs <- mapM codeGenFunDef tyFunDefs
    return $ MIL.Program (builtInTypeDefs, builtInAliasDefs, classMilFunDefs ++ milFunDefs)

-- | Collects information about the class: field and methods names. Constructs
-- a type of the object representation.
collectClassTypes :: ClassName -> ClassTypes -> CodeGenM ClassTypes
collectClassTypes className classTypes = do
  classTypeEnv <- asks (getClassTypeEnv . fst)
  let fields = getClassFieldsAssoc className classTypeEnv
      methods = getClassMethodsAssoc className classTypeEnv
      (fieldNames, fieldTypes) = unzip fields
      (methodNames, methodTypes) = unzip methods
      -- TODO: virtual methods and self parameters
      classTupleType = MIL.TyTuple [ MIL.TyTuple $ map typeMil fieldTypes
                                   , MIL.TyTuple $ map typeMil methodTypes]
  return $ Map.insert className (fieldNames, methodNames, classTupleType) classTypes

codeGenClassDef :: TyClassDef -> CodeGenM [MIL.FunDef]
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) = do
  let (_, tyMethodDecls) = partitionClassMembers tyMembers
  classMemberFunDefs <- mapM codeGenClassMethod tyMethodDecls
  return $ classMemberFunDefs

-- | TODO: inits (together with constructor).
codeGenClassField :: ClassName -> TyFieldDecl -> CodeGenM MIL.Type
codeGenClassField className (FieldDecl _ tyDecl _) = do
  let fieldName = getVar $ getDeclVarName tyDecl
  fieldType <- asks (getClassFieldType className fieldName . getClassTypeEnv . fst)
  return $ typeMil fieldType

-- | TODO: add method specifics.
codeGenClassMethod :: TyMethodDecl -> CodeGenM MIL.FunDef
codeGenClassMethod (MethodDecl _ tyFunDef _) = codeGenFunDef tyFunDef

codeGenFunDef :: TyFunDef -> CodeGenM MIL.FunDef
codeGenFunDef (FunDef _ srcFunName tyFunType tyStmts) = do
  let funName = getFunName srcFunName
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv . fst)
  let isPure = isPureFunType funType
  let funMonad = if isPure
                   then pureMonad
                   else impureMonad
  let milFunType = funTypeMil funType
  funBody <- codeGenStmts tyStmts funMonad
  let funParams = getFunParams tyFunType
  let funBodyWithParams = foldr (\tyVarBinder e ->
                                   MIL.LambdaE ( MIL.VarBinder (varMil (getVar $ getBinderVar tyVarBinder)
                                               , typeMil $ getTypeOf tyVarBinder)) e)
                            funBody funParams
  return $ MIL.FunDef (funNameMil funName) milFunType funBodyWithParams

-- | List of statements is not empty.
-- Takes a monad of the containing function.
--
-- Declaration statement needs a special treatment to get variable scope right.
codeGenStmts :: [TyStmt] -> MIL.TypeM -> CodeGenM MIL.Expr

codeGenStmts [DeclS _ decl] funMonad =
  codeGenDecl decl funMonad (MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit))
codeGenStmts [tyStmt] funMonad = codeGenStmt tyStmt funMonad

codeGenStmts ((DeclS _ decl):tyStmts) funMonad = do
  milBodyExpr <- codeGenStmts tyStmts funMonad
  codeGenDecl decl funMonad milBodyExpr
codeGenStmts (tyStmt:tyStmts) funMonad = do
  milBindExpr <- codeGenStmt tyStmt funMonad
  -- See Note [Variable type in bind].
  let milBindExprType = typeMil $ stripPureType $ getTypeOf tyStmt
  milBodyExpr <- codeGenStmts tyStmts funMonad
  return $ MIL.LetE (MIL.VarBinder (MIL.Var "_", milBindExprType))
             milBindExpr
             milBodyExpr

-- | Takes a monad of the containing function.
codeGenStmt :: TyStmt -> MIL.TypeM -> CodeGenM MIL.Expr
codeGenStmt tyStmt funMonad =
  case tyStmt of
    ExprS _ tyExpr -> codeGenExpr tyExpr funMonad

    -- TODO:
    -- + "then" with state putting operations
    -- + ANF/SSA construction
    AssignS _ srcAssignOp tyExprLeft tyExprRight _ -> undefined

    DeclS {} -> error "codeGenStmt: DeclS should have a special treatment."

-- | Code generation for declarations.
-- It takes an expression which will become a body of the monadic bind, where a
-- declared variable will be in scope.
codeGenDecl :: TyDeclaration -> MIL.TypeM -> MIL.Expr -> CodeGenM MIL.Expr
codeGenDecl (Decl _ tyVarBinder mTyInit _) funMonad milBodyExpr = do
  milInitExpr <- case mTyInit of
                  Just tyInit -> codeGenExpr (getInitExpr tyInit) funMonad
                  -- It may be a variable with Mutable type, so we need
                  -- 'getUnderType'.
                  Nothing -> codeGenExpr (maybeDefaultExpr $ getUnderType $ getTypeOf tyVarBinder) funMonad
  return $ MIL.LetE ( MIL.VarBinder (varMil (getVar $ getBinderVar tyVarBinder)
                    , typeMil $ getTypeOf tyVarBinder))
             milInitExpr
             milBodyExpr

-- | Expression code generation.
-- Takes a monad of the containing function.
codeGenExpr :: TyExpr -> MIL.TypeM -> CodeGenM MIL.Expr
codeGenExpr tyExpr funMonad =
  case tyExpr of
    LitE lit -> return $ MIL.ReturnE funMonad (literalMil lit)

    VarE _ varType var varPure -> do
      let varE = MIL.VarE $ MIL.VarBinder (varMil var, typeMil varType)
      case (isValueType varType, isPureFunType varType, funMonad == pureMonad) of
        -- It is a function, so to make it monadic value, we need return.
        (False, _, _) -> return $ MIL.ReturnE funMonad varE
        -- It is an impure value type inside a pure function, so it is a local
        -- variable or parameter and it is pure, we need return.
        (True, False, True) -> return $ MIL.ReturnE funMonad varE
        -- Pure_M monad value inside a pure or impure function.
        (True, True, _) -> return varE
        -- It can be an impure value inside an impure function (must be global
        -- impure function), then use 'funTypeMil' for type annotation, or it
        -- can be a local variable, look at its purity annotation.
        (True, False, False) -> if varPure
                                  then return $ MIL.ReturnE funMonad varE
                                  else return $ MIL.VarE $ MIL.VarBinder (varMil var, funTypeMil varType)

    BinOpE _ resultType srcBinOp tyExpr1 tyExpr2 _ ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 resultType funMonad

    ParenE _ tySubExpr -> codeGenExpr tySubExpr funMonad

literalMil :: TyLiteral -> MIL.Expr
literalMil UnitLit {} = MIL.LitE MIL.UnitLit
literalMil (BoolLit _ _ b) =
  if b
    then MIL.ConNameE (MIL.ConName "True")  (MIL.mkSimpleType "Bool")
    else MIL.ConNameE (MIL.ConName "False") (MIL.mkSimpleType "Bool")
literalMil (IntLit _ _ i)     = MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ _ f _) = MIL.LitE (MIL.FloatLit f)
literalMil (StringLit _ _ s)  = MIL.LitE (MIL.StringLit s)
literalMil (NothingLit _ t _) =
  -- Monomorphise Nothing constructor.
  MIL.TypeAppE
    (MIL.ConNameE (MIL.ConName "Nothing")
       (MIL.TyForAll (MIL.TypeVar "A") $
          MIL.TyApp (MIL.TyTypeCon $ MIL.TypeName "Maybe") (MIL.mkTypeVar "A")))
    (typeMil $ getMaybeUnderType t)

-- | Takes a monad of the containing function.
codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> Type -> MIL.TypeM -> CodeGenM MIL.Expr
codeGenBinOp App tyExpr1 tyExpr2 resultType funMonad = do
  milExpr1 <- codeGenExpr tyExpr1 funMonad
  milExpr2 <- codeGenExpr tyExpr2 funMonad
  var1 <- newMilVar
  var2 <- newMilVar
  -- See Note [Variable type in bind].
  let var1Type = typeMil $ stripPureType $ getTypeOf tyExpr1
  let var2Type = typeMil $ stripPureType $ getTypeOf tyExpr2
  let appE = MIL.AppE (MIL.VarE $ MIL.VarBinder (var1, var1Type))
                      (MIL.VarE $ MIL.VarBinder (var2, var2Type))
  return $ MIL.LetE (MIL.VarBinder (var1, var1Type))
             milExpr1
             (MIL.LetE (MIL.VarBinder (var2, var2Type))
                milExpr2
                (case (isValueType resultType, isPureFunType resultType, funMonad == pureMonad) of
                   -- It is a partially applied function, so to make it monadic
                   -- value, we need return.
                   (False, _, _) -> MIL.ReturnE funMonad appE
                   --(True, False, True) -> MIL.ReturnE funMonad appE  -- TODO: should this be possible?
                   -- Fully applied Pure function inside a pure or impure function.
                   (True, True, _) -> appE
                   -- Fully applied impure function inside an impure function.
                   (True, False, False) -> appE))

-- | Note [Variable type in bind]:
--
-- In bind, the variable type should be "atomic" (non-monadic), so we must get
-- rid of 'TyPure'.

-- * Type conversions

-- | Note [Type transformation]:
--
-- There are two versions of type transformation: 'typeMil' and 'funTypeMil'.
-- Usually, on the top-level, 'typeMil' should be used, except for the cases,
-- where it is known that the type being transformed is a type of the function.
-- Another case, where 'funTypeMil' is used is when it is known that the type
-- being transformed is the type of the impure value.
--
-- 'typeMil' introduces Impure_M only for function return type.
-- 'funTypeMil' introduces Impure_M also for atomic types, which are not Pure.
--
-- It is important, that 'funTypeMil' recursive call is used only on the
-- left-hand side of the function arrow, since this is the only place, where
-- Impure_M should be introduced, when 'funTypeMil' is not a top-level call (in
-- this case, atomic types will get Impure_M as well).

-- | See Note [Type transformation].
typeMil :: Type -> MIL.Type
typeMil TyUnit   = MIL.unitType
typeMil TyBool   = MIL.mkSimpleType "Bool"
typeMil TyInt    = MIL.intType
typeMil TyFloat  = MIL.floatType
typeMil TyString = MIL.stringType
typeMil (TyClass className) = MIL.TyTypeCon $ typeNameMil className
typeMil (TyArrow t1 t2)     = MIL.TyArrow (typeMil t1) (funTypeMil t2)
typeMil (TyPure t)          = MIL.TyApp (MIL.TyMonad pureMonad) (typeMil t)
typeMil (TyMaybe t)         = MIL.TyApp (MIL.TyTypeCon $ MIL.TypeName "Maybe") (typeMil t)
typeMil (TyMutable t)       = typeMil t

-- | See Note [Type transformation].
funTypeMil :: Type -> MIL.Type
funTypeMil (TyArrow t1 t2) = MIL.TyArrow (typeMil t1) (funTypeMil t2)
funTypeMil (TyPure t)      = MIL.TyApp (MIL.TyMonad pureMonad) (typeMil t)
funTypeMil (TyMaybe t)     = MIL.TyApp (MIL.TyTypeCon $ MIL.TypeName "Maybe") (typeMil t)
funTypeMil (TyMutable t)   = typeMil t
funTypeMil               t = MIL.TyApp (MIL.TyMonad impureMonad) (typeMil t)

-- * Conversion utils

typeNameMil :: ClassName -> MIL.TypeName
typeNameMil (ClassName classNameStr) = MIL.TypeName classNameStr

conNameMil :: ClassName -> MIL.ConName
conNameMil (ClassName classNameStr) = MIL.ConName classNameStr

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

-- * Built-ins

builtInTypeDefs :: [MIL.TypeDef]
builtInTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Bool") [] [ MIL.ConDef (MIL.ConName "True")  []
                                         , MIL.ConDef (MIL.ConName "False") []]
  , MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkTypeVar "A"]]
  ]

builtInAliasDefs :: [MIL.AliasDef]
builtInAliasDefs =
  [ MIL.AliasDef pureMonadName   $ MIL.TyMonad pureMonadType
  , MIL.AliasDef impureMonadName $ MIL.TyMonad impureMonadType]

maybeDefaultExpr :: Type -> TyExpr
maybeDefaultExpr t@(TyRef mt) = NewRefE undefined t (LitE $ NothingLit undefined mt undefined)
maybeDefaultExpr t = LitE $ NothingLit undefined t undefined

-- * Monads

pureMonadName :: MIL.TypeName
pureMonadName = MIL.TypeName "Pure_M"

pureMonad :: MIL.TypeM
pureMonad = MIL.MTyAlias pureMonadName

pureMonadType :: MIL.TypeM
pureMonadType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonad MIL.NonTerm

impureMonadName :: MIL.TypeName
impureMonadName = MIL.TypeName "Impure_M"

impureMonad :: MIL.TypeM
impureMonad = MIL.MTyAlias impureMonadName

impureMonadType :: MIL.TypeM
impureMonadType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonadCons MIL.NonTerm $
      MIL.MTyMonadCons MIL.State $
        MIL.MTyMonad MIL.IO

exceptionType :: MIL.Type
exceptionType = MIL.unitType

-- * CodeGenM operations

newMilVar :: CodeGenM MIL.Var
newMilVar = do
  i <- get
  modify (+1)
  return $ MIL.Var ("var_" ++ show i)

