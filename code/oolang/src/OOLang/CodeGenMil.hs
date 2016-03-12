{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module responsible for MIL code generation.
--
-- All generated code operates in two monads: one for pure computations and
-- another one for default (impure) computations.
-- Statement sequences are represented with monadic binds.
-- Basically, all expressions (even the simplest ones, like literals and
-- variables) result in some sequence of binds (possibly empty) and return. We
-- give fresh names to subexpressions and introduce sequencing.
module OOLang.CodeGenMil
  ( codeGen
  , monadErrorTypeCons
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
-- Used just to transform components of a pair
import Control.Arrow (first, second)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (sortBy, find)

import OOLang.AST
import OOLang.AST.TypeAnnotated
import OOLang.AST.Helpers
import OOLang.TypeChecker
import OOLang.TypeChecker.TypeEnv hiding
  ( getSuperClass
  , getClassFieldsAssoc
  , getClassMethodsAssoc
  , isClassMethodDefined
  , getClassMethodType
  )
import qualified OOLang.TypeChecker.TypeEnv as TypeEnv
  ( getSuperClass
  , getClassFieldsAssoc
  , getClassMethodsAssoc
  , isClassMethodDefined
  , getClassMethodType
  )
import OOLang.BuiltIn
import OOLang.Utils
import qualified MIL.AST as MIL
import qualified MIL.AST.Builder as MIL
import qualified MIL.AST.Helpers as MIL
import qualified MIL.BuiltIn as MIL

import System.IO.Unsafe

-- | Entry point to the code generator.
-- Takes a type checked program in OOLang and a type environment and produces a
-- source program in MIL.
codeGen :: TyProgram -> TypeEnv -> MIL.SrcProgram
codeGen tyProgram typeEnv = unsafePerformIO $ runReaderT (evalStateTFrom (0, Map.empty, Map.empty) (runCG $ codeGenProgram tyProgram)) typeEnv

-- | Code generation monad. Uses 'StateT' for providing fresh variable names,
-- names for variable occurences (for Mutable assignments) and to keep
-- generated code for class field initialisation. 'Reader' is used for querying
-- the type environment.  'IO' may be used for debug printing.
newtype CodeGenM a = CG { runCG :: StateT (NameSupply, VarMap, ClassMap) (ReaderT TypeEnv IO) a }
  deriving (Monad, MonadState (NameSupply, VarMap, ClassMap), MonadReader TypeEnv, Functor, Applicative, MonadIO)

-- | Keeps an MIL expression of field initialisation for a class.
type ClassMap = Map.Map ClassName MIL.SrcExpr

-- | A counter for generating unique variable names.
type NameSupply = Int

-- | A map to keep track of the last occurence of a variable
-- and generate fresh names for new occurences. Used for assignments.
type VarMap = Map.Map Var Int

-- | Entry point into the code generation of a program.
-- There is a list of MIL functions generated for each
-- class definition (in order: from base to subclasses).
-- There is an MIL function generated for each function definition.
-- All these definitions are then regrouped and the built-ins are added.
codeGenProgram :: TyProgram -> CodeGenM MIL.SrcProgram
codeGenProgram (Program _ tyClassDefs tyFunDefs) = do
  sortedTyClassDefs <- sortClassesByHierarchyDepth tyClassDefs
  classMilFunDefs <- concat <$> mapM codeGenClassDef sortedTyClassDefs
  milFunDefs <- mapM codeGenFunDef tyFunDefs
  return $ MIL.Program (builtInMilTypeDefs, builtInMilFunDefs ++ (classMilFunDefs ++ milFunDefs))

-- | Code must be generated for base classes first and then for all the
-- subsequent subclasses. This is done, because of usage of field
-- initialisation code of superclasses in subclasses.
sortClassesByHierarchyDepth :: [TyClassDef] -> CodeGenM [TyClassDef]
sortClassesByHierarchyDepth tyClassDefs = do
  depths <- mapM getClassDefHierarchyDepth tyClassDefs
  let tyClassDefsWithDepths = zip tyClassDefs depths
  return (map fst $ sortBy hierarchyDepthComparison tyClassDefsWithDepths)

-- | Class depth is a number of super classes for a class.
-- Class that has no super class has depth 0.
getClassDefHierarchyDepth :: TyClassDef -> CodeGenM Int
getClassDefHierarchyDepth (ClassDef _ srcClassName _ _) = do
  let className = getClassName srcClassName
  getClassDepth className 0

getClassDepth :: ClassName -> Int -> CodeGenM Int
getClassDepth className depthAcc = do
  mSuperClassName <- getSuperClass className
  case mSuperClassName of
    Just superClassName -> getClassDepth superClassName (depthAcc + 1)
    Nothing -> return depthAcc

hierarchyDepthComparison :: (TyClassDef, Int) -> (TyClassDef, Int) -> Ordering
hierarchyDepthComparison (_, depth1) (_, depth2) = compare depth1 depth2

-- | There are three function definitions generated for every class definition:
-- data constructor, class constructor and a function for class definition.
-- Data constructor is a function that contains field initialisation code and
-- produces a tuple of class data fields.
-- Class constructor is a function that will be used in object construction
-- expressions, so it basically constructs an object of this class by
-- constructing the data part and using class definition part.
-- Class definition function takes a data part and produces an object using
-- full class definition (methods).
-- Class type is a tuple of two tuples: first - data fields, second - methods.
-- All of them include things from all the super classes.
codeGenClassDef :: TyClassDef -> CodeGenM [MIL.SrcFunDef]
codeGenClassDef (ClassDef _ srcClassName mSuperSrcClassName tyMembers) = do
  let className = getClassName srcClassName
  let mSuperClassName = getClassName <$> mSuperSrcClassName
  let (tyFieldDecls, tyMethodDecls) = partitionClassMembers tyMembers
  classDataConstructor <- codeGenClassDataConstructor className mSuperClassName tyFieldDecls
  classConstructor <- codeGenClassConstructor className
  classFunDef <- codeGenClassFunDef className tyMethodDecls
  let classMilFunDefs = [ classDataConstructor
                        , classConstructor
                        , classFunDef
                        ]
  return classMilFunDefs

codeGenClassDataConstructor :: ClassName -> Maybe ClassName -> [TyFieldDecl] -> CodeGenM MIL.SrcFunDef
codeGenClassDataConstructor className mSuperClassName tyFieldDecls = do
  classFieldsType <- getClassFieldsType className
  fieldDeclsExpr <- codeGenClassFieldDecls className mSuperClassName tyFieldDecls
  return $ MIL.FunDef (classDataConstructorNameMil className)
    (MIL.SrcTyApp pureSrcMonadMil classFieldsType) fieldDeclsExpr

-- | Code generation of field initialisation and class data construction.
-- Produces a sequence of bind expressions (field initialisations only for
-- fields defined in this class) with class data constructor application at the
-- bottom. Then gets field initialisation code for a super class (if any) from
-- the ClassMap and replaces the bottom part (class data constructor
-- application) with generated code for this class.
-- Puts the result in the ClassMap to be used in subclasses if there are any.
codeGenClassFieldDecls :: ClassName -> Maybe ClassName -> [TyFieldDecl] -> CodeGenM MIL.SrcExpr
codeGenClassFieldDecls className mSuperClassName tyFieldDecls = do
  classTypeEnv <- asks getClassTypeEnv
  let (fieldNames, _) = unzip (TypeEnv.getClassFieldsAssoc className classTypeEnv)
  let conExpr = MIL.ReturnE pureSrcMonadMil $
                  MIL.TupleE (map (MIL.VarE . varMil . classFieldVar) fieldNames)
  fieldDeclsExpr <- codeGenFields conExpr tyFieldDecls
  classMap <- getClassMap
  let fieldDeclsExprWithSuper = case mSuperClassName of
                                  Just superClassName ->
                                    let superFieldDeclsExpr = classMap Map.! superClassName
                                    in fieldDeclsExpr `replacesConstructorExprIn` superFieldDeclsExpr
                                  Nothing -> fieldDeclsExpr
  modifyClassMap (Map.insert className fieldDeclsExprWithSuper)
  return fieldDeclsExprWithSuper

-- | Takes an expression that will use a sequence of class field bind
-- expressions at the end.
-- Starts with this expression at the bottom and builds a sequence of binds
-- upwards (hence reverse).
codeGenFields :: MIL.SrcExpr -> [TyFieldDecl] -> CodeGenM MIL.SrcExpr
codeGenFields milBodyExpr tyFieldDecls =
  foldM (\e (FieldDecl _ tyDecl _) -> fst <$> codeGenDecl tyDecl classFieldVar pureSrcMonadMil (e, undefined))
    milBodyExpr (reverse tyFieldDecls)

-- | First expression replaces a return expression (containing class data
-- constructor) at the bottom of a bind sequence in the second expression.
replacesConstructorExprIn :: MIL.SrcExpr -> MIL.SrcExpr -> MIL.SrcExpr
e `replacesConstructorExprIn` (MIL.LetE vb bindExpr bodyExpr) =
  MIL.LetE vb bindExpr (e `replacesConstructorExprIn` bodyExpr)
e `replacesConstructorExprIn` (MIL.ReturnE {}) = e

codeGenClassConstructor :: ClassName -> CodeGenM MIL.SrcFunDef
codeGenClassConstructor className = do
  classFieldsType <- getClassFieldsType className
  classTypeMil <- srcTypeMil (TyClass className)
  return $ MIL.FunDef (classConstructorNameMil className) (MIL.SrcTyApp pureSrcMonadMil classTypeMil) $
    MIL.mkSrcLet (MIL.Var classDataVarName) classFieldsType (MIL.VarE (MIL.funNameToVar $ classDataConstructorNameMil className))
      (MIL.AppE (MIL.VarE (MIL.funNameToVar $ classDefNameMil className)) (MIL.mkSrcVar classDataVarName))

-- | Generates a class definition function.
-- The function takes class data as an argument.
-- The body is a constructed class (tuple) with fields and methods.
-- If a method is defined only in the super class, it is referenced by name.
-- If there is a super class, it is constructed first and then pattern matched
-- to get its fields and methods available.
codeGenClassFunDef :: ClassName -> [TyMethodDecl] -> CodeGenM MIL.SrcFunDef
codeGenClassFunDef className tyMethodDecls = do
  classFieldsType <- getClassFieldsType className
  classTypeMil <- srcTypeMil (TyClass className)
  let classFunDefType = MIL.SrcTyArrow classFieldsType (MIL.SrcTyApp pureSrcMonadMil classTypeMil)

  classMethods <- getClassMethodsAssoc className
  classMethodsExpr <- MIL.TupleE <$> forM classMethods (\(methodName, methodType) -> do
    let mTyMethodDecl = find (\(MethodDecl _ (FunDef _ srcFunName _ _) _) -> getFunName srcFunName == methodName) tyMethodDecls
    case mTyMethodDecl of
      Just tyMethodDecl -> codeGenClassMethod className tyMethodDecl
      Nothing -> return (MIL.VarE $ varMil (funNameToVar $ superMethodName methodName)))

  mSuperClassName <- getSuperClass className
  case mSuperClassName of
    Just superClassName -> do
      superClassTypeMil <- srcTypeMil (TyClass superClassName)
      superClassFieldsType <- getClassFieldsType superClassName
      superClassMethodsType <- getClassMethodsType superClassName
      superClassMethods <- getClassMethodsAssoc superClassName

      superClassMethodsPattern <- MIL.TupleP <$> forM superClassMethods (\(superClassMethodName, superClassMethodType) -> do
        superClassMethodSrcTypeMil <- methodSrcTypeMil $ ftiType superClassMethodType
        return $ MIL.VarBinder (varMil (funNameToVar $ superMethodName superClassMethodName), superClassMethodSrcTypeMil))

      return $ MIL.FunDef (classDefNameMil className) classFunDefType $
        MIL.mkSrcLambda (MIL.Var classDataVarName) classFieldsType $
          MIL.mkSrcLet (MIL.Var superClassVarName) superClassTypeMil (MIL.VarE (MIL.funNameToVar $ classConstructorNameMil superClassName)) $
            MIL.mkSrcLetRec [(MIL.Var selfVarName, classTypeMil,
              MIL.CaseE (MIL.mkSrcVar superClassVarName)
                [MIL.CaseAlt (MIL.TupleP [ MIL.VarBinder (MIL.Var superClassDataVarName, superClassFieldsType)
                                         , MIL.VarBinder (MIL.Var superClassMethodsVarName, superClassMethodsType)],
                   MIL.CaseE (MIL.mkSrcVar superClassMethodsVarName)
                     [MIL.CaseAlt (superClassMethodsPattern,
                        MIL.TupleE [MIL.mkSrcVar classDataVarName, classMethodsExpr])])])]
              (MIL.ReturnE pureSrcMonadMil (MIL.mkSrcVar selfVarName))

    Nothing ->
      return $ MIL.FunDef (classDefNameMil className) classFunDefType $
        MIL.mkSrcLambda (MIL.Var classDataVarName) classFieldsType $
          MIL.mkSrcLetRec [(MIL.Var selfVarName, classTypeMil, MIL.TupleE [MIL.mkSrcVar classDataVarName, classMethodsExpr])] $
            MIL.ReturnE pureSrcMonadMil (MIL.mkSrcVar selfVarName)

-- | Class method body expression generation is almost exactly the same as
-- function code generation, but it gets one extra Unit parameter at the
-- beginning to make it lazy.
codeGenClassMethod :: ClassName -> TyMethodDecl -> CodeGenM MIL.SrcExpr
codeGenClassMethod className (MethodDecl _ (FunDef _ srcFunName tyFunType tyStmts) _) = do
  let funName = getFunName srcFunName
  funType <- asks (ftiType . getClassMethodTypeInfo className funName . getClassTypeEnv)
  retType <- asks (ftiReturnType . getClassMethodTypeInfo className funName . getClassTypeEnv)
  let isPure = isPureType $ unReturn retType
  let funMonad = if isPure
                   then pureSrcMonadMil
                   else impureSrcMonadMil
  resetVariablesMap  -- TODO: test
  (funBody, _) <- codeGenStmts tyStmts funMonad
  let funParams = getFunParams tyFunType
  funBodyWithParams <- foldM (\e tyVarBinder ->
                                MIL.mkSrcLambda (varMil (getVar $ getBinderVar tyVarBinder)) <$>
                                                (srcTypeMil $ getTypeOf tyVarBinder) <*> pure e)
                         funBody (reverse funParams)
  return $ MIL.mkSrcLambda (MIL.Var "lazy_") (MIL.mkSimpleSrcType "Unit") funBodyWithParams

codeGenFunDef :: TyFunDef -> CodeGenM MIL.SrcFunDef
codeGenFunDef (FunDef _ srcFunName tyFunType tyStmts) = do
  let funName = getFunName srcFunName
  funType <- asks (ftiType . getFunTypeInfo funName . getFunTypeEnv)
  retType <- asks (ftiReturnType . getFunTypeInfo funName . getFunTypeEnv)
  let isPure = isPureType $ unReturn retType
  let funMonad = if isPure
                   then pureSrcMonadMil
                   else impureSrcMonadMil
  milFunSrcType <- srcFunTypeMil tyFunType retType
  resetVariablesMap
  (funBody, _) <- codeGenStmts tyStmts funMonad
  let funParams = getFunParams tyFunType
  funBodyWithParams <- foldM (\e tyVarBinder ->
                                MIL.mkSrcLambda (varMil (getVar $ getBinderVar tyVarBinder)) <$>
                                                (srcTypeMil $ getTypeOf tyVarBinder) <*> pure e)
                         funBody (reverse funParams)
  return $ MIL.FunDef (funNameMil funName) milFunSrcType funBodyWithParams

-- | List of statements is not empty.
-- Takes a monad of the containing function.
--
-- Declaration and assignment statements need a special treatment to get
-- variable scope right.
codeGenStmts :: [TyStmt] -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
-- When a statement block is empty, it means it returns unit
codeGenStmts [] funMonad =
  return ( MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit)
         , MIL.SrcTyApp funMonad (MIL.mkSimpleSrcType "Unit"))
codeGenStmts [DeclS _ decl] funMonad =
  codeGenDecl decl id funMonad ( MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit)
                               , MIL.SrcTyApp funMonad (MIL.mkSimpleSrcType "Unit"))
codeGenStmts [stmt@(AssignS {})] funMonad = do
  preCodeGenAssign stmt
  result <- codeGenAssign stmt funMonad ( MIL.ReturnE funMonad (MIL.LitE MIL.UnitLit)
                                        , MIL.SrcTyApp funMonad (MIL.mkSimpleSrcType "Unit"))
  postCodeGenAssign stmt
  return result
codeGenStmts [tyStmt] funMonad = codeGenStmt tyStmt funMonad

codeGenStmts ((DeclS _ decl):tyStmts) funMonad = do
  milBodyExprWithType <- codeGenStmts tyStmts funMonad
  codeGenDecl decl id funMonad milBodyExprWithType
codeGenStmts (stmt@(AssignS {}):tyStmts) funMonad = do
  preCodeGenAssign stmt
  milBodyExprWithType <- codeGenStmts tyStmts funMonad
  result <- codeGenAssign stmt funMonad milBodyExprWithType
  postCodeGenAssign stmt
  return result
codeGenStmts (tyStmt:tyStmts) funMonad = do
  (milBindExpr, milBindExprType) <- codeGenStmt tyStmt funMonad
  var <- newMilVar
  (milBodyExpr, milBodyExprType) <- codeGenStmts tyStmts funMonad
  return ( MIL.mkSrcLet var (MIL.getSrcResultType milBindExprType) milBindExpr milBodyExpr
         , milBodyExprType)

-- | Takes a monad of the containing function.
codeGenStmt :: TyStmt -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenStmt tyStmt funMonad =
  case tyStmt of
    ExprS _ tyExpr -> codeGenExpr tyExpr funMonad

    WhenS _ tyCondExpr tyThenStmts tyOtherwiseStmts -> do
      (milCondExpr, milCondExprType) <- codeGenExpr tyCondExpr funMonad
      milCondVar <- newMilVar
      (thenMilExpr, thenMilType) <- codeGenStmts tyThenStmts funMonad
      (otherwiseMilExpr, _) <- codeGenStmts tyOtherwiseStmts funMonad
      return ( MIL.mkSrcLet milCondVar (MIL.getSrcResultType milCondExprType) milCondExpr $
                 MIL.CaseE (MIL.VarE milCondVar)
                   [ MIL.CaseAlt (MIL.ConP (MIL.ConName "True") [], thenMilExpr)
                   , MIL.CaseAlt (MIL.ConP (MIL.ConName "False") [], otherwiseMilExpr)]
             , thenMilType)

    TryS _ tyTryStmts tyCatchStmts tyFinallyStmts -> do
      (tryMilExpr, tryMilType) <- codeGenStmts tyTryStmts funMonad
      (catchMilExpr, _) <- codeGenStmts tyCatchStmts funMonad
      (finallyMilExpr, finallyMilType) <- codeGenStmts tyFinallyStmts funMonad
      tryCatchMilVar <- newMilVar
      finallyMilVar <- newMilVar
      let catchErrorFunName = if funMonad == pureSrcMonadMil
                                then "catch_error_1"
                                else "catch_error_2"
      return ( MIL.mkSrcLet tryCatchMilVar (MIL.getSrcResultType tryMilType)
                 (MIL.AppE
                    (MIL.AppE (MIL.TypeAppE (MIL.TypeAppE (MIL.VarE $ MIL.Var catchErrorFunName) (MIL.mkSimpleSrcType "Unit")) (MIL.getSrcResultType tryMilType))
                              tryMilExpr)
                    (MIL.mkSrcLambda (MIL.Var "error_") (MIL.mkSimpleSrcType "Unit") catchMilExpr)) $
                 MIL.mkSrcLet finallyMilVar (MIL.getSrcResultType finallyMilType) finallyMilExpr
                   (MIL.ReturnE funMonad (MIL.VarE tryCatchMilVar))
             , tryMilType)

    ThrowS _ t _ -> do
      t' <- srcTypeMil t
      return ( MIL.AppE (MIL.TypeAppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "throw_error") (MIL.mkSimpleSrcType "Unit"))
                                      t')
                        (MIL.LitE MIL.UnitLit)
             , MIL.SrcTyApp funMonad t')

    AssignS {} -> error "codeGenStmt: AssignS should have a special treatment."
    DeclS {} -> error "codeGenStmt: DeclS should have a special treatment."

-- | Code generation for declarations.
-- It takes an expression which will become a body of the monadic bind, where a
-- declared variable will be in scope and a type of this expression.
-- It also takes a function to transform a variable that is being declared.
-- Used to add extra prefixes/suffixes to the name.
codeGenDecl :: TyDeclaration -> (Var -> Var) -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenDecl (Decl _ tyVarBinder mTyInit _) varTransform funMonad (milBodyExpr, milBodyExprType) = do
  let var = getVar $ getBinderVar tyVarBinder
  (milInitExpr, milInitExprType) <-
    case mTyInit of
      Just tyInit -> codeGenExpr (getInitExpr tyInit) funMonad
      -- It may be a variable with Mutable type, so we need 'getUnderType'.
      Nothing -> codeGenExpr (maybeDefaultExpr $ getUnderType $ getTypeOf tyVarBinder) funMonad
  return ( MIL.mkSrcLet (varMil $ varTransform var) (MIL.getSrcResultType milInitExprType) milInitExpr milBodyExpr
         , milBodyExprType)

-- | Fresh name for new assigned variable occurence should be generated
-- separately, because of the order in which code is generated for statements.
preCodeGenAssign :: TyStmt -> CodeGenM ()
preCodeGenAssign (AssignS _ srcAssignOp tyExprLeft _ _) =
  case getAssignOp srcAssignOp of
    AssignMut -> preCodeGenAssignMut tyExprLeft
    _ -> return ()

-- | Because of the order in which code is generated for statements,
-- there should be a post step to be able to fix the environment.
-- See 'postCodeGenAssignMut'.
postCodeGenAssign :: TyStmt -> CodeGenM ()
postCodeGenAssign (AssignS _ srcAssignOp tyExprLeft _ _) =
  case getAssignOp srcAssignOp of
    AssignMut -> postCodeGenAssignMut tyExprLeft
    _ -> return ()

-- | Code generation for assignments.
-- It takes an expression which will become a body of the monadic bind, where a
-- new occurence of the assigned variable will be in scope and a type of this
-- expression.
codeGenAssign :: TyStmt -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenAssign (AssignS _ srcAssignOp tyExprLeft tyExprRight _) funMonad milBodyExprWithType =
  case getAssignOp srcAssignOp of
    AssignMut -> codeGenAssignMut tyExprLeft tyExprRight funMonad milBodyExprWithType
    AssignRef -> codeGenAssignRef tyExprLeft tyExprRight funMonad milBodyExprWithType

preCodeGenAssignMut :: TyExpr -> CodeGenM ()
preCodeGenAssignMut tyExprLeft =
  case tyExprLeft of
    VarE _ _ var _ -> void $ nextVar var

-- | Fresh name for new assigned variable occurence should be fixed
-- back after the assignment statement code generation.
postCodeGenAssignMut :: TyExpr -> CodeGenM ()
postCodeGenAssignMut tyExprLeft =
  case tyExprLeft of
    VarE _ _ var _ -> void $ previousVar var

codeGenAssignMut :: TyExpr -> TyExpr -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenAssignMut tyExprLeft tyExprRight funMonad (milBodyExpr, milBodyExprType) =
  case tyExprLeft of
    VarE _ _ var _ -> do
      milVar <- currentVar var
      (milExprRight, milExprRightType) <- codeGenExpr tyExprRight funMonad
      return ( MIL.mkSrcLet milVar (MIL.getSrcResultType milExprRightType) milExprRight milBodyExpr
             , milBodyExprType)

codeGenAssignRef :: TyExpr -> TyExpr -> MIL.SrcType -> (MIL.SrcExpr, MIL.SrcType) -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenAssignRef tyExprLeft tyExprRight funMonad (milBodyExpr, milBodyExprType) =
  case tyExprLeft of
    VarE _ _ var _ -> do
      (milExprRight, milExprRightType) <- codeGenExpr tyExprRight funMonad
      exprRightVar <- newMilVar
      stmtResultVar <- newMilVar
      return ( MIL.mkSrcLet stmtResultVar (MIL.mkSimpleSrcType "Unit")
                 (MIL.mkSrcLet exprRightVar (MIL.getSrcResultType milExprRightType) milExprRight $
                    (MIL.AppE (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "write_ref") (MIL.getSrcResultType milExprRightType))
                                        (MIL.VarE $ varMil var))
                              (MIL.VarE exprRightVar)))
                 milBodyExpr
             , milBodyExprType)

-- | Expression code generation.
-- Takes a monad of the containing function.
codeGenExpr :: TyExpr -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenExpr tyExpr funMonad =
  case tyExpr of
    LitE lit -> do
      e <- MIL.ReturnE funMonad <$> literalMil lit
      t <- MIL.SrcTyApp funMonad <$> (srcTypeMil $ getTypeOf tyExpr)
      return (e, t)

    VarE _ varType var varPure -> do
      varCase <- getVarCase var varType
      milVar <- currentVar var
      case varCase of
        LocalVarValueType -> do
          t <- MIL.SrcTyApp funMonad <$> srcTypeMil varType
          return (MIL.ReturnE funMonad (MIL.VarE milVar), t)
        LocalVarFunType -> do
          t <- MIL.SrcTyApp funMonad <$> srcTypeMil varType
          return (MIL.ReturnE funMonad (MIL.VarE milVar), t)
        GlobalFunWithParams -> do
          t <- MIL.SrcTyApp funMonad <$> srcTypeMil varType
          return (MIL.ReturnE funMonad (MIL.VarE milVar), t)
        GlobalFunWithoutParams -> do
          t <- funSrcTypeMil varType
          return (MIL.VarE milVar, t)

    MemberAccessE _ t tyObjExpr srcMemberName _ -> do
      (objMilExpr, objMilExprType) <- codeGenExpr tyObjExpr funMonad
      let memberName = getMemberName srcMemberName
      let memberMilVar = varMil $ memberNameToVar memberName

      let TyClass className = getTypeOf tyObjExpr

      fieldsType <- getClassFieldsType className
      methodsType <- getClassMethodsType className

      classMethods <- getClassMethodsAssoc className
      methodsPattern <- MIL.TupleP <$> forM classMethods (\(methodName, methodType) -> do
        methodSrcTypeMil <- methodSrcTypeMil $ ftiType methodType
        return $ MIL.VarBinder (varMil (funNameToVar methodName), methodSrcTypeMil))

      classFields <- getClassFieldsAssoc className
      fieldsPattern <- MIL.TupleP <$> forM classFields (\(fieldName, fieldType) -> do
        fieldSrcTypeMil <- srcTypeMil fieldType
        return $ MIL.VarBinder (varMil fieldName, fieldSrcTypeMil))

      objExprMilVar <- newMilVar
      fieldsMilVar <- newMilVar
      methodsMilVar <- newMilVar
      (memberExpr, memberAccessTypeMil) <-
        ifM (isClassMethodDefined className (memberNameToFunName memberName))
          (do methodArity <- getClassMethodArity className (memberNameToFunName memberName)
              if methodArity == 0
                then do memberTempMilVar <- newMilVar
                        methodType <- getClassMethodType className (memberNameToFunName memberName)
                        methodTypeMil <- srcTypeMil methodType
                        -- Parameterless method types already have monad at the
                        -- top. At this point we also want their effect to
                        -- happen, so bind is needed.
                        -- Lazy unit argument is supplied as well.
                        t' <- srcTypeMil t
                        return ( MIL.mkSrcLet memberTempMilVar (MIL.getSrcResultType methodTypeMil)
                                    (MIL.AppE (MIL.VarE memberMilVar) (MIL.LitE MIL.UnitLit)) $
                                    (MIL.ReturnE funMonad $ MIL.VarE memberTempMilVar)
                               , t')
                else do t' <- MIL.SrcTyApp funMonad <$> srcTypeMil t
                        return ( MIL.ReturnE funMonad (MIL.AppE (MIL.VarE memberMilVar) (MIL.LitE MIL.UnitLit))
                               , t'))
          (do t' <- MIL.SrcTyApp funMonad <$> srcTypeMil t
              return (MIL.ReturnE funMonad (MIL.VarE memberMilVar), t'))
      return $ ( MIL.mkSrcLet objExprMilVar (MIL.getSrcResultType objMilExprType) objMilExpr $
                   MIL.CaseE (MIL.VarE objExprMilVar)
                     [MIL.CaseAlt (MIL.TupleP [ MIL.VarBinder (fieldsMilVar, fieldsType)
                                              , MIL.VarBinder (methodsMilVar, methodsType)],
                        MIL.CaseE (MIL.VarE fieldsMilVar)
                          [MIL.CaseAlt (fieldsPattern,
                             MIL.CaseE (MIL.VarE methodsMilVar)
                               [MIL.CaseAlt (methodsPattern, memberExpr)])])]
               , memberAccessTypeMil)

    -- Only 'new'
    ClassAccessE _ t srcClassName _ -> do
      let className = getClassName srcClassName
      t' <- MIL.SrcTyApp pureSrcMonadMil <$> srcTypeMil t
      return (MIL.VarE (MIL.funNameToVar $ classConstructorNameMil className), t')

    NewRefE _ _ tyRefUnderExpr -> do
      (milRefUnderExpr, milRefUnderExprType) <- codeGenExpr tyRefUnderExpr funMonad
      refUnderExprVar <- newMilVar
      let milRefUnderType = MIL.getSrcResultType milRefUnderExprType
      let milRefType = MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") milRefUnderType
      return ( MIL.mkSrcLet refUnderExprVar milRefUnderType milRefUnderExpr $
                 (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "new_ref") milRefUnderType)
                           (MIL.VarE refUnderExprVar))
             , MIL.SrcTyApp funMonad milRefType)

    DerefE _ refUnderType tyRefExpr -> do
      (milRefExpr, _) <- codeGenExpr tyRefExpr funMonad
      refExprVar <- newMilVar
      milRefUnderType <- srcTypeMil refUnderType
      let milRefType = MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") milRefUnderType
      return ( MIL.mkSrcLet refExprVar milRefType milRefExpr $
                 (MIL.AppE (MIL.TypeAppE (MIL.VarE $ MIL.Var "read_ref") milRefUnderType)
                           (MIL.VarE refExprVar))
             , MIL.SrcTyApp funMonad milRefUnderType)

    BinOpE _ resultType srcBinOp tyExpr1 tyExpr2 _ ->
      codeGenBinOp (getBinOp srcBinOp) tyExpr1 tyExpr2 resultType funMonad

    -- TODO: Should the type from JustE annotation be used more?
    JustE _ _ tySubExpr -> do
      (milSubExpr, milSubExprMonadType) <- codeGenExpr tySubExpr funMonad
      let milSubExprType = MIL.getSrcResultType milSubExprMonadType
      var <- newMilVar
      return ( MIL.mkSrcLet var milSubExprType milSubExpr $
                 MIL.ReturnE funMonad (MIL.AppE (MIL.TypeAppE (MIL.mkSrcConName "Just") milSubExprType)
                                                (MIL.VarE var))
             , MIL.SrcTyApp funMonad $ MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") milSubExprType)

    ParenE _ tySubExpr -> codeGenExpr tySubExpr funMonad

literalMil :: TyLiteral -> CodeGenM MIL.SrcExpr
literalMil UnitLit {} = return $ MIL.LitE MIL.UnitLit
literalMil (BoolLit _ _ b) =
  return $ if b
    then MIL.mkSrcConName "True"
    else MIL.mkSrcConName "False"
literalMil (IntLit _ _ i)     = return $ MIL.LitE (MIL.IntLit i)
literalMil (FloatLit _ _ f _) = return $ MIL.LitE (MIL.FloatLit f)
literalMil (StringLit _ _ s)  = return $ stringMil s
literalMil (NothingLit _ t _) =
  -- Monomorphise Nothing constructor.
  MIL.TypeAppE
    (MIL.mkSrcConName "Nothing")
    <$> (srcTypeMil $ getMaybeUnderType t)

-- var can be:
-- + local variable of value type
-- + local variable of function type
-- + global pure function (without parameters)
-- + global impure function (without parameters)
-- + global pure/impure function with parameters
-- context can be:
-- + pure
-- + impure
data VarCase
  = LocalVarValueType
  | LocalVarFunType
  | GlobalFunWithParams
  | GlobalFunWithoutParams

getVarCase :: Var -> Type -> CodeGenM VarCase
getVarCase var varType = do
  let funName = varToFunName var
  isGlobalFun <- asks (isFunctionDefined funName . getFunTypeEnv)
  if isGlobalFun
    then do
      arity <- asks (ftiArity . getFunTypeInfo funName . getFunTypeEnv)
      if arity == 0
        then return GlobalFunWithoutParams
        else return GlobalFunWithParams
    else
      if isValueType varType
        then return LocalVarValueType
        else return LocalVarFunType

-- | Takes a monad of the containing function.
codeGenBinOp :: BinOp -> TyExpr -> TyExpr -> Type -> MIL.SrcType -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenBinOp binOp tyExpr1 tyExpr2 resultType funMonad =
  case binOp of
    App -> do
      (milExpr1, milExpr1Type) <- codeGenExpr tyExpr1 funMonad
      var1 <- newMilVar
      (milExpr2, milExpr2Type) <- codeGenExpr tyExpr2 funMonad
      var2 <- newMilVar
      (appE, milResultType) <- if isValueType resultType
                                 then do
                                   t <- funSrcTypeMil resultType
                                   return (MIL.AppE (MIL.VarE var1) (MIL.VarE var2), t)
                                 else do
                                   -- TODO: It seems like it doesn't matter if
                                   -- 'srcTypeMil' or 'funSrcTypeMil' is used,
                                   -- because they do the same for the
                                   -- function (arrow) type. Is there a
                                   -- counter example?
                                   t <- MIL.SrcTyApp funMonad <$> srcTypeMil resultType
                                   return (MIL.ReturnE funMonad (MIL.AppE (MIL.VarE var1) (MIL.VarE var2)), t)
      return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
                 MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2
                   appE
             , milResultType)

    Add ->
      let arithFunName = case getTypeOf tyExpr1 of
                           TyFloat -> "add_float"
                           TyInt -> "add_int"
                           _ -> error "codeGenBinOp: unsupported type for Add"
      in codeGenArith tyExpr1 tyExpr2 resultType funMonad arithFunName

    Sub ->
      let arithFunName = case getTypeOf tyExpr1 of
                           TyFloat -> "sub_float"
                           TyInt -> "sub_int"
                           _ -> error "codeGenBinOp: unsupported type for Sub"
      in codeGenArith tyExpr1 tyExpr2 resultType funMonad arithFunName

    Mul ->
      let arithFunName = case getTypeOf tyExpr1 of
                           TyFloat -> "mul_float"
                           TyInt -> "mul_int"
                           _ -> error "codeGenBinOp: unsupported type for Mul"
      in codeGenArith tyExpr1 tyExpr2 resultType funMonad arithFunName

    Div -> do
      let arithFunName = case getTypeOf tyExpr1 of
                           TyFloat -> "div_float"
                           TyInt -> "div_int"
                           _ -> error "codeGenBinOp: unsupported type for Div"
      (milExpr1, milExpr1Type) <- codeGenExpr tyExpr1 funMonad
      var1 <- newMilVar
      (milExpr2, milExpr2Type) <- codeGenExpr tyExpr2 funMonad
      var2 <- newMilVar
      milResultType <- funSrcTypeMil resultType
      let arithBaseExpr = MIL.AppE (MIL.AppE (MIL.VarE $ MIL.Var arithFunName) (MIL.VarE var1)) (MIL.VarE var2)
      let arithExpr = if funMonad == pureSrcMonadMil
                        then arithBaseExpr
                        else MIL.LiftE arithBaseExpr pureSrcMonadMil impureSrcMonadMilWithErrorBase
      return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
                 MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2
                   arithExpr
             , milResultType)

    NothingCoalesce -> do
      (milMaybeExpr, milMaybeExprType) <- codeGenExpr tyExpr1 funMonad
      maybeMilVar <- newMilVar
      (milDefaultExpr, milDefaultExprType) <- codeGenExpr tyExpr2 funMonad
      milUnderMaybeVar <- newMilVar
      milMaybeUnderType <- srcTypeMil (getMaybeUnderType $ getTypeOf tyExpr1)
      return ( MIL.mkSrcLet maybeMilVar (MIL.getSrcResultType milMaybeExprType) milMaybeExpr $
                 MIL.CaseE (MIL.VarE maybeMilVar)
                   [ MIL.CaseAlt ( MIL.ConP (MIL.ConName "Just") [MIL.VarBinder (milUnderMaybeVar, milMaybeUnderType)]
                                 , MIL.ReturnE funMonad (MIL.VarE milUnderMaybeVar))
                   , MIL.CaseAlt (MIL.ConP (MIL.ConName "Nothing") [], milDefaultExpr)]
             , milDefaultExprType)

codeGenArith :: TyExpr -> TyExpr -> Type -> MIL.SrcType -> String -> CodeGenM (MIL.SrcExpr, MIL.SrcType)
codeGenArith  tyExpr1 tyExpr2 resultType funMonad arithFunName = do
  (milExpr1, milExpr1Type) <- codeGenExpr tyExpr1 funMonad
  var1 <- newMilVar
  (milExpr2, milExpr2Type) <- codeGenExpr tyExpr2 funMonad
  var2 <- newMilVar
  milResultType <- srcTypeMil resultType
  return ( MIL.mkSrcLet var1 (MIL.getSrcResultType milExpr1Type) milExpr1 $
             MIL.mkSrcLet var2 (MIL.getSrcResultType milExpr2Type) milExpr2 $
               MIL.ReturnE funMonad (MIL.AppE (MIL.AppE (MIL.VarE $ MIL.Var arithFunName) (MIL.VarE var1)) (MIL.VarE var2))
         , MIL.SrcTyApp funMonad milResultType)

{-
  let (appBodyExpr, appBodyType) = case (isValueType resultType, isPureFunType resultType, funMonad == pureMonadMil) of
                                     (True, False, True) -> MIL.ReturnE funMonad appE  -- TODO: should this be possible?

-- | Note [Variable type in bind]:
--
-- In bind, the variable type should be "atomic" (non-monadic), so we must get
-- rid of 'TyPure'.
-}

-- * Type conversions
-- TODO: revise
-- | Note [Type transformation]:
--
-- There are two main versions of type transformation: 'srcTypeMil' and
-- 'funSrcTypeMil'. Usually, on the top-level, 'srcTypeMil' should be used,
-- except for the cases, where it is known that the type being transformed is a
-- type of a function (?). Another case, where 'funSrcTypeMil' is used is when
-- it is known that the type being transformed is the type of the global impure
-- function (with arity 0).
--
-- 'srcTypeMil' introduces impure monad stack only for function return type.
-- 'funSrcTypeMil' introduces impure monad stack also for atomic types, which
-- are not Pure.
--
-- It is important, that 'funSrcTypeMil' recursive call is used only on the
-- right-hand side of the function arrow, since this is the only place, where
-- impure monad stack should be introduced, when 'funSrcTypeMil' is not a
-- top-level call (in this case, atomic types will get impure monad stack as
-- well).
--
-- 'srcFunTypeMil' is introduced because we need to know where is the return
-- type to transform the type correctly (for which 'funSrcTypeMilRetType' is
-- used).
--
-- TODO: SrcTyArrow with Pure monad on every step? Not needed at the moment,
-- but if there are lambdas, then seems to be necessary.

-- | See Note [Type transformation].
srcTypeMil :: Type -> CodeGenM MIL.SrcType
srcTypeMil TyUnit   = return $ MIL.mkSimpleSrcType "Unit"
srcTypeMil TyBool   = return $ MIL.mkSimpleSrcType "Bool"
srcTypeMil TyInt    = return $ MIL.mkSimpleSrcType "Int"
srcTypeMil TyFloat  = return $ MIL.mkSimpleSrcType "Float"
srcTypeMil TyString = return $ MIL.mkSimpleSrcType "String"
srcTypeMil (TyClass className) = do
  classFieldsType <- getClassFieldsType className
  classMethodsType <- getClassMethodsType className
  return $ MIL.SrcTyTuple [classFieldsType, classMethodsType]
srcTypeMil (TyArrow t1 t2) = MIL.SrcTyArrow <$> srcTypeMil t1 <*> funSrcTypeMil t2
srcTypeMil (TyPure t)      = MIL.SrcTyApp pureSrcMonadMil <$> srcTypeMil t
srcTypeMil (TyMaybe t)     = MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") <$> srcTypeMil t
srcTypeMil (TyMutable t)   = srcTypeMil t
srcTypeMil (TyRef t)       = MIL.SrcTyApp (MIL.mkSimpleSrcType "Ref") <$> srcTypeMil t

getClassFieldsType :: ClassName -> CodeGenM MIL.SrcType
getClassFieldsType className = do
  classTypeEnv <- asks getClassTypeEnv
  let (_, fieldTypes) = unzip (TypeEnv.getClassFieldsAssoc className classTypeEnv)
  MIL.SrcTyTuple <$> mapM srcTypeMil fieldTypes

getClassMethodsType :: ClassName -> CodeGenM MIL.SrcType
getClassMethodsType className = do
  classTypeEnv <- asks getClassTypeEnv
  let (_, methodTypes) = unzip (TypeEnv.getClassMethodsAssoc className classTypeEnv)
  MIL.SrcTyTuple <$> mapM (methodSrcTypeMil . ftiType) methodTypes

-- | See Note [Type transformation].
funSrcTypeMil :: Type -> CodeGenM MIL.SrcType
funSrcTypeMil (TyArrow t1 t2) = MIL.SrcTyArrow <$> srcTypeMil t1 <*> funSrcTypeMil t2
funSrcTypeMil (TyPure t)      = MIL.SrcTyApp pureSrcMonadMil <$> srcTypeMil t
funSrcTypeMil (TyMaybe t)     = MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") <$> srcTypeMil t
funSrcTypeMil t = MIL.SrcTyApp impureSrcMonadMil <$> srcTypeMil t

-- | See Note [Type transformation].
srcFunTypeMil :: TyFunType -> ReturnType -> CodeGenM MIL.SrcType
srcFunTypeMil (FunType _ tyVarBinders _) retType = do
  retTypeMil <- funSrcTypeMilRetType retType
  foldM (\acc (VarBinder _ t _ _) -> MIL.SrcTyArrow <$> srcTypeMil t <*> pure acc) retTypeMil (reverse tyVarBinders)

-- | See Note [Type transformation].
funSrcTypeMilRetType :: ReturnType -> CodeGenM MIL.SrcType
funSrcTypeMilRetType (ReturnType (TyPure t))  = MIL.SrcTyApp pureSrcMonadMil <$> srcTypeMil t
funSrcTypeMilRetType (ReturnType (TyMaybe t)) = MIL.SrcTyApp (MIL.mkSimpleSrcType "Maybe") <$> srcTypeMil t
funSrcTypeMilRetType (ReturnType t) = MIL.SrcTyApp impureSrcMonadMil <$> srcTypeMil t

methodSrcTypeMil :: Type -> CodeGenM MIL.SrcType
methodSrcTypeMil t = MIL.SrcTyArrow (MIL.mkSimpleSrcType "Unit") <$> srcTypeMil t

-- * Conversion utils, name helpers

typeNameMil :: ClassName -> MIL.TypeName
typeNameMil (ClassName classNameStr) = MIL.TypeName classNameStr

conNameMil :: ClassName -> MIL.ConName
conNameMil (ClassName classNameStr) = MIL.ConName classNameStr

funNameMil :: FunName -> MIL.FunName
funNameMil (FunName funNameStr) = MIL.FunName funNameStr

classDataConstructorNameMil :: ClassName -> MIL.FunName
classDataConstructorNameMil (ClassName classNameStr) = MIL.FunName ("new_" ++ (classNameStr ++ classDataSuffix))

classConstructorNameMil :: ClassName -> MIL.FunName
classConstructorNameMil (ClassName classNameStr) = MIL.FunName ("new_" ++ classNameStr)

classDefNameMil :: ClassName -> MIL.FunName
classDefNameMil (ClassName classNameStr) = MIL.FunName ("class_" ++ classNameStr)

varMil :: Var -> MIL.Var
varMil (Var varStr) = MIL.Var varStr

classFieldVar :: Var -> Var
classFieldVar (Var varStr) = Var ("self_" ++ varStr)

superMethodName :: FunName -> FunName
superMethodName (FunName funNameStr) = FunName ("super_" ++ funNameStr)

classDataSuffix :: String
classDataSuffix = "_Data"

selfVarName :: String
selfVarName = "self"

classDataVarName :: String
classDataVarName = "self_data"

superClassVarName :: String
superClassVarName = "super"

superClassDataVarName :: String
superClassDataVarName = "super_data"

superClassMethodsVarName :: String
superClassMethodsVarName = "super_methods"

-- * CodeGenM operations

newMilVar :: CodeGenM MIL.Var
newMilVar = do
  i <- getNameSupply
  modifyNameSupply (+1)
  return $ MIL.Var ("var_" ++ show i)

-- | Since VarMap is used for local variables, it needs to be reset between
-- functions.
resetVariablesMap :: CodeGenM ()
resetVariablesMap = modifyVarMap (const Map.empty)

nextVar :: Var -> CodeGenM MIL.Var
nextVar var@(Var varStr) = do
  varMap <- getVarMap
  let i' = case Map.lookup var varMap of
             Just i -> i + 1
             Nothing -> 1
  modifyVarMap (Map.insert var i')
  return $ MIL.Var (varStr ++ "_" ++ show i')

previousVar :: Var -> CodeGenM MIL.Var
previousVar var@(Var varStr) = do
  varMap <- getVarMap
  let i' = case Map.lookup var varMap of
             Just i -> i - 1
             Nothing -> error "Cannot get previousVar"
  modifyVarMap (Map.insert var i')
  return $ MIL.Var (varStr ++ "_" ++ show i')

currentVar :: Var -> CodeGenM MIL.Var
currentVar var@(Var varStr) = do
  varMap <- getVarMap
  return $ case Map.lookup var varMap of
             Just i -> MIL.Var (varStr ++ "_" ++ show i)
             Nothing -> varMil var

-- * CodeGenM helpers

getNameSupply :: CodeGenM NameSupply
getNameSupply = (\(ns, _, _) -> ns) <$> get

getVarMap :: CodeGenM VarMap
getVarMap = (\(_, varMap, _) -> varMap) <$> get

getClassMap :: CodeGenM ClassMap
getClassMap = (\(_, _, classMap) -> classMap) <$> get

getSuperClass :: ClassName -> CodeGenM (Maybe ClassName)
getSuperClass className =
  TypeEnv.getSuperClass className <$> asks getClassTypeEnv

getClassFieldsAssoc :: ClassName -> CodeGenM [(Var, Type)]
getClassFieldsAssoc className =
  TypeEnv.getClassFieldsAssoc className <$> asks getClassTypeEnv

getClassMethodsAssoc :: ClassName -> CodeGenM [(FunName, FunTypeInfo)]
getClassMethodsAssoc className =
  TypeEnv.getClassMethodsAssoc className <$> asks getClassTypeEnv

isClassMethodDefined :: ClassName -> FunName -> CodeGenM Bool
isClassMethodDefined className methodName =
  TypeEnv.isClassMethodDefined className methodName <$> asks getClassTypeEnv

getClassMethodArity :: ClassName -> FunName -> CodeGenM Int
getClassMethodArity className methodName =
  asks (ftiArity . getClassMethodTypeInfo className methodName . getClassTypeEnv)

getClassMethodType :: ClassName -> FunName -> CodeGenM Type
getClassMethodType className methodName =
  asks (ftiType . getClassMethodTypeInfo className methodName . getClassTypeEnv)

modifyNameSupply :: (NameSupply -> NameSupply) -> CodeGenM ()
modifyNameSupply f = modify (\(ns, varMap, classMap) -> (f ns, varMap, classMap))

modifyVarMap :: (VarMap -> VarMap) -> CodeGenM ()
modifyVarMap f = modify (\(ns, varMap, classMap) -> (ns, f varMap, classMap))

modifyClassMap :: (ClassMap -> ClassMap) -> CodeGenM ()
modifyClassMap f = modify (\(ns, varMap, classMap) -> (ns, varMap, f classMap))

-- * Built-ins

pureSrcMonadMil :: MIL.SrcType
pureSrcMonadMil =
  (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType)

impureSrcMonadMil :: MIL.SrcType
impureSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State") $
    MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
        (MIL.mkSimpleSrcType "IO")

impureSrcMonadMilWithErrorBase :: MIL.SrcType
impureSrcMonadMilWithErrorBase =
  MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State") $
    (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType)

monadErrorTypeCons :: [MIL.Type -> MIL.MonadType]
monadErrorTypeCons =
  [
    \et ->
    MIL.MTyMonad (MIL.SinMonadApp (MIL.SinMonad MIL.Error) et)
  , \et ->
    MIL.MTyMonadCons (MIL.SinMonad MIL.State) $
      MIL.MTyMonadCons (MIL.SinMonadApp (MIL.SinMonad MIL.Error) et) $
        MIL.MTyMonad (MIL.SinMonad MIL.IO)
  ]

exceptionSrcType :: MIL.SrcType
exceptionSrcType = MIL.mkSimpleSrcType "Unit"

builtInMilTypeDefs :: [MIL.SrcTypeDef]
builtInMilTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkSimpleSrcType "A"]]
  , MIL.TypeDef (MIL.TypeName "String") []
      [ MIL.ConDef (MIL.ConName "Empty_Str") []
      , MIL.ConDef (MIL.ConName "Cons_Str") [MIL.mkSimpleSrcType "Char", MIL.mkSimpleSrcType "String"]]
  ]

builtInMilFunTypes :: [(MIL.FunName, MIL.SrcType)]
builtInMilFunTypes =
  [ (MIL.FunName "printString", MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readString",  MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "String"))
  , (MIL.FunName "printBool",   MIL.SrcTyArrow (MIL.mkSimpleSrcType "Bool")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readBool",    MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Bool"))
  , (MIL.FunName "printInt",    MIL.SrcTyArrow (MIL.mkSimpleSrcType "Int")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readInt",     MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Int"))
  , (MIL.FunName "printFloat",  MIL.SrcTyArrow (MIL.mkSimpleSrcType "Float")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readFloat",   MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Float"))
  ]

-- | Unsafe. Make sure that there exists such a built-in function.
getMilBuiltInFunType :: MIL.FunName -> MIL.SrcType
getMilBuiltInFunType milFunName = fromJust $ lookup milFunName builtInMilFunTypes

builtInMilFunDefs :: [MIL.SrcFunDef]
builtInMilFunDefs =
  [ printStringMilDef
  ]
  ++ readStringMilDef
  ++
  [ printBoolMilDef
  , readBoolMilDef
  , printIntMilDef
  , readIntMilDef
  , printFloatMilDef
  , readFloatMilDef
  ]

printStringMilDef :: MIL.SrcFunDef
printStringMilDef =
  MIL.mkSrcFunDef "printString" (getMilBuiltInFunType $ MIL.FunName "printString") $
    MIL.mkSrcLambda (MIL.Var "s_") (MIL.mkSimpleSrcType "String") $
      MIL.CaseE (MIL.mkSrcVar "s_")
        [ MIL.CaseAlt (MIL.ConP (MIL.ConName "Empty_Str") [],
            MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))
        , MIL.CaseAlt (MIL.ConP (MIL.ConName "Cons_Str")
              [ MIL.VarBinder (MIL.Var "c_", MIL.mkSimpleSrcType "Char")
              , MIL.VarBinder (MIL.Var "cs_", MIL.mkSimpleSrcType "String")],
            MIL.mkSrcLet (MIL.Var "unit_0") (MIL.mkSimpleSrcType "Unit")
              (MIL.LiftE (MIL.AppE (MIL.mkSrcVar "print_char") (MIL.mkSrcVar "c_"))
                 (MIL.mkSimpleSrcType "IO")
                 impureSrcMonadMil)
              (MIL.AppE (MIL.mkSrcVar "printString") (MIL.mkSrcVar "cs_")))]

readStringMilDef :: [MIL.SrcFunDef]
readStringMilDef =
  [ MIL.mkSrcFunDef "readString" (getMilBuiltInFunType $ MIL.FunName "readString") $
      MIL.AppE (MIL.mkSrcVar "readString_") (MIL.mkSrcConName "Empty_Str")
  , MIL.mkSrcFunDef "readString_" (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                                  (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "String"))) $
      MIL.mkSrcLambda (MIL.Var "acc_") (MIL.mkSimpleSrcType "String") $
        MIL.mkSrcLet (MIL.Var "c_") (MIL.mkSimpleSrcType "Char")
          (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") impureSrcMonadMil)
          (MIL.CaseE (MIL.mkSrcVar "c_")
             [ MIL.CaseAlt (MIL.LitP $ MIL.CharLit ' ',
                 MIL.ReturnE impureSrcMonadMil (MIL.AppE (MIL.AppE (MIL.mkSrcVar "reverseString_") (MIL.mkSrcVar "acc_")) (MIL.mkSrcConName "Empty_Str")))
             , MIL.CaseAlt (MIL.DefaultP,
                 MIL.AppE (MIL.mkSrcVar "readString_") (MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str") (MIL.mkSrcVar "c_")) (MIL.mkSrcVar "acc_")))])
  , MIL.mkSrcFunDef "reverseString_" (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                                     (MIL.SrcTyArrow (MIL.mkSimpleSrcType "String") (MIL.mkSimpleSrcType "String"))) $
      MIL.mkSrcLambda (MIL.Var "s_") (MIL.mkSimpleSrcType "String") $
        MIL.mkSrcLambda (MIL.Var "acc_") (MIL.mkSimpleSrcType "String") $
          MIL.CaseE (MIL.mkSrcVar "s_")
            [ MIL.CaseAlt (MIL.ConP (MIL.ConName "Empty_Str") [],
                MIL.mkSrcVar "acc_")
            , MIL.CaseAlt (MIL.ConP (MIL.ConName "Cons_Str")
                  [ MIL.VarBinder (MIL.Var "c_", MIL.mkSimpleSrcType "Char")
                  , MIL.VarBinder (MIL.Var "cs_", MIL.mkSimpleSrcType "String")],
                MIL.AppE (MIL.AppE (MIL.mkSrcVar "reverseString_") (MIL.mkSrcVar "cs_"))
                         (MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str") (MIL.mkSrcVar "c_")) (MIL.mkSrcVar "acc_")))
            ]
  ]

printBoolMilDef :: MIL.SrcFunDef
printBoolMilDef =
  MIL.mkSrcFunDef "printBool" (getMilBuiltInFunType $ MIL.FunName "printBool") $
    MIL.mkSrcLambda (MIL.Var "b_") (MIL.mkSimpleSrcType "Bool") $
      MIL.CaseE (MIL.mkSrcVar "b_")
        [ MIL.CaseAlt (MIL.ConP (MIL.ConName "True") [],
            MIL.AppE (MIL.mkSrcVar "printString") (stringMil "true"))
        , MIL.CaseAlt (MIL.ConP (MIL.ConName "False") [],
            MIL.AppE (MIL.mkSrcVar "printString") (stringMil "false"))
        ]

readBoolMilDef :: MIL.SrcFunDef
readBoolMilDef =
  MIL.mkSrcFunDef "readBool" (getMilBuiltInFunType $ MIL.FunName "readBool") $
    MIL.mkSrcLet (MIL.Var "c_1") (MIL.mkSimpleSrcType "Char")
      (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") impureSrcMonadMil)
      (MIL.CaseE (MIL.mkSrcVar "c_1")
         [ readBoolCaseAlt "true" "True" 2
         , readBoolCaseAlt "false" "False" 2
         , readBoolErrorCaseAlt
         ])

readBoolCaseAlt :: String -> String -> Int -> MIL.SrcCaseAlt
readBoolCaseAlt "" conNameStr _ =
  MIL.CaseAlt (MIL.LitP $ MIL.CharLit ' ',
    MIL.ReturnE impureSrcMonadMil (MIL.mkSrcConName conNameStr))
readBoolCaseAlt (c:cs) conNameStr i =
  MIL.CaseAlt (MIL.LitP $ MIL.CharLit c,
    MIL.mkSrcLet (MIL.Var $ "c_" ++ show i) (MIL.mkSimpleSrcType "Char")
      (MIL.LiftE (MIL.mkSrcVar "read_char") (MIL.mkSimpleSrcType "IO") impureSrcMonadMil)
      (MIL.CaseE (MIL.mkSrcVar $ "c_" ++ show i)
         [ readBoolCaseAlt cs conNameStr (i+1)
         , readBoolErrorCaseAlt
         ]))

readBoolErrorCaseAlt :: MIL.SrcCaseAlt
readBoolErrorCaseAlt =
  MIL.CaseAlt (MIL.DefaultP,
    MIL.LiftE (MIL.AppE (MIL.TypeAppE (MIL.TypeAppE (MIL.mkSrcVar "throw_error") (MIL.mkSimpleSrcType "Unit")) (MIL.mkSimpleSrcType "Bool"))
                        (MIL.LitE MIL.UnitLit))
              pureSrcMonadMil impureSrcMonadMilWithErrorBase)

printIntMilDef :: MIL.SrcFunDef
printIntMilDef =
  MIL.mkSrcFunDef "printInt" (getMilBuiltInFunType $ MIL.FunName "printInt")
    (MIL.mkSrcLambda (MIL.Var "i_") (MIL.mkSimpleSrcType "Int") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readIntMilDef :: MIL.SrcFunDef
readIntMilDef =
  MIL.mkSrcFunDef "readInt" (getMilBuiltInFunType $ MIL.FunName "readInt")
    (MIL.ReturnE impureSrcMonadMil (MIL.LitE $ MIL.IntLit 1))

printFloatMilDef :: MIL.SrcFunDef
printFloatMilDef =
  MIL.mkSrcFunDef "printFloat" (getMilBuiltInFunType $ MIL.FunName "printFloat")
    (MIL.mkSrcLambda (MIL.Var "f_") (MIL.mkSimpleSrcType "Float") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readFloatMilDef :: MIL.SrcFunDef
readFloatMilDef =
  MIL.mkSrcFunDef "readFloat" (getMilBuiltInFunType $ MIL.FunName "readFloat")
    (MIL.ReturnE impureSrcMonadMil (MIL.LitE $ MIL.FloatLit 1.0))

stringMil :: String -> MIL.SrcExpr
stringMil "" = MIL.mkSrcConName "Empty_Str"
stringMil (c:cs) =
  MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str")
                     (MIL.mkCharLit c))
           (stringMil cs)

