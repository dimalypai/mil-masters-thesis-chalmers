-- | Type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
--
-- Type checking produces a type environment with information about global
-- definitions and annotates local variable occurences with their types. So,
-- when we say type checked and it is applicable to annotate something it also
-- means `annotated`.
module OOLang.TypeChecker
  ( typeCheck
  , typeCheckStage
  , typeOf
  , TypeEnv
  , initTypeEnv
  , TcError
  , prPrint
  ) where

import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)
import Data.List (find)
import Control.Applicative ((<$>), (<*>))

import OOLang.AST
import OOLang.TypeChecker.TypeCheckM
import OOLang.TypeChecker.TcError
import OOLang.BuiltIn
import OOLang.Utils

-- | Main batch entry point to the TypeChecker.
-- In the case of success returns a typed program and a type environment.
typeCheck :: SrcProgram -> Either TcError (TyProgram, TypeEnv)
typeCheck srcProgram = runTypeCheckM (tcProgram srcProgram) initTypeEnv

-- | Main staging entry point to the TypeChecker.
-- Takes a type environment to begin with. May be used for adding chunks of the
-- program and type checking them together with previously type checked chunks.
-- In the case of success returns a typed program and a type environment.
typeCheckStage :: SrcProgram -> TypeEnv -> Either TcError (TyProgram, TypeEnv)
typeCheckStage srcProgram typeEnv = runTypeCheckM (tcProgram srcProgram) typeEnv

-- | Type checks a given source expression in a given type environment.
-- In the case of success - returns its type.
typeOf :: SrcExpr -> TypeEnv -> Either TcError Type
typeOf srcExpr typeEnv = getExprType <$> runTypeCheckM (tcExpr srcExpr) typeEnv
  where getExprType = snd . fst

-- | Entry point into the type checking of the program.
-- Returns a type checked program.
tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram (Program s classDefs funDefs) = do
  collectDefs classDefs funDefs
  -- Class names and function types (but not parameter names) have been checked.
  -- Now first information about definitions is in the environment:
  -- + class names and their super classes (not checked)
  -- + function names and their types
  checkMain
  checkInheritance
  tyClassDefs <- mapM tcClassDef classDefs
  tyFunDefs <- mapM tcFunDef funDefs
  return $ Program s tyClassDefs tyFunDefs

-- | In order to be able to handle (mutually) recursive definitions, we need to
-- do an additional first pass to collect function signatures and class names
-- (together with super classes).
--
-- It collects:
--
-- * class names and possibly their super classes
--
-- * function names and their types
--
-- It also does checking of function types. But it does not check super
-- classes and function parameter names.
collectDefs :: [SrcClassDef] -> [SrcFunDef] -> TypeCheckM ()
collectDefs classDefs funDefs = do
  -- It is essential that we collect classes before functions, because function
  -- types may mention defined classes.
  mapM_ collectClassDef classDefs
  mapM_ collectFunDef funDefs

-- | Checks if the class is already defined.
-- Adds the class (and its super class if it has one) to the environment.
-- Super class is not checked (whether it is defined).
collectClassDef :: SrcClassDef -> TypeCheckM ()
collectClassDef (ClassDef _ srcClassName mSuperSrcClassName _) = do
  whenM (isClassDefined $ getClassName srcClassName) $
    throwError $ ClassAlreadyDefined srcClassName
  addClass srcClassName mSuperSrcClassName

-- | Checks if the function is already defined.
-- Checks that the specified function type is correct (used types are defined).
-- Does not check parameter names.
-- Performs a function type transformation.
-- Adds the function and its type to the environment.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef _ srcFunName srcFunType _) = do
  whenM (isFunctionDefined $ getFunName srcFunName) $
    throwError $ FunctionAlreadyDefined srcFunName
  funType <- srcFunTypeToType srcFunType
  addFunction srcFunName funType srcFunType

-- | Program needs to have an entry point: `main : Unit`.
checkMain :: TypeCheckM ()
checkMain = do
  unlessM (isFunctionDefined $ FunName "main") $
    throwError MainNotDefined
  funTypeInfo <- getFunTypeInfo $ FunName "main"
  when (ftiType funTypeInfo /= TyUnit) $
    throwError $ MainIncorrectType (ftiSrcFunType funTypeInfo) (ftiType funTypeInfo)

-- | Checks that all super classes are defined and that there are no cycles in
-- inheritance.
checkInheritance :: TypeCheckM ()
checkInheritance = do
  classesAssoc <- getClassesAssoc
  -- running DFS from each vertex (class name)
  -- See comment on 'traverseHierarchy'
  foldM_ (\(marked, onStack) (className, classTypeInfo) -> do
      let mSuperClassName = ctiMSuperClassName classTypeInfo
      when (isJust mSuperClassName) $ do
        let superClassName = fromJust mSuperClassName
        unlessM (isClassDefined superClassName) $
          throwError $ ClassNotDefined (fromJust $ ctiMSuperSrcClassName classTypeInfo)

      -- Classical running of DFS from non-marked vertices
      if Set.notMember className marked
        then traverseHierarchy className marked onStack
        else return (marked, onStack)
    ) (Set.empty, Set.empty) classesAssoc

-- | 'traverseHierarchy' is basically a DFS function.  It takes a vertex (class
-- name), a set of marked vertices and a set of vertices which are currently on
-- the DFS stack ('traverseHierarchy' hasn't returned yet).
-- It returns a modified state (marked and onStack).
-- Algorithm is basically from Algorithms by Sedgewick and Wayne.
-- See http://algs4.cs.princeton.edu/44sp/DirectedCycle.java.html.
traverseHierarchy :: ClassName
                  -> Set.Set ClassName -> Set.Set ClassName
                  -> TypeCheckM (Set.Set ClassName, Set.Set ClassName)
traverseHierarchy className marked onStack = do
  -- mark current vertex (class name) and put it on the DFS stack
  let marked' = Set.insert className marked
      onStack' = Set.insert className onStack
  mSuperClassName <- getSuperClass className
  if isJust mSuperClassName  -- if there is an adjacent vertex
    then do
      let superClassName = fromJust mSuperClassName
      if Set.notMember superClassName marked'
        then traverseHierarchy superClassName marked' onStack'  -- DFS on adjacent vertex
        else if Set.member superClassName onStack'
               then throwError InheritanceCycle
               else return (marked', onStack)  -- remove current class name from the stack
    else return (marked', onStack)  -- remove current class name from the stack

-- | Checks all class members (in two passes) and adds them to the environment.
-- Returns type checked class definition.
tcClassDef :: SrcClassDef -> TypeCheckM TyClassDef
tcClassDef (ClassDef s srcClassName mSuperSrcClassName srcMembers) = do
  return $ ClassDef s srcClassName mSuperSrcClassName []  -- TODO

-- | Checks that all parameter names are distinct. Their types are already
-- checked and the function type is transformed. Adds parameters to the local
-- environment.
-- Checks function body (statements).
-- Checks that the actual return type is consistent with the specified return type.
-- Checks that the function is pure (if it is declared as such).
-- Returns type checked function definition.
tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef s srcFunName srcFunType srcStmts) = do
  -- collect function parameters and check for variable shadowing
  let varBinders = getFunParams srcFunType
  localTypeEnv <- foldM (\localTyEnv vb -> do
      let var = getVar (getBinderVar vb)
      isBound <- isVarBound var
      -- it is important to check in both places, since localTyEnv
      -- is not queried by 'isVarBound', see `FunParamsDup` test case
      when (isBound || isVarInLocalEnv var localTyEnv) $
        throwError $ VarShadowing (getBinderVar vb)
      varType <- srcTypeToType (getBinderType vb)
      return $ addLocalVar var varType localTyEnv)
    emptyLocalTypeEnv varBinders

  (tyStmts, stmtTypes) <-
    unzip <$> locallyWithEnv localTypeEnv (mapM tcStmt srcStmts)

  retType <- srcTypeToType $ getFunReturnType srcFunType
  -- There is at least one statement, the value of the last one (and hence the
  -- type) is what function returns.
  let lastStmtType = last stmtTypes
  unless (lastStmtType `isSubTypeOf` retType) $
    throwError $ FunIncorrectReturnType srcFunName (last tyStmts) retType lastStmtType

  when (isPure retType) $ do
    let mFirstImpureStmt = snd <$> find (not . isPure . fst) (zip stmtTypes tyStmts)
    case mFirstImpureStmt of
      Just firstImpureStmt -> throwError $ FunctionNotPure srcFunName firstImpureStmt
      Nothing -> return ()

  return $ FunDef s srcFunName srcFunType tyStmts

-- | Statement type checking.
-- Returns a type checked statement together with its type.
tcStmt :: SrcStmt -> TypeCheckM (TyStmt, Type)
tcStmt srcStmt =
  case srcStmt of
    ExprS s srcExpr -> do
      (tyExpr, exprType) <- tcExpr srcExpr
      return (ExprS s tyExpr, exprType)

-- | Expression type checking.
-- Returns a type checked expression together with its type.
tcExpr :: SrcExpr -> TypeCheckM (TyExpr, Type)
tcExpr srcExpr =
  case srcExpr of
    LitE srcLit -> return (LitE srcLit, typeOfLiteral $ getLiteral srcLit)

    VarE s var -> do
      -- it can be both a local variable and a global function
      unlessM (isVarBound var) $
        throwError $ VarNotBound var s
      varType <- getVarType var
      return (VarE s (VarTy (var, varType)), varType)

    ParenE s srcSubExpr -> do
      (tySubExpr, exprType) <- tcExpr srcSubExpr
      return (ParenE s tySubExpr, exprType)

-- | Subtyping relation.
-- It is reflexive (type is a subtype of itself).
-- Pure A `isSubTypeOf` B iff A `isSubTypeOf` B.
isSubTypeOf :: Type -> Type -> Bool
t1 `isSubTypeOf` t2 = t1 == t2 || pureSubType
  where pureSubType = case t1 of
                        TyPure pt1 -> pt1 `isSubTypeOf` t2
                        _ -> False

-- Type transformations

-- | Converts a source representation of type to an internal one.
-- Checks that all types are defined.
srcTypeToType :: SrcType -> TypeCheckM Type
srcTypeToType (SrcTyUnit _) = return TyUnit
srcTypeToType (SrcTyBool _) = return TyBool
srcTypeToType (SrcTyInt  _) = return TyInt
srcTypeToType (SrcTyClass srcClassName) = do
  let className = getClassName srcClassName
  unlessM (isClassDefined className) $
    throwError $ ClassNotDefined srcClassName
  return $ TyClass className
srcTypeToType (SrcTyArrow _ st1 st2) =
  TyArrow <$> srcTypeToType st1 <*> srcTypeToType st2
srcTypeToType (SrcTyPure _ t) =
  TyPure <$> srcTypeToType t

-- | Transforms function type which has variable binders and return type to one
-- big internal type (right associative type arrow without parameter names).
-- Checks that all used types are defined.
-- Does not check parameter names.
srcFunTypeToType :: SrcFunType -> TypeCheckM Type
srcFunTypeToType (FunType _ varBinders srcRetType) = do
  retType <- srcTypeToType srcRetType
  paramTypes <- mapM (srcTypeToType . getBinderType) varBinders
  return $ tyArrowFromList retType paramTypes

-- | Constructs an arrow type given a result type and a list of parameter
-- types.
tyArrowFromList :: Type -> [Type] -> Type
tyArrowFromList resultType = foldr (\t acc -> TyArrow t acc) resultType

