-- | Type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
--
-- Type checking produces a type environment with information about global
-- definitions and annotates some syntax nodes with their types. So, when we
-- say `type checked` and it is applicable to annotate something it also means
-- `annotated`.
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
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (find)
import Control.Applicative

import OOLang.AST
import OOLang.AST.PureAnnotated
import OOLang.AST.SrcAnnotated
import OOLang.AST.TypeAnnotated
import OOLang.AST.Helpers
import OOLang.TypeChecker.TypeCheckM
import OOLang.TypeChecker.TypeEnv
import OOLang.TypeChecker.TcError
import OOLang.TypeChecker.Helpers
import OOLang.SrcSpan
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
typeOf srcExpr typeEnv = (getTypeOf . fst) <$> runTypeCheckM (tcExpr Nothing srcExpr) typeEnv

-- | Entry point into the type checking of the program.
-- Returns a type checked program.
tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram (Program s classDefs funDefs) = do
  collectDefs classDefs funDefs
  -- Class names, function/method names and types (but *not* parameter names)
  -- and class field names and types (but *not* initialisers) have been
  -- checked.
  -- Now first information about definitions is in the environment:
  -- + class names, super classes (*not* checked), field and method names and
  -- types
  -- + function names and their types
  checkMain
  checkInheritance
  tyClassDefs <- mapM tcClassDef classDefs
  tyFunDefs <- mapM (tcFunDef Nothing) funDefs
  return $ Program s tyClassDefs tyFunDefs

-- | In order to be able to handle (mutually) recursive definitions and forward
-- references to super class members, we need to do an additional first pass to
-- collect function signatures, class names, super classes and class members.
--
-- It collects:
--
-- * class names, their super classes and members (field and method names and
-- types)
--
-- * function names and their types
--
-- It also does checking of function names and types, class field and method
-- names and types. But it does *not* check super classes, function/method
-- parameter names and field initialisers.
collectDefs :: [SrcClassDef] -> [SrcFunDef] -> TypeCheckM ()
collectDefs classDefs funDefs = do
  -- It is essential that we collect classes before functions, because function
  -- types may mention defined classes.
  mapM_ collectClassDef classDefs
  mapM_ collectFunDef funDefs

-- | Checks if the class is already defined.
-- Adds the class, its super class if it has one, field and method names and
-- types to the environment.
-- Field and method names and types are checked. Types are transformed.
-- Method parameter names and bodies and field initialisers are *not* checked.
-- Super class is *not* checked (whether it is defined).
collectClassDef :: SrcClassDef -> TypeCheckM ()
collectClassDef (ClassDef _ srcClassName mSuperSrcClassName srcMembers) = do
  let className = getClassName srcClassName
  whenM (isClassDefinedM className) $
    throwError $ ClassAlreadyDefined srcClassName
  addClassM className mSuperSrcClassName
  -- it is important that we add class to the environment first
  collectClassMembers className srcMembers

-- | Checks that all types of fields and methods are correct and defined.
-- Checks that field and method names are distinct. Checks that there are no
-- methods with the same name and different types (we don't allow overloading).
-- Transforms types to their internal representation.
-- Does *not* check method parameter names, method bodies and field
-- initialisers.
collectClassMembers :: ClassName -> [SrcMemberDecl] -> TypeCheckM ()
collectClassMembers className srcMembers = do
  let (srcFieldDecls, srcMethodDecls) = partitionClassMembers srcMembers
  mapM_ (collectClassField className) srcFieldDecls
  mapM_ (collectClassMethod className) srcMethodDecls

-- | Checks if a member with the same name is already defined in this hierarchy
-- (we don't allow field overriding/hiding).
-- Checks that the field doesn't have a name `self` and `super`.
-- Class fields can shadow global function names because we reference class
-- members with `self` or `super`.
-- Checks that the specified field type is correct (used types are defined).
collectClassField :: ClassName -> SrcFieldDecl -> TypeCheckM ()
collectClassField className (FieldDecl _ (Decl _ srcVarBinder _ _) _) = do
  let fieldName = getVar (getBinderVar srcVarBinder)
  let fieldNameSrcSpan = getSrcSpan (getBinderVar srcVarBinder)

  when (fieldName == Var "self") $
    throwError $ SelfMemberName fieldNameSrcSpan
  when (fieldName == Var "super") $
    throwError $ SuperMemberName fieldNameSrcSpan
  when (fieldName == Var "new") $
    throwError $ NewMemberName fieldNameSrcSpan

  let memberName = varToMemberName fieldName
  whenM (isClassMemberDefinedM className memberName) $
    throwError $ MemberAlreadyDefined memberName fieldNameSrcSpan

  fieldType <- srcTypeToType (getBinderSrcType srcVarBinder)
  addClassFieldM className fieldName fieldType

-- | Checks if a field with the same name is already defined.
-- Checks if a method with the same name and different type is already
-- defined in this hierarchy (we don't allow overloading/hiding).
-- Checks that the method doesn't have names: `self`, `super`, `new`.
-- Class methods can shadow global function names because we reference class
-- members with `self` or `super`.
-- Checks that the specified method type is correct (used types are defined).
-- Performs a method type transformation.
collectClassMethod :: ClassName -> SrcMethodDecl -> TypeCheckM ()
collectClassMethod className (MethodDecl _ (FunDef _ srcFunName srcFunType _) _) = do
  let methodName = getFunName srcFunName
  let methodNameSrcSpan = getSrcSpan srcFunName

  when (methodName == FunName "self") $
    throwError $ SelfMemberName methodNameSrcSpan
  when (methodName == FunName "super") $
    throwError $ SuperMemberName methodNameSrcSpan
  when (methodName == FunName "new") $
    throwError $ NewMemberName methodNameSrcSpan

  (methodType, retType, arity) <- srcFunTypeToType srcFunType
  let memberName = funNameToMemberName methodName
  whenM (isClassMemberDefinedM className memberName) $ do
    unlessM (isClassMethodOverrideM className methodName methodType) $
      throwError $ MemberAlreadyDefined memberName methodNameSrcSpan

  -- This means that even if a method is overriden in the subclass, it will not
  -- be present in the TypeEnv on its level, but rather only on the superclass
  -- level (where it was originally defined). This may not be the best
  -- solution, but it seems to cover the use cases so far. Another solution
  -- could be to fix the querying of the TypeEnv to return only unique methods.
  unlessM (isClassMethodOverrideM className methodName methodType) $
    addClassMethodM className methodName methodType retType arity srcFunType

-- | Checks if the function is already defined.
-- Checks that the specified function type is correct (used types are defined).
-- Performs a function type transformation.
-- Does *not* check parameter names.
-- Adds the function and its type to the environment.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef _ srcFunName srcFunType _) = do
  let funName = getFunName srcFunName
  whenM (isFunctionDefinedM funName) $
    throwError $ FunctionAlreadyDefined srcFunName
  (funType, retType, arity) <- srcFunTypeToType srcFunType
  addFunctionM funName funType retType arity srcFunType

-- | Program needs to have an entry point: `main : Unit`.
checkMain :: TypeCheckM ()
checkMain = do
  unlessM (isFunctionDefinedM $ FunName "main") $
    throwError MainNotDefined
  funTypeInfo <- getFunTypeInfoM $ FunName "main"
  when (ftiType funTypeInfo /= TyUnit) $
    throwError $ MainIncorrectType (ftiSrcFunType funTypeInfo) (ftiType funTypeInfo)

-- | Checks that all super classes are defined and that there are no cycles in
-- inheritance.
checkInheritance :: TypeCheckM ()
checkInheritance = do
  classesAssoc <- getClassesAssocM
  -- running DFS from each vertex (class name)
  -- See comment on 'traverseHierarchy'
  foldM_ (\(marked, onStack) (className, classTypeInfo) -> do
      let mSuperClassName = ctiMSuperClassName classTypeInfo
      when (isJust mSuperClassName) $ do
        let superClassName = fromJust mSuperClassName
        unlessM (isClassDefinedM superClassName) $
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
  mSuperClassName <- getSuperClassM className
  if isJust mSuperClassName  -- if there is an adjacent vertex
    then do
      let superClassName = fromJust mSuperClassName
      if Set.notMember superClassName marked'
        then do
          (marked'', onStack'') <-
            traverseHierarchy superClassName marked' onStack'  -- DFS on adjacent vertex
          -- This was not a tail call, we need to remove the current class name
          -- from the stack after the recursion is finished. See
          -- `NoInheritanceCycleRegression` test case.
          return (marked'', Set.delete className onStack'')
        else if Set.member superClassName onStack'
               then throwError InheritanceCycle
               else return (marked', onStack)  -- remove current class name from the stack
    else return (marked', onStack)  -- remove current class name from the stack

-- | Checks class definition.
-- Class members (types of fields and methods) have already been added to the
-- class type environment. Method parameter names and field initialisers have
-- *not* been checked.
-- Adds `super` (if it has a super class) to the local environment and runs
-- member checking in this environment.
-- Returns type checked class definition.
tcClassDef :: SrcClassDef -> TypeCheckM TyClassDef
tcClassDef (ClassDef s srcClassName mSuperSrcClassName srcMembers) = do
  let className = getClassName srcClassName
  let localTypeContext = case mSuperSrcClassName of
                           Nothing -> emptyLocalTypeContext
                           Just superSrcClassName ->
                             addLocalVar (Var "super") (TyClass $ getClassName superSrcClassName)
                               emptyLocalTypeContext
  tyMembers <- locallyWithContext localTypeContext (tcClassMembers className srcMembers)
  return $ ClassDef s srcClassName mSuperSrcClassName tyMembers

-- | Checks all class members.
-- First it checks all class field initialisers (we don't allow mutual
-- recursion for them as well as referencing class methods, but they can use
-- member access to super class fields via `super`).
-- Then it checks method parameter names and bodies (with `self` variable in
-- scope).
tcClassMembers :: ClassName -> [SrcMemberDecl] -> TypeCheckM [TyMemberDecl]
tcClassMembers className srcMembers = do
  let (srcFieldDecls, srcMethodDecls) = partitionClassMembers srcMembers
  tyFieldDecls <- mapM (tcClassField className) srcFieldDecls
  let localTypeContextSelf = addLocalVar (Var "self") (TyClass className)
                               emptyLocalTypeContext
  tyMethodDecls <- locallyWithContext localTypeContextSelf (mapM (tcClassMethod className) srcMethodDecls)
  return (map FieldMemberDecl tyFieldDecls ++ map MethodMemberDecl tyMethodDecls)

-- | Checks class field initialiser.
-- Its type and name are already checked.
-- Class field initialisers have to be pure.
-- Class fields are checked in the same local environment as function bodies
-- but possibly with `super` variable in scope.
-- Class fields can use member access to super class fields or methods via
-- `super`, but mutual recursion and accessing current class members is *not*
-- allowed.
tcClassField :: ClassName -> SrcFieldDecl -> TypeCheckM TyFieldDecl
tcClassField className (FieldDecl fs (Decl ds srcVarBinder mSrcInit _) _) = do
  let (VarBinder vs _ srcFieldName srcFieldType) = srcVarBinder
  let fieldName = getVar srcFieldName
  fieldType <- getClassFieldTypeM className fieldName
  mTyInit <-
    case mSrcInit of
      Just srcInit -> do
        tyInit <- tcInit (Just className) srcInit
        let initType = getTypeOf tyInit
        unless (getPurityOf tyInit) $
          throwError $ FieldInitNotPure fieldName fs
        let initOp = getInitOp $ getInitOpS srcInit
        case initOp of
          InitEqual ->
            unless (isImmutableType fieldType || isRefType fieldType) $
              throwError $ IncorrectImmutableOpUsage fs
          InitMut ->
            unless (isMutableType fieldType) $
              throwError $ IncorrectMutableOpUsage fs
        unlessM (initType `isSubTypeOf` fieldType) $
          throwError $ DeclInitIncorrectType srcInit fieldType initType
        return $ Just tyInit
      Nothing -> do
        unless (hasMaybeType fieldType) $
          throwError $ NonMaybeVarNotInit fieldName fs
        return Nothing
  let tyVarBinder = VarBinder vs fieldType srcFieldName srcFieldType
  let tyDecl = Decl ds tyVarBinder mTyInit True
  return $ FieldDecl fs tyDecl []

-- | Checks class method declaration.
-- See 'tcFunDef' for details.
tcClassMethod :: ClassName -> SrcMethodDecl -> TypeCheckM TyMethodDecl
tcClassMethod className (MethodDecl s srcFunDef _) = do
  tyFunDef <- tcFunDef (Just className) srcFunDef
  return $ MethodDecl s tyFunDef []

-- | Checks function/method definition.
-- Takes a class name if it is inside a class.
-- Its name and type are already in the environment.
-- Checks that all parameter names are distinct. Their types are already
-- checked and the function type is transformed. Adds parameters to the local
-- environment.
-- Checks function/method body (statements).
-- Checks that the actual return type is consistent with the specified return
-- type.
-- Checks that the function/method is pure (if it is declared as such).
-- Returns type checked function definition.
-- Class methods are checked in the same local environment as function bodies
-- but with `self` and possibly `super` variables in scope.
-- Class methods' parameter names can shadow class fields and methods (because
-- of explicit member access with `self` or `super`), but they can *not* shadow
-- global function names.
tcFunDef :: Maybe ClassName -> SrcFunDef -> TypeCheckM TyFunDef
tcFunDef mClassName (FunDef s srcFunName srcFunType srcStmts) = do
  -- collect function parameters and check for variable shadowing
  let (FunType fts srcVarBinders srcRetType) = srcFunType
  (localTypeContext, revTyVarBinders) <- foldM (\(localTyCtx, revTyVbs) svb -> do
      let (VarBinder vs _ srcVar srcVarType) = svb
      let var = getVar srcVar
      isBound <- isVarBoundM var
      -- it is important to check in both places, since localTyCtx
      -- is not queried by 'isVarBoundM', see `FunParamsDup` test case
      when (isBound || isVarBoundInTypeContext var localTyCtx) $
        throwError $ VarShadowing srcVar
      varType <- srcFunParamTypeToType srcVarType
      let tyVarBinder = VarBinder vs varType srcVar srcVarType
      return (addLocalVar var varType localTyCtx, tyVarBinder:revTyVbs))
    (emptyLocalTypeContext, []) srcVarBinders
  let tyFunType = FunType fts (reverse revTyVarBinders) srcRetType

  tyStmts <- locallyWithContext localTypeContext (mapM (tcStmt mClassName) srcStmts)

  retType <- unReturn <$> (srcFunReturnTypeToType $ getFunReturnType srcFunType)
  -- There is at least one statement, the value of the last one (and hence the
  -- type) is what function returns.
  let lastStmtType = getTypeOf $ last tyStmts
  unlessM (lastStmtType `isSubTypeOf` retType) $
    throwError $ FunIncorrectReturnType srcFunName (last tyStmts) (stripPureType retType) lastStmtType

  when (isPureType retType) $ do
    let mFirstImpureStmt = find (not . getPurityOf) tyStmts
    case mFirstImpureStmt of
      Just firstImpureStmt -> throwError $ FunctionNotPure srcFunName firstImpureStmt
      Nothing -> return ()

  return $ FunDef s srcFunName tyFunType tyStmts

-- | Note [Assignment purity]:
--
-- Assignments to Mutable variables are pure, since they are purely local and
-- don't influence the global state of the program/world (they don't break
-- referential transparency).
-- Assignments to Mutable fields of the class inside the class is impure, since
-- they modify the internal state of the object. We don't need to have a
-- special treatment of assignments to fields outside of the class (not via
-- `self` or `super`), since all class fields are not visible from the outside
-- of the class and its subclasses.
-- Assignments to Refs are considered impure.

-- | Statement type checking.
-- Takes a class name if it is inside a class.
-- Returns a type checked statement.
tcStmt :: Maybe ClassName -> SrcStmt -> TypeCheckM TyStmt
tcStmt mClassName srcStmt =
  case srcStmt of
    DeclS s srcDecl ->
      DeclS s <$> tcDecl mClassName srcDecl srcStmt

    ExprS s srcExpr ->
      ExprS s <$> tcExpr mClassName srcExpr

    -- See Note [Assignment purity]
    AssignS s srcAssignOp srcExprLeft srcExprRight _ -> do
      let assignOp = getAssignOp srcAssignOp

      (tyExprLeft, assignPurity) <-
        case srcExprLeft of
          VarE varS _ var _ -> do
            tyVar <- tcExpr mClassName srcExprLeft
            whenM (isFunctionDefinedM $ varToFunName var) $
              throwError $ AssignToFunction varS
            return (tyVar, not $ isAssignRef assignOp)

          MemberAccessE ms _ srcObjExpr srcMemberName _ -> do
            -- Check the whole member access expression: whether it is defined,
            -- and most importantly - for class field access check
            -- (visibility).
            -- TODO: can it be harmful to perform double checking?
            tyMemberAccess <- tcExpr mClassName srcExprLeft
            tyObjExpr <- tcExpr mClassName srcObjExpr
            className <- tryGetClassName (getTypeOf tyObjExpr) srcObjExpr
            let fieldName = memberNameToVar (getMemberName srcMemberName)
            -- Visibility has already been checked with 'tcExpr', so if there
            -- is a field, it can be assigned to.
            unlessM (isClassFieldDefinedM className fieldName) $
              throwError $ AssignNotField fieldName className (getSrcSpan srcMemberName)
            fieldType <- getClassFieldTypeM className fieldName
            return (tyMemberAccess, False)

          _ -> throwError $ IncorrectAssignLeft (getSrcSpan srcExprLeft)

      let exprLeftType = getTypeOf tyExprLeft
      tyExprRight <- tcExpr mClassName srcExprRight
      let exprRightType = getTypeOf tyExprRight

      case assignOp of
        AssignMut -> do
          unless (isMutableType exprLeftType) $
            throwError $ IncorrectMutableOpUsage (getSrcSpan srcStmt)
          unlessM (exprRightType `isSubTypeOf` exprLeftType) $
            throwError $ AssignIncorrectType tyExprRight (getUnderType exprLeftType) exprRightType
        AssignRef ->
          case exprLeftType of
            TyRef exprLeftUnderType ->
              unlessM (exprRightType `isSubTypeOf` exprLeftUnderType) $
                throwError $ AssignIncorrectType tyExprRight exprLeftUnderType exprRightType
            _ -> throwError $ IncorrectRefOpUsage (getSrcSpan srcStmt)

      let exprRightPure = getPurityOf tyExprRight
      return $ AssignS s srcAssignOp tyExprLeft tyExprRight (assignPurity && exprRightPure)

    TryS s srcTryStmts srcCatchStmts srcFinallyStmts -> do
      tyTryStmts <- locally $ mapM (tcStmt mClassName) srcTryStmts
      tyCatchStmts <- locally $ mapM (tcStmt mClassName) srcCatchStmts
      tyFinallyStmts <- locally $ mapM (tcStmt mClassName) srcFinallyStmts

      let tryBlockType = getTypeOf $ last tyTryStmts
      if null tyCatchStmts
        then when (tryBlockType /= TyUnit) $
               throwError $ EmptyCatchBlockNonUnit tryBlockType s
        else do let catchBlockType = getTypeOf (last tyCatchStmts)
                when (tryBlockType /= catchBlockType) $
                  throwError $ CatchIncorrectType tryBlockType catchBlockType (getSrcSpan $ last tyCatchStmts)

      return $ TryS s tyTryStmts tyCatchStmts tyFinallyStmts

-- | Note [Purity of declarations]:
--
-- Mark declarations without initialisation as pure.
-- In order for declaration to be pure, its initialisation expression must be
-- pure.

-- | Declaration type checking. Takes a declaration source statement for error
-- messages.
-- Takes a class name if it is inside a class.
-- Returns a type checked declaration.
tcDecl :: Maybe ClassName -> SrcDeclaration -> SrcStmt -> TypeCheckM TyDeclaration
tcDecl mClassName (Decl s srcVarBinder mSrcInit _) srcDeclStmt = do
  let (VarBinder vs _ srcVar varSrcType) = srcVarBinder
  let var = getVar srcVar
  whenM (isVarBoundM var) $
    throwError $ VarShadowing srcVar
  varType <- srcTypeToType varSrcType
  addLocalVarM var varType
  let tyVarBinder = VarBinder vs varType srcVar varSrcType
  -- See Note [Purity of declarations]
  case mSrcInit of
    Just srcInit -> do
      tyInit <- tcInit mClassName srcInit
      let initOp = getInitOp $ getInitOpS srcInit
      case initOp of
        InitEqual ->
          unless (isImmutableType varType || isRefType varType) $
            throwError $ IncorrectImmutableOpUsage (getSrcSpan srcDeclStmt)
        InitMut ->
          unless (isMutableType varType) $
            throwError $ IncorrectMutableOpUsage (getSrcSpan srcDeclStmt)
      let initType = getTypeOf tyInit
      unlessM (initType `isSubTypeOf` varType) $
        throwError $ DeclInitIncorrectType srcInit (getUnderType varType) initType
      return $ Decl s tyVarBinder (Just tyInit) (getPurityOf tyInit)
    Nothing -> do
      unless (hasMaybeType varType) $
        throwError $ NonMaybeVarNotInit var (getSrcSpan srcDeclStmt)
      return $ Decl s tyVarBinder Nothing True

-- | Initialisation expression type checking.
-- Takes a class name if it is inside a class.
-- Returns a type checked initialisation expression.
tcInit :: Maybe ClassName -> SrcInit -> TypeCheckM TyInit
tcInit mClassName (Init s srcInitOp srcExpr) = do
  tyExpr <- tcExpr mClassName srcExpr
  return $ Init s srcInitOp tyExpr

-- | Note [Purity of function and value types]:
--
-- This applies when checking variable/function references and member access.
--
-- * If it is a global function or a method and it does not take any arguments,
-- then in order to be pure, it must have Pure type. This applies when function
-- arity is 0, meaning that it didn't have any parameter binders in its
-- definition. It still can be a function (arrow) type.
--
-- * If it *does* take arguments, then it is pure, since we just reference its
-- name and don't run the computation, it will be checked when it is fully
-- applied. Full application in this case means that all parameter binders are
-- used, not that it has value type at the end.
--
-- * If it is a local variable (parameter) or a class field, then it is always
-- pure, since if it has a value type, it has been computed already (we are in
-- a strict language), and if it has a function type, then we again only
-- reference its name, and its purity should be checked when applied.

-- | Expression type checking.
-- Takes a class name if it is inside a class.
-- Returns a type checked expression.
tcExpr :: Maybe ClassName -> SrcExpr -> TypeCheckM TyExpr
tcExpr mClassName srcExpr =
  case srcExpr of
    LitE srcLit -> LitE <$> tcLit srcLit

    VarE s _ var _ -> do
      -- it can be both a local variable and a global function
      unlessM (isVarBoundM var) $
        throwError $ VarNotBound var s
      varType <- getVarTypeM var
      -- See Note [Purity of function and value types]
      let funName = varToFunName var
      isPure <- ifM (isFunctionDefinedM funName)
                  (do retType <- unReturn <$> (ftiReturnType <$> getFunTypeInfoM funName)
                      arity <- ftiArity <$> getFunTypeInfoM funName
                      return (arity /= 0 || isPureType retType))
                  (return True)
      return $ VarE s varType var isPure

    MemberAccessE s _ srcObjExpr srcMemberName _ -> do
      tyObjExpr <- tcExpr mClassName srcObjExpr
      let objType = getTypeOf tyObjExpr
      className <- tryGetClassName objType srcObjExpr
      let memberName = getMemberName srcMemberName
      unlessM (isClassMemberDefinedM className memberName) $
        throwError $ MemberNotDefined memberName className srcExpr
      isField <- isClassFieldDefinedM className (memberNameToVar memberName)
      when (isField && (isNothing mClassName || not (isSelfOrSuper srcObjExpr))) $
        throwError $ OutsideFieldAccess s
      memberType <- getClassMemberTypeM className memberName
      -- See Note [Purity of function and value types]
      let methodName = memberNameToFunName memberName
      memberPure <- ifM (isClassMethodDefinedM className methodName)
                      (do retType <- unReturn <$> (ftiReturnType <$> getClassMethodTypeInfoM className methodName)
                          arity <- ftiArity <$> getClassMethodTypeInfoM className methodName
                          return (arity /= 0 || isPureType retType))
                      (return True)
      let objPure = getPurityOf tyObjExpr
      return $ MemberAccessE s memberType tyObjExpr srcMemberName (objPure && memberPure)

    MemberAccessMaybeE s _ srcMaybeObjExpr srcMemberName _ -> do
      tyMaybeObjExpr <- tcExpr mClassName srcMaybeObjExpr
      let objMaybeType = getTypeOf tyMaybeObjExpr
      case objMaybeType of
        TyMaybe objType -> do
          className <- tryGetClassName objType srcMaybeObjExpr
          let methodName = memberNameToFunName $ getMemberName srcMemberName
          unlessM (isClassMethodDefinedM className methodName) $
            throwError $ MethodNotDefined methodName className srcExpr
          methodType <- getClassMethodTypeM className methodName
          -- See Note [Purity of function and value types]
          methodPure <- do
            retType <- unReturn <$> (ftiReturnType <$> getClassMethodTypeInfoM className methodName)
            arity <- ftiArity <$> getClassMethodTypeInfoM className methodName
            return (arity /= 0 || isPureType retType)
          let objPure = getPurityOf tyMaybeObjExpr
          return $ MemberAccessMaybeE s (TyMaybe methodType) tyMaybeObjExpr srcMemberName (objPure && methodPure)
        _ -> throwError $ MemberAccessMaybeWithNonMaybe srcExpr objMaybeType

    ClassAccessE s _ srcClassName srcMethodName -> do
      let className = getClassName srcClassName
          methodName = getFunName srcMethodName
      unless (methodName == FunName "new") $
        throwError $ ClassAccessNotNew s
      -- Constructor is parameterless and pure and it is always "defined"
      -- (there is just the default one). Purity comes from the fact that all
      -- field initialisers are pure.
      return $ ClassAccessE s (TyClass className) srcClassName srcMethodName

    -- Reference operations are impure
    NewRefE s _ srcRefUnderExpr -> do
      tyRefUnderExpr <- tcExpr mClassName srcRefUnderExpr
      let refUnderType = getTypeOf tyRefUnderExpr
      when (isRefType refUnderType) $
        throwError $ NestedRefCreation s
      return $ NewRefE s (TyRef refUnderType) tyRefUnderExpr

    DerefE s _ srcRefExpr -> do
      tyRefExpr <- tcExpr mClassName srcRefExpr
      let refType = getTypeOf tyRefExpr
      case refType of
        TyRef refUnderType ->
          return $ DerefE s refUnderType tyRefExpr
        _ -> throwError $ NonRefDeref s

    BinOpE s _ srcBinOp srcExpr1 srcExpr2 _ -> do
      let op = getBinOp srcBinOp
      (tyExpr1, tyExpr2, resultType, resultPure) <- tcBinOp mClassName op srcExpr1 srcExpr2
      return $ BinOpE s resultType srcBinOp tyExpr1 tyExpr2 resultPure

    JustE s _ srcSubExpr -> do
      tySubExpr <- tcExpr mClassName srcSubExpr
      return $ JustE s (TyMaybe $ getTypeOf tySubExpr) tySubExpr

    ParenE s srcSubExpr ->
      ParenE s <$> tcExpr mClassName srcSubExpr

tcLit :: SrcLiteral -> TypeCheckM TyLiteral
tcLit srcLit =
  case srcLit of
    UnitLit s _ -> UnitLit s <$> typeOfLiteral srcLit
    BoolLit s _ b -> BoolLit s <$> typeOfLiteral srcLit <*> return b
    IntLit s _ i -> IntLit s <$> typeOfLiteral srcLit <*> return i
    FloatLit s _ f str -> FloatLit s <$> typeOfLiteral srcLit <*> return f <*> return str
    StringLit s _ str -> StringLit s <$> typeOfLiteral srcLit <*> return str
    NothingLit s _ st -> NothingLit s <$> typeOfLiteral srcLit <*> return st

typeOfLiteral :: SrcLiteral -> TypeCheckM Type
typeOfLiteral UnitLit    {}     = return TyUnit
typeOfLiteral (BoolLit   {})    = return TyBool
typeOfLiteral (IntLit    {})    = return TyInt
typeOfLiteral (FloatLit  {})    = return TyFloat
typeOfLiteral (StringLit {})    = return TyString
typeOfLiteral (NothingLit _ _ st) = do
  nothingType <- srcTypeToType st
  unless (isMaybeType nothingType) $
    throwError $ IncorrectNothingType st
  return nothingType

-- | Returns a class name for 'TyClass', 'TyMutable' or 'TyPure' which contains
-- 'TyClass' inside. Throws an error for Maybe (with a suggestion for using `?`
-- member access) and all other types.
-- Takes an expression for error reporting.
tryGetClassName :: Type -> Expr t SrcSpan -> TypeCheckM ClassName
tryGetClassName objType expr =
  case objType of
    TyClass className -> return className
    TyMutable mutUnderType -> tryGetClassName mutUnderType expr
    TyPure pureUnderType -> tryGetClassName pureUnderType expr
    TyMaybe {} -> throwError $ MemberAccessWithMaybe expr objType
    _ -> throwError $ NotObject expr objType

-- * Binary operations type checking

-- | Type checks a given binary operation. Takes operands.
-- Takes a class name if it is inside a class.
-- Returns type checked operands, a type of the result and its purity
-- indicator.
tcBinOp :: Maybe ClassName -> BinOp -> SrcExpr -> SrcExpr -> TypeCheckM (TyExpr, TyExpr, Type, Bool)
tcBinOp mClassName op srcExpr1 srcExpr2 = do
  tyExpr1 <- tcExpr mClassName srcExpr1
  tyExpr2 <- tcExpr mClassName srcExpr2
  (resultType, resultPure) <- case op of
    App -> tcApp tyExpr1 tyExpr2 mClassName
    NothingCoalesce -> tcNothingCoalesce tyExpr1 tyExpr2
    Add -> tcArith tyExpr1 tyExpr2
    Sub -> tcArith tyExpr1 tyExpr2
    Mul -> tcArith tyExpr1 tyExpr2
    Div -> tcArith tyExpr1 tyExpr2
    Equal -> tcCompare tyExpr1 tyExpr2
    NotEq -> tcCompare tyExpr1 tyExpr2
    Less -> tcCompare tyExpr1 tyExpr2
    Greater -> tcCompare tyExpr1 tyExpr2
    LessEq -> tcCompare tyExpr1 tyExpr2
    GreaterEq -> tcCompare tyExpr1 tyExpr2
  return (tyExpr1, tyExpr2, resultType, resultPure)

-- | Type synonym for binary operation type checking functions.
-- They take type checked operands and return a type of the result and its
-- purity indicator.
type BinOpTc = TyExpr -> TyExpr -> TypeCheckM (Type, Bool)

-- | Function application type checking.
-- Takes a class name if it happens inside a class.
tcApp :: TyExpr -> TyExpr -> Maybe ClassName -> TypeCheckM (Type, Bool)
tcApp tyExpr1 tyExpr2 mClassName =
  -- Function can have Pure type at the top, which we need to discard.
  let expr1Type = stripPureType $ getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2
  in case expr1Type of
       TyArrow argType resultType ->
         ifM (not <$> expr2Type `isSubTypeOf` argType)
           (throwError $ IncorrectFunArgType tyExpr2 argType expr2Type)
           (do
             -- Function performs side effects only when fully applied, so if
             -- it does not take more arguments, then we check for purity,
             -- otherwise - postpone this check. Full application here means:
             -- until return type (all parameter binders are used). It still
             -- can return a function, but impurely.

             -- Try to figure out the purity of result (approximation).
             resultPure <-
               case tyExpr1 of
                 VarE _ _ var _ -> do
                   let funName = varToFunName var
                   ifM (isFunctionDefinedM funName)
                     (do retType <- unReturn <$> (ftiReturnType <$> getFunTypeInfoM funName)
                         arity <- ftiArity <$> getFunTypeInfoM funName
                         let parametersLeft = arity - 1 /= 0
                         return (parametersLeft || isPureType retType))
                     (return $ isPureType resultType)
                 MemberAccessE _ _ tyObjExpr srcMemberName _ -> do
                   let methodName = memberNameToFunName $ getMemberName srcMemberName
                   className <- tryGetClassName (getTypeOf tyObjExpr) tyObjExpr
                   ifM (isClassMethodDefinedM className methodName)
                     (do retType <- unReturn <$> (ftiReturnType <$> getClassMethodTypeInfoM className methodName)
                         arity <- ftiArity <$> getClassMethodTypeInfoM className methodName
                         let parametersLeft = arity - 1 /= 0
                         return (parametersLeft || isPureType retType))
                     (return $ isPureType resultType)
                 MemberAccessMaybeE {} -> undefined
                   -- Should have been the same as MemberAccessE, but at the
                   -- moment this is not possible, because types don't match to
                   -- do this kind of application.
                 _ -> return (isPureType resultType)
             let expr1Pure = getPurityOf tyExpr1
                 expr2Pure = getPurityOf tyExpr2
             return (resultType, expr1Pure && expr2Pure && resultPure))
       _ -> throwError $ NotFunctionType tyExpr1 expr1Type

tcNothingCoalesce :: BinOpTc
tcNothingCoalesce tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2
  in case expr1Type of
       TyMaybe t -> ifM (not <$> expr2Type `isSubTypeOf` t)
                      (throwError $ IncorrectExprType t expr2Type (getSrcSpan tyExpr2))
                      (return (t, getPurityOf tyExpr1 && getPurityOf tyExpr2))
       _ -> throwError $ NothingCoalesceNonMaybe expr1Type (getSrcSpan tyExpr2)

tcArith :: BinOpTc
tcArith tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2 in
  case (isArithType expr1Type, isArithType expr2Type, expr2Type == expr1Type) of
    (False, _, _) -> throwError $ NotArithType expr1Type (getSrcSpan tyExpr1)
    (True, False, _) -> throwError $ NotArithType expr2Type (getSrcSpan tyExpr2)
    (True, True, False) -> throwError $ IncorrectExprType expr1Type expr2Type (getSrcSpan tyExpr2)
    (True, True, True) -> return (expr1Type, getPurityOf tyExpr1 && getPurityOf tyExpr2)

tcCompare :: BinOpTc
tcCompare tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2 in
  case (isComparableType expr1Type, isComparableType expr2Type, expr2Type == expr1Type) of
    (False, _, _) -> throwError $ NotComparableType expr1Type (getSrcSpan tyExpr1)
    (True, False, _) -> throwError $ NotComparableType expr2Type (getSrcSpan tyExpr2)
    (True, True, False) -> throwError $ IncorrectExprType expr1Type expr2Type (getSrcSpan tyExpr2)
    (True, True, True) -> return (TyBool, getPurityOf tyExpr1 && getPurityOf tyExpr2)

