-- | Type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
module OOLang.TypeChecker
  ( typeCheck
  , typeCheckStage
  , TypeEnv
  , initTypeEnv
  , TcError
  , prPrint
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)

import OOLang.AST
import OOLang.TypeChecker.TypeCheckM
import OOLang.TypeChecker.TcError
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

tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram (Program s classDefs funDefs) = do
  collectDefs classDefs funDefs
  -- Now first information about definitions is in the environment:
  -- * class names and their super classes
  -- * function names and their types and purity indicators
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
-- * function names and their types and purity indicators
collectDefs :: [SrcClassDef] -> [SrcFunDef] -> TypeCheckM ()
collectDefs classDefs funDefs = do
  mapM_ collectClassDef classDefs
  mapM_ collectFunDef funDefs

-- | Checks if the class was already defined.
-- If yes - throws an error, otherwise - adds the class to the environment.
collectClassDef :: SrcClassDef -> TypeCheckM ()
collectClassDef (ClassDef _ srcClassName mSuperSrcClassName _) = do
  whenM (isClassDefined $ getClassName srcClassName) $
    throwError $ ClassAlreadyDefined srcClassName
  addClass srcClassName mSuperSrcClassName

-- | Checks if the function was already defined.
-- If yes - throws an error, otherwise - adds the function to the environment.
-- Performs a function type transformation.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef _ srcFunName srcFunType _ isPure) = do
  whenM (isFunctionDefined $ getFunName srcFunName) $
    throwError $ FunctionAlreadyDefined srcFunName
  addFunction srcFunName srcFunType isPure

-- | Program needs to have an entry point: `main : Unit`.
checkMain :: TypeCheckM ()
checkMain = do
  unlessM (isFunctionDefined $ FunName "main") $
    throwError MainNotDefined
  funTypeInfo <- getFunTypeInfo $ FunName "main"
  when (ftiType funTypeInfo /= TyUnit) $
    throwError $ MainWrongType (ftiSrcFunType funTypeInfo)
  when (ftiIsPure funTypeInfo) $
    throwError $ MainPure (ftiSrcFunName funTypeInfo)

-- | Checks that all super classes are defined and that there are no cycles in
-- inheritance.
checkInheritance :: TypeCheckM ()
checkInheritance = do
  classTypeEnv <- getClassTypeEnv
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
    ) (Set.empty, Set.empty) (Map.assocs classTypeEnv)

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

tcClassDef :: SrcClassDef -> TypeCheckM TyClassDef
tcClassDef (ClassDef s srcClassName mSuperSrcClassName members) = do
  return $ ClassDef s srcClassName mSuperSrcClassName []

tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef s srcFunName srcFunType stmts isPure) = do
  return $ FunDef s srcFunName srcFunType [] isPure

