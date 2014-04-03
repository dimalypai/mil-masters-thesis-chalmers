module OOLang.TypeChecker
  ( typeCheck
  , typeCheckStage
  , TypeEnv
  , initTypeEnv
  , module OOLang.TypeChecker.TcError
  ) where

import OOLang.AST
import OOLang.TypeChecker.TypeCheckM
import OOLang.TypeChecker.TcError

typeCheck :: SrcProgram -> Either TcError (TyProgram, TypeEnv)
typeCheck srcProgram = runTypeCheckM (tcProgram srcProgram) initTypeEnv

typeCheckStage :: SrcProgram -> TypeEnv -> Either TcError (TyProgram, TypeEnv)
typeCheckStage srcProgram typeEnv = runTypeCheckM (tcProgram srcProgram) typeEnv

tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram (Program s classDefs funDefs) = do
  -- TODO: collect defs
  -- TODO: check main
  -- TODO: check inheritance trees
  tyClassDefs <- mapM tcClassDef classDefs
  tyFunDefs <- mapM tcFunDef funDefs
  return $ Program s tyClassDefs tyFunDefs

tcClassDef :: SrcClassDef -> TypeCheckM TyClassDef
tcClassDef (ClassDef s srcClassName mSuperSrcClassName members) = do
  classTypeEnv <- getClassTypeEnv
  when (getClassName srcClassName `elem` classTypeEnv) $
    throwError $ OtherError "Class is already defined"
  modifyClassTypeEnv (getClassName srcClassName :)
  return $ ClassDef s srcClassName mSuperSrcClassName []

tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef s srcFunName srcFunType stmts isPure) = do
  return $ FunDef s srcFunName srcFunType [] isPure

