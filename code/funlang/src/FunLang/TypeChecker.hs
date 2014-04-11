-- | Type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
module FunLang.TypeChecker
  ( typeCheck
  , typeCheckStage
  , TypeEnv
  , initTypeEnv
  , TcError
  , prPrint
  ) where

import FunLang.AST
import FunLang.TypeChecker.TypeCheckM
import FunLang.TypeChecker.TcError
import FunLang.Utils

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
tcProgram (Program s typeDefs funDefs) = do
  collectDefs typeDefs funDefs
  -- Now first information about definitions is in the environment:
  -- + type names and their kinds
  -- + function names and their types
  checkMain
  tyTypeDefs <- mapM tcTypeDef typeDefs
  tyFunDefs <- mapM tcFunDef funDefs
  return $ Program s tyTypeDefs tyFunDefs

-- | In order to be able to handle (mutually) recursive definitions, we need to
-- do an additional first pass to collect function type signatures and type
-- names with their kinds.
--
-- It collects:
--
-- * type names and their kinds
--
-- * function names and their types
collectDefs :: [SrcTypeDef] -> [SrcFunDef] -> TypeCheckM ()
collectDefs typeDefs funDefs = do
  mapM_ collectTypeDef typeDefs
  mapM_ collectFunDef funDefs

-- Checks if the type is already defined.
-- If yes - throws an error, otherwise - adds the type name and its kind
-- to the type environment.
--
-- Note: it doesn't look inside the type definition (at the constructors).
-- It doesn't look at the names of the type variables either.
collectTypeDef :: SrcTypeDef -> TypeCheckM ()
collectTypeDef (TypeDef _ srcTypeName srcTypeVars _) = do
  whenM (isTypeDefined $ getTypeName srcTypeName) $
    throwError $ TypeAlreadyDefined srcTypeName
  let kind = mkKind (length srcTypeVars)
  addType srcTypeName kind

-- Checks if the function is already defined.
-- If yes - throws an error, otherwise - adds the function to the environment.
--
-- Note: it doesn't check if the specified type is well-formed and all
-- components are defined. Just collects the name and the specified type.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef _ srcFunName funSrcType _) = do
  whenM (isFunctionDefined $ getFunName srcFunName) $
    throwError $ FunctionAlreadyDefined srcFunName
  addFunction srcFunName funSrcType

-- | Program needs to have an entry point: `main : IO Unit`.
checkMain :: TypeCheckM ()
checkMain = do
  unlessM (isFunctionDefined $ FunName "main") $
    throwError MainNotDefined
  funTypeInfo <- getFunTypeInfo $ FunName "main"
  let mainType = TyApp (TypeName "IO") [TyApp (TypeName "Unit") []]
  t <- srcTypeToType (ftiSrcType funTypeInfo)
  when (t /= mainType) $
    throwError $ MainWrongType (ftiSrcType funTypeInfo)

tcTypeDef :: SrcTypeDef -> TypeCheckM SrcTypeDef
tcTypeDef (TypeDef s srcTypeName srcTypeVars conDefs) =
  return $ TypeDef s srcTypeName srcTypeVars conDefs

tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef s srcFunName funSrcType funEqs) =
  return $ FunDef s srcFunName funSrcType []

-- | Transforms a source representation of types to an internal one.
--
-- It does it in the type checking monad to be able to report an error, namely,
-- when the type is ill-formed (when something other than a type constructor or
-- another type application or parenthesised type is on the left-hand side of
-- the type application).
--
-- Most of the cases are straight-forward. 'SrcTyCon' which may stand for a
-- type variable or a type constructor is transformed to an application with
-- the empty argument list. Note, that we don't produce 'TyVar' at this point,
-- we will distinguish between them later on. The most interesting case is
-- 'SrcTyApp', which is binary as opposed to the 'TyApp'. We recursively dive
-- into the left-hand side of the application and collect transformed
-- right-hand sides to the arguments list until we hit the 'SrcTyCon'.
srcTypeToType :: SrcType -> TypeCheckM Type
srcTypeToType (SrcTyCon srcTypeName) = return $ TyApp (getTypeName srcTypeName) []
srcTypeToType st@(SrcTyApp _ stl str) = handleTyApp st stl str []
  where handleTyApp stApp st1 st2 args =
        -- stApp is the whole SrcTyApp - used for error message
        -- st1 and st2 are components of the type application
        -- in args we collect the arguments for the internal representation
          case st1 of
            SrcTyCon srcTypeName -> do
              t2 <- srcTypeToType st2
              return $ TyApp (getTypeName srcTypeName) (t2 : args)
            stApp'@(SrcTyApp _ st1' st2') -> do
              t2 <- srcTypeToType st2
              handleTyApp stApp' st1' st2' (t2 : args)
            -- just strip off the parens and replace the left-hand side
            SrcTyParen _ st' -> handleTyApp stApp st' st2 args
            _ -> throwError $ IllFormedType stApp
srcTypeToType (SrcTyArrow _ st1 st2) = do
  t1 <- srcTypeToType st1
  t2 <- srcTypeToType st2
  return $ TyArrow t1 t2
srcTypeToType (SrcTyForAll _ srcTypeVar st) = do
  t <- srcTypeToType st
  return $ TyForAll (getTypeVar srcTypeVar) t
srcTypeToType (SrcTyParen _ st) = srcTypeToType st

