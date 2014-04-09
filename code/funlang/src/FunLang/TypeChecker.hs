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
tcProgram = undefined

