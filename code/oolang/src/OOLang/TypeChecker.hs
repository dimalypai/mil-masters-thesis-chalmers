module OOLang.TypeChecker
  ( typeCheck
  , typeCheckStage
  , TypeEnv
  , emptyTypeEnv
  , module OOLang.TypeChecker.TcError
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import OOLang.AST
import OOLang.TypeChecker.TcError

typeCheck :: SrcProgram -> Either TcError (TyProgram, TypeEnv)
--typeCheck program = runState (runErrorT $ runTC $ tcProgram program) ()
typeCheck program = runIdentity $
                    runErrorT $
                    runStateTFrom emptyTypeEnv $
                    runTC $ tcProgram program

typeCheckStage :: SrcProgram -> TypeEnv -> Either TcError (TyProgram, TypeEnv)
typeCheckStage program typeEnv = runIdentity $
                                 runErrorT $
                                 runStateTFrom typeEnv $
                                 runTC $ tcProgram program

--newtype TypeCheckM a = TC { runTC :: ErrorT TcError (State TypeEnv) a }
newtype TypeCheckM a = TC { runTC :: StateT TypeEnv (ErrorT TcError Identity) a }

type TypeEnv = ()

emptyTypeEnv :: TypeEnv
emptyTypeEnv = ()

tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram = undefined

tcClassDef :: SrcClassDef -> TypeCheckM TyClassDef
tcClassDef = undefined

tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef = undefined

-- Utils

runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

