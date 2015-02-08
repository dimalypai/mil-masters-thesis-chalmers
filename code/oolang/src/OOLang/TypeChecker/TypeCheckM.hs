{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
-- This module is mostly for working with type environment (querying, adding)
-- via monadic wrappers for pure functions from "OOLang.TypeChecker.TypeEnv".
-- Nothing smart should happen here.
module OOLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM

  , isClassDefinedM
  , isClassMemberDefinedM
  , isClassFieldDefinedM
  , isClassMethodDefinedM
  , isClassMethodOverrideM
  , addClassM
  , addClassFieldM
  , addClassMethodM
  , getClassMemberTypeM
  , getClassFieldTypeM
  , getClassMethodTypeM
  , getSuperClassM
  , getClassesAssocM

  , isFunctionDefinedM
  , addFunctionM
  , getFunTypeInfoM

  , isVarBoundM
  , addLocalVarM
  , getVarTypeM
  , locallyWithContext
  , locally

  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
-- 'first' and 'second' are used just to transform components of a pair
import Control.Arrow (first, second)

import OOLang.AST
import OOLang.AST.Helpers
import OOLang.TypeChecker.TypeEnv
import OOLang.TypeChecker.TcError
import OOLang.Utils

-- | Type checking monad. Uses 'StateT' for type environment and 'ErrorT' for
-- error handling.
newtype TypeCheckM a = TC { runTC :: StateT TypeCheckMState (ErrorT TcError Identity) a }
  deriving (Monad, MonadState TypeCheckMState, MonadError TcError, Functor, Applicative)

type TypeCheckMState = (TypeEnv, LocalTypeEnv)

-- | Main entry point to the type checking monad.
-- Get a type checking computation and a type environment to begin with.
runTypeCheckM :: TypeCheckM a -> TypeEnv -> Either TcError (a, TypeEnv)
runTypeCheckM tcm typeEnv = fmap dropLocalTypeEnv $
                            runIdentity $
                            runErrorT $
                            runStateTFrom (typeEnv, emptyLocalTypeEnv) $
                            runTC tcm

getTypeEnv :: TypeCheckMState -> TypeEnv
getTypeEnv = fst

getLocalTypeEnv :: TypeCheckMState -> LocalTypeEnv
getLocalTypeEnv = snd

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> TypeCheckM ()
modifyTypeEnv = modify . first

modifyLocalTypeEnv :: (LocalTypeEnv -> LocalTypeEnv) -> TypeCheckM ()
modifyLocalTypeEnv = modify . second

dropLocalTypeEnv :: (a, (TypeEnv, LocalTypeEnv))
                 -> (a, TypeEnv)
dropLocalTypeEnv = second getTypeEnv

-- * Class type environment

getClassTypeEnvM :: TypeCheckM ClassTypeEnv
getClassTypeEnvM = gets (getClassTypeEnv . getTypeEnv)

modifyClassTypeEnv :: (ClassTypeEnv -> ClassTypeEnv) -> TypeCheckM ()
modifyClassTypeEnv f = do
  (classTypeEnv, funTypeEnv) <- (,) <$> getClassTypeEnvM <*> getFunTypeEnvM
  modifyTypeEnv (const $ mkTypeEnv (f classTypeEnv) funTypeEnv)

isClassDefinedM :: ClassName -> TypeCheckM Bool
isClassDefinedM className = isClassDefined className <$> getClassTypeEnvM

-- | Looks for a member in the whole class hierarchy.
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassMemberDefinedM :: ClassName -> MemberName -> TypeCheckM Bool
isClassMemberDefinedM className memberName = do
  let fieldName = memberNameToVar memberName
      methodName = memberNameToFunName memberName
  isField <- isClassFieldDefinedM className fieldName
  isMethod <- isClassMethodDefinedM className methodName
  return (isField || isMethod)

-- | Looks for a field in the whole class hierarchy.
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassFieldDefinedM :: ClassName -> Var -> TypeCheckM Bool
isClassFieldDefinedM className fieldName =
  isClassFieldDefined className fieldName <$> getClassTypeEnvM

-- | Looks for a method in the whole class hierarchy.
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassMethodDefinedM :: ClassName -> FunName -> TypeCheckM Bool
isClassMethodDefinedM className methodName =
  isClassMethodDefined className methodName <$> getClassTypeEnvM

-- | Checks whether there is a method with a given name and type in on of the
-- super classes (does *not* check a given class).
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassMethodOverrideM :: ClassName -> FunName -> Type -> TypeCheckM Bool
isClassMethodOverrideM className methodName methodType =
  isClassMethodOverride className methodName methodType <$> getClassTypeEnvM

-- | Doesn't check if the class is already in the environment.
-- Will overwrite it in this case.
addClassM :: ClassName -> Maybe SrcClassName -> TypeCheckM ()
addClassM className mSuperSrcClassName = do
  let mSuperClassName = getClassName <$> mSuperSrcClassName
  modifyClassTypeEnv $
    addClass className mSuperClassName mSuperSrcClassName

-- | Doesn't check if the field is already in the environment.
-- Will overwrite it in this case.
--
-- Note: Unsafe. Should be used only after check that class is defined.
addClassFieldM :: ClassName -> Var -> Type -> TypeCheckM ()
addClassFieldM className fieldName fieldType =
  modifyClassTypeEnv $ addClassField className fieldName fieldType

-- | Doesn't check if the method is already in the environment.
-- Will overwrite it in this case.
--
-- Note: Unsafe. Should be used only after check that class is defined.
addClassMethodM :: ClassName -> FunName -> Type -> ReturnType -> Int -> SrcFunType -> TypeCheckM ()
addClassMethodM className methodName methodType retType arity srcMethodType =
  modifyClassTypeEnv $ addClassMethod className methodName methodType retType arity srcMethodType

-- | Returns a type of the class member.
--
-- Note: Unsafe. Should be used only after check that the class and the member
-- are defined.
getClassMemberTypeM :: ClassName -> MemberName -> TypeCheckM Type
getClassMemberTypeM className memberName =
  getClassMemberType className memberName <$> getClassTypeEnvM

-- | Returns a type of the class field.
--
-- Note: Unsafe. Should be used only after check that the class and the field
-- are defined.
getClassFieldTypeM :: ClassName -> Var -> TypeCheckM Type
getClassFieldTypeM className fieldName =
  getClassFieldType className fieldName <$> getClassTypeEnvM

-- | Returns a type of the class method.
--
-- Note: Unsafe. Should be used only after check that the class and the method
-- are defined.
getClassMethodTypeM :: ClassName -> FunName -> TypeCheckM Type
getClassMethodTypeM className methodName =
  getClassMethodType className methodName <$> getClassTypeEnvM

-- | Returns a super class of the given class if it has one.
--
-- Note: Unsafe. Should be used only after check that class is defined.
getSuperClassM :: ClassName -> TypeCheckM (Maybe ClassName)
getSuperClassM className = getSuperClass className <$> getClassTypeEnvM

-- | Get class type environment as an associative list.
getClassesAssocM :: TypeCheckM [(ClassName, ClassTypeInfo)]
getClassesAssocM = getClassesAssoc <$> getClassTypeEnvM

-- * Function type environment

getFunTypeEnvM :: TypeCheckM FunTypeEnv
getFunTypeEnvM = gets (getFunTypeEnv . getTypeEnv)

modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (classTypeEnv, funTypeEnv) <- (,) <$> getClassTypeEnvM <*> getFunTypeEnvM
  modifyTypeEnv (const $ mkTypeEnv classTypeEnv (f funTypeEnv))

isFunctionDefinedM :: FunName -> TypeCheckM Bool
isFunctionDefinedM funName = isFunctionDefined funName <$> getFunTypeEnvM

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunctionM :: FunName -> Type -> ReturnType -> Int -> SrcFunType -> TypeCheckM ()
addFunctionM funName funType retType arity srcFunType =
  modifyFunTypeEnv $ addFunction funName funType retType arity srcFunType

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfoM :: FunName -> TypeCheckM FunTypeInfo
getFunTypeInfoM funName = getFunTypeInfo funName <$> getFunTypeEnvM

-- * Local type environment

-- | Check whether a variable is in scope. It can be either local variable name
-- or a function name.
isVarBoundM :: Var -> TypeCheckM Bool
isVarBoundM var = do
  isLocalVar <- gets (isVarBound var . getLocalTypeEnv)
  isFunction <- isFunctionDefinedM (varToFunName var)
  return (isLocalVar || isFunction)

-- | Monadic function for adding variables to the local environment.
-- Doesn't check if the variable is already in the environment.
-- Will overwrite it in this case.
addLocalVarM :: Var -> Type -> TypeCheckM ()
addLocalVarM var varType =
  modifyLocalTypeEnv (modifyLocalTypeContext $ addLocalVar var varType)

-- | Returns variable type. First looks for locals and then for functions.
--
-- Note: Unsafe. Should be used only after check that the variable is bound.
getVarTypeM :: Var -> TypeCheckM Type
getVarTypeM var = do
  mVarType <- gets (getVarType var . getLocalTypeEnv)
  case mVarType of
    Just varType -> return varType
    Nothing -> do
      funTypeInfo <- getFunTypeInfoM (varToFunName var)
      return $ ftiType funTypeInfo

-- | Takes a separate local type context and puts it on top of the current
-- local type environment and performs a given computation in this new
-- environment. Then restores the local environment (by removing the top
-- context).
locallyWithContext :: LocalTypeContext -> TypeCheckM a -> TypeCheckM a
locallyWithContext localTypeContext tcm = do
  modifyLocalTypeEnv (addLocalTypeContext localTypeContext)
  a <- tcm
  modifyLocalTypeEnv removeLocalTypeContext
  return a

-- | Creates a new empty local type context on top of the current local type
-- environment and performs a given computation in this new environment. Then
-- restores the local environment (by removing the top context). Thus, all
-- variables declared in the block dissapear at the end.
locally :: TypeCheckM a -> TypeCheckM a
locally tcm = do
  modifyLocalTypeEnv addEmptyLocalTypeContext
  a <- tcm
  modifyLocalTypeEnv removeLocalTypeContext
  return a

