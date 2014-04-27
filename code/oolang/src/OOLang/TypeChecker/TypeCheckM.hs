{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module containing type checking monad and API for building a type checker.
-- This module mostly is for working with type environment (querying, adding).
-- Nothing smart should happen here.
module OOLang.TypeChecker.TypeCheckM
  ( TypeCheckM
  , runTypeCheckM

  , TypeEnv
  , initTypeEnv

  , ctiMSuperClassName
  , ctiSrcClassName
  , ctiMSuperSrcClassName
  , getClassesAssoc
  , isClassDefined
  , isClassMemberDefined
  , isClassFieldDefined
  , isClassMethodDefined
  , isClassMethodOverride
  , addClass
  , addClassField
  , addClassMethod
  , getClassMemberType
  , getClassFieldType
  , getSuperClass

  , ftiType
  , ftiSrcFunName
  , ftiSrcFunType
  , isFunctionDefined
  , addFunction
  , getFunTypeInfo

  , emptyLocalTypeEnv
  , isVarBound
  , isVarInLocalEnv
  , getVarType
  , addLocalVar
  , addLocalVarM
  , locallyWithEnv

  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
-- 'first' and 'second' are used just to transform components of a pair
import Control.Arrow (first, second)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import OOLang.AST
import OOLang.AST.Helpers
import OOLang.TypeChecker.TcError

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

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (ClassTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = mkTypeEnv Map.empty Map.empty

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: ClassTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv classTypeEnv funTypeEnv = TypeEnv (classTypeEnv, funTypeEnv)

-- Class type environment

type ClassTypeEnv = Map.Map ClassName ClassTypeInfo

-- | All the information about classes that we store in the type environment.
-- Some of the fields are kept just for error messages.
data ClassTypeInfo = ClassTypeInfo
  { ctiMSuperClassName    :: Maybe ClassName       -- ^ Name of the super class, if it has one.
  , ctiClassFields        :: Map.Map Var Type      -- ^ Class fields environment.
  , ctiClassMethods       :: Map.Map FunName Type  -- ^ Class methods environment.
  , ctiSrcClassName       :: SrcClassName          -- ^ Source name. For error messages.
  , ctiMSuperSrcClassName :: Maybe SrcClassName    -- ^ Super class source name. For error messages.
  }

-- | 'ClassTypeEnv' getter.
getClassTypeEnv :: TypeCheckM ClassTypeEnv
getClassTypeEnv = gets (fst . unTypeEnv . getTypeEnv)

-- | Get class type environment as an associative list.
getClassesAssoc :: TypeCheckM [(ClassName, ClassTypeInfo)]
getClassesAssoc = fmap Map.assocs getClassTypeEnv

-- | Modification function for 'ClassTypeEnv'.
modifyClassTypeEnv :: (ClassTypeEnv -> ClassTypeEnv) -> TypeCheckM ()
modifyClassTypeEnv f = do
  (classTypeEnv, funTypeEnv) <- (,) <$> getClassTypeEnv <*> getFunTypeEnv
  modifyTypeEnv (const $ mkTypeEnv (f classTypeEnv) funTypeEnv)

-- | Returns all information about the class from the environment.
--
-- Note: Unsafe. Should be used only after check that class is defined.
getClassTypeInfo :: ClassName -> TypeCheckM ClassTypeInfo
getClassTypeInfo className = do
  classTypeEnv <- getClassTypeEnv
  return $ fromJust $ Map.lookup className classTypeEnv  -- fromJust may fail

modifyClassTypeInfo :: ClassName -> (ClassTypeInfo -> ClassTypeInfo) -> TypeCheckM ()
modifyClassTypeInfo className f = do
  classTypeInfo <- getClassTypeInfo className
  -- overwrite with the modified ClassTypeInfo
  modifyClassTypeEnv (Map.insert className (f classTypeInfo))

isClassDefined :: ClassName -> TypeCheckM Bool
isClassDefined className = do
  classTypeEnv <- getClassTypeEnv
  return $ Map.member className classTypeEnv

isClassMemberDefined :: ClassName -> MemberName -> TypeCheckM Bool
isClassMemberDefined className memberName = do
  let fieldName = memberNameToVar memberName
      methodName = memberNameToFunName memberName
  isField <- isClassFieldDefined className fieldName
  isMethod <- isClassMethodDefined className methodName
  return (isField || isMethod)

-- | Looks for a field in the whole class hierarchy.
isClassFieldDefined :: ClassName -> Var -> TypeCheckM Bool
isClassFieldDefined className fieldName = do
  classTypeInfo <- getClassTypeInfo className
  if Map.member fieldName (ctiClassFields classTypeInfo)
    then return True
    else case ctiMSuperClassName classTypeInfo of
           Nothing -> return False
           Just superClassName -> isClassFieldDefined superClassName fieldName

-- | Looks for a method in the whole class hierarchy.
isClassMethodDefined :: ClassName -> FunName -> TypeCheckM Bool
isClassMethodDefined className methodName = do
  classTypeInfo <- getClassTypeInfo className
  if Map.member methodName (ctiClassMethods classTypeInfo)
    then return True
    else case ctiMSuperClassName classTypeInfo of
           Nothing -> return False
           Just superClassName -> isClassMethodDefined superClassName methodName

-- | Checks whether there is a method with a given name and type in on of the
-- super classes (does *not* check a given class).
isClassMethodOverride :: ClassName -> FunName -> Type -> TypeCheckM Bool
isClassMethodOverride className methodName methodType = do
  mSuperClassName <- getSuperClass className
  case mSuperClassName of
    Nothing -> return False
    Just superClassName -> hasClassMethodOfType superClassName methodName methodType

-- | Checks whether there is a method with a given name and type in on of the
-- classes in the hierarchy (starts from a given class).
hasClassMethodOfType :: ClassName -> FunName -> Type -> TypeCheckM Bool
hasClassMethodOfType className methodName methodType = do
  classTypeInfo <- getClassTypeInfo className
  case Map.lookup methodName (ctiClassMethods classTypeInfo) of
    Just methodType' -> return (methodType == methodType')
    Nothing -> do
      mSuperClassName <- getSuperClass className
      case mSuperClassName of
        Nothing -> return False
        Just superClassName -> hasClassMethodOfType superClassName methodName methodType

-- | Doesn't check if the class is already in the environment.
-- Will overwrite it in this case.
addClass :: SrcClassName -> Maybe SrcClassName -> TypeCheckM ()
addClass srcClassName mSuperSrcClassName = do
  let className = getClassName srcClassName
      mSuperClassName = getClassName <$> mSuperSrcClassName
  modifyClassTypeEnv $
    Map.insert className (ClassTypeInfo mSuperClassName
                                        Map.empty
                                        Map.empty
                                        srcClassName
                                        mSuperSrcClassName)

-- | Doesn't check if the field is already in the environment.
-- Will overwrite it in this case.
addClassField :: ClassName -> Var -> Type -> TypeCheckM ()
addClassField className fieldName fieldType = do
  modifyClassTypeInfo className (\classTypeInfo ->
    -- overwrite with the modified class fields mapping
    classTypeInfo { ctiClassFields = Map.insert fieldName fieldType (ctiClassFields classTypeInfo) })

-- | Doesn't check if the method is already in the environment.
-- Will overwrite it in this case.
addClassMethod :: ClassName -> FunName -> Type -> TypeCheckM ()
addClassMethod className methodName methodType = do
  modifyClassTypeInfo className (\classTypeInfo ->
    -- overwrite with the modified class methods mapping
    classTypeInfo { ctiClassMethods = Map.insert methodName methodType (ctiClassMethods classTypeInfo) })

-- | Returns a type of the class member.
--
-- Note: Unsafe. Should be used only after check that the class and the member
-- are defined.
getClassMemberType :: ClassName -> MemberName -> TypeCheckM Type
getClassMemberType className memberName = do
  classTypeInfo <- getClassTypeInfo className
  case Map.lookup (memberNameToVar memberName) (ctiClassFields classTypeInfo) of
    Just fieldType -> return fieldType
    Nothing ->
      case Map.lookup (memberNameToFunName memberName) (ctiClassMethods classTypeInfo) of
        Just methodType -> return methodType
        Nothing -> do
          mSuperClassName <- getSuperClass className
          getClassMemberType (fromJust mSuperClassName) memberName  -- fromJust may fail

-- | Returns a type of the class field.
--
-- Note: Unsafe. Should be used only after check that the class and the field
-- are defined.
getClassFieldType :: ClassName -> Var -> TypeCheckM Type
getClassFieldType className fieldName = getClassMemberType className (varToMemberName fieldName)

-- | Returns a super class of the given class if it has one.
--
-- Note: Unsafe. Should be used only after check that class is defined.
getSuperClass :: ClassName -> TypeCheckM (Maybe ClassName)
getSuperClass className = do
  classTypeEnv <- getClassTypeEnv
  return $ ctiMSuperClassName $ fromJust $ Map.lookup className classTypeEnv  -- fromJust may fail

-- Function type environment

type FunTypeEnv = Map.Map FunName FunTypeInfo

-- | All the information about functions that we store in the type environment.
-- Some of the fields are kept just for error messages.
data FunTypeInfo = FunTypeInfo
  { ftiType       :: Type        -- ^ Function type.
  , ftiSrcFunName :: SrcFunName  -- ^ Source name. For error messages.
  , ftiSrcFunType :: SrcFunType  -- ^ Source type. For error messages.
  }

-- | 'FunTypeEnv' getter.
getFunTypeEnv :: TypeCheckM FunTypeEnv
getFunTypeEnv = gets (snd . unTypeEnv . getTypeEnv)

-- | Modification function for 'FunTypeEnv'.
modifyFunTypeEnv :: (FunTypeEnv -> FunTypeEnv) -> TypeCheckM ()
modifyFunTypeEnv f = do
  (classTypeEnv, funTypeEnv) <- (,) <$> getClassTypeEnv <*> getFunTypeEnv
  modifyTypeEnv (const $ mkTypeEnv classTypeEnv (f funTypeEnv))

isFunctionDefined :: FunName -> TypeCheckM Bool
isFunctionDefined funName = do
  funTypeEnv <- getFunTypeEnv
  return $ Map.member funName funTypeEnv

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: SrcFunName -> Type ->SrcFunType -> TypeCheckM ()
addFunction srcFunName funType srcFunType = do
  let funName = getFunName srcFunName
  modifyFunTypeEnv $ Map.insert funName (FunTypeInfo funType srcFunName srcFunType)

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfo :: FunName -> TypeCheckM FunTypeInfo
getFunTypeInfo funName = do
  funTypeEnv <- getFunTypeEnv
  return $ fromJust $ Map.lookup funName funTypeEnv  -- fromJust may fail

-- Local type environment

-- | Local variables, parameters etc. and their types.
type LocalTypeEnv = Map.Map Var Type

emptyLocalTypeEnv :: LocalTypeEnv
emptyLocalTypeEnv = Map.empty

-- | Check whether a variable is in scope. It can be either local variable name
-- or a function name.
isVarBound :: Var -> TypeCheckM Bool
isVarBound var = do
  isLocalVar <- gets (Map.member var . getLocalTypeEnv)
  isFunction <- isFunctionDefined (varToFunName var)
  return (isLocalVar || isFunction)

-- | Pure function for querying local type environment.
isVarInLocalEnv :: Var -> LocalTypeEnv -> Bool
isVarInLocalEnv var = Map.member var

-- | Returns variable type. First looks for locals and then for functions.
--
-- Note: Unsafe. Should be used only after check that the variable is bound.
getVarType :: Var -> TypeCheckM Type
getVarType var = do
  mVarType <- gets (Map.lookup var . getLocalTypeEnv)
  case mVarType of
    Just varType -> return varType
    Nothing -> do
      funTypeInfo <- getFunTypeInfo (varToFunName var)
      return $ ftiType funTypeInfo

-- | Extends local type environment. Pure (meaning, not a 'TypeCheckM' function).
addLocalVar :: Var -> Type -> LocalTypeEnv -> LocalTypeEnv
addLocalVar = Map.insert

-- | Monadic function for adding variables to the local environment.
-- Doesn't check if the variable is already in the environment.
-- Will overwrite it in this case.
addLocalVarM :: Var -> Type -> TypeCheckM ()
addLocalVarM var varType =
  modifyLocalTypeEnv $ Map.insert var varType

-- | Takes a separate local type environment and merges it with what is already
-- in the local type environment and performs a given computation in this new
-- environment. Then restores the local environment.
-- Should be safe, since we don't allow shadowing.
locallyWithEnv :: LocalTypeEnv -> TypeCheckM a -> TypeCheckM a
locallyWithEnv localTypeEnv tcm = do
  currentLocalTypeEnv <- gets getLocalTypeEnv
  modifyLocalTypeEnv (Map.union localTypeEnv)
  a <- tcm
  modifyLocalTypeEnv (const currentLocalTypeEnv)
  return a

-- Utils

-- | Convenient version of 'runStateT' with the arguments flipped.
runStateTFrom :: Monad m => s -> StateT s m a -> m (a, s)
runStateTFrom = flip runStateT

