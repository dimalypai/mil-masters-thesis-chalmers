-- | Module defining different type environments and pure functions for working
-- with them.
module OOLang.TypeChecker.TypeEnv
  ( TypeEnv
  , initTypeEnv
  , mkTypeEnv

  , ClassTypeEnv
  , ClassTypeInfo
  , ctiMSuperClassName
  , ctiMSuperSrcClassName
  , getClassTypeEnv
  , isClassDefined
  , isClassFieldDefined
  , isClassMethodDefined
  , isClassMethodOverride
  , addClass
  , addClassField
  , addClassMethod
  , getClassMemberType
  , getClassFieldType
  , getClassMethodType
  , getSuperClass
  , getClassesAssoc
  , getClassFieldsAssoc
  , getClassMethodsAssoc

  , FunTypeEnv
  , FunTypeInfo
  , ftiType
  , ftiReturnType
  , ftiArity
  , ftiSrcFunType
  , getFunTypeEnv
  , isFunctionDefined
  , addFunction
  , getFunTypeInfo

  , LocalTypeEnv
  , LocalTypeContext
  , emptyLocalTypeEnv
  , emptyLocalTypeContext
  , isVarBound
  , isVarBoundInTypeContext
  , addLocalVar
  , getVarType
  , addEmptyLocalTypeContext
  , addLocalTypeContext
  , removeLocalTypeContext
  , modifyLocalTypeContext
  ) where

import qualified Data.Map as Map
-- 'second' is used just to transform components of a pair
import Control.Arrow (second)
import Data.Maybe (fromJust)

import OOLang.AST
import OOLang.AST.Helpers
import OOLang.BuiltIn

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (ClassTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = mkTypeEnv Map.empty initFunTypeEnv

initFunTypeEnv :: FunTypeEnv
initFunTypeEnv = Map.fromList $
  map (second (\(funType, retType, arity) -> builtInFunTypeInfo funType retType arity)) builtInFunctions

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: ClassTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv classTypeEnv funTypeEnv = TypeEnv (classTypeEnv, funTypeEnv)

-- * Class type environment

type ClassTypeEnv = Map.Map ClassName ClassTypeInfo

-- | All the information about classes that we store in the type environment.
-- Some of the fields are kept just for error messages.
data ClassTypeInfo = ClassTypeInfo
  { ctiMSuperClassName    :: Maybe ClassName       -- ^ Name of the super class, if it has one.
  , ctiClassFields        :: Map.Map Var Type      -- ^ Class fields environment.
  , ctiClassMethods       :: Map.Map FunName Type  -- ^ Class methods environment.
  , ctiMSuperSrcClassName :: Maybe SrcClassName    -- ^ Super class source name. For error messages.
  }

getClassTypeEnv :: TypeEnv -> ClassTypeEnv
getClassTypeEnv = fst . unTypeEnv

isClassDefined :: ClassName -> ClassTypeEnv -> Bool
isClassDefined = Map.member

-- | Looks for a field in the whole class hierarchy.
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassFieldDefined :: ClassName -> Var -> ClassTypeEnv -> Bool
isClassFieldDefined className fieldName classTypeEnv =
  let classTypeInfo = getClassTypeInfo className classTypeEnv in
  if Map.member fieldName (ctiClassFields classTypeInfo)
    then True
    else case ctiMSuperClassName classTypeInfo of
           Nothing -> False
           Just superClassName ->
             isClassFieldDefined superClassName fieldName classTypeEnv

-- | Looks for a method in the whole class hierarchy.
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassMethodDefined :: ClassName -> FunName -> ClassTypeEnv -> Bool
isClassMethodDefined className methodName classTypeEnv =
  let classTypeInfo = getClassTypeInfo className classTypeEnv in
  if Map.member methodName (ctiClassMethods classTypeInfo)
    then True
    else case ctiMSuperClassName classTypeInfo of
           Nothing -> False
           Just superClassName ->
             isClassMethodDefined superClassName methodName classTypeEnv

-- | Checks whether there is a method with a given name and type in on of the
-- super classes (does *not* check a given class).
-- Both argument and return types are *invariant* for overriding.
--
-- Note: Unsafe. Should be used only after check that class is defined.
isClassMethodOverride :: ClassName -> FunName -> Type -> ClassTypeEnv -> Bool
isClassMethodOverride className methodName methodType classTypeEnv =
  let mSuperClassName = getSuperClass className classTypeEnv in
  case mSuperClassName of
    Nothing -> False
    Just superClassName ->
      hasClassMethodOfType superClassName methodName methodType classTypeEnv

-- | Checks whether there is a method with a given name and type in on of the
-- classes in the hierarchy (starts from a given class).
--
-- Note: Unsafe. Should be used only after check that class is defined.
hasClassMethodOfType :: ClassName -> FunName -> Type -> ClassTypeEnv -> Bool
hasClassMethodOfType className methodName methodType classTypeEnv =
  let classTypeInfo = getClassTypeInfo className classTypeEnv in
  case Map.lookup methodName (ctiClassMethods classTypeInfo) of
    Just methodType' -> methodType == methodType'
    Nothing ->
      let mSuperClassName = getSuperClass className classTypeEnv in
      case mSuperClassName of
        Nothing -> False
        Just superClassName ->
          hasClassMethodOfType superClassName methodName methodType classTypeEnv

-- | Doesn't check if the class is already in the environment.
-- Will overwrite it in this case.
addClass :: ClassName -> Maybe ClassName -> Maybe SrcClassName -> ClassTypeEnv -> ClassTypeEnv
addClass className mSuperClassName mSuperSrcClassName =
  Map.insert className (ClassTypeInfo mSuperClassName Map.empty Map.empty mSuperSrcClassName)

-- | Doesn't check if the field is already in the environment.
-- Will overwrite it in this case.
--
-- Note: Unsafe. Should be used only after check that class is defined.
addClassField :: ClassName -> Var -> Type -> ClassTypeEnv -> ClassTypeEnv
addClassField className fieldName fieldType =
  modifyClassTypeInfo className (\classTypeInfo ->
    -- overwrite with the modified class fields mapping
    classTypeInfo { ctiClassFields = Map.insert fieldName fieldType (ctiClassFields classTypeInfo) })

-- | Doesn't check if the method is already in the environment.
-- Will overwrite it in this case.
--
-- Note: Unsafe. Should be used only after check that class is defined.
addClassMethod :: ClassName -> FunName -> Type -> ClassTypeEnv -> ClassTypeEnv
addClassMethod className methodName methodType =
  modifyClassTypeInfo className (\classTypeInfo ->
    -- overwrite with the modified class methods mapping
    classTypeInfo { ctiClassMethods = Map.insert methodName methodType (ctiClassMethods classTypeInfo) })

-- | Returns a type of the class member.
--
-- Note: Unsafe. Should be used only after check that the class and the member
-- are defined.
getClassMemberType :: ClassName -> MemberName -> ClassTypeEnv -> Type
getClassMemberType className memberName classTypeEnv =
  let classTypeInfo = getClassTypeInfo className classTypeEnv in
  case Map.lookup (memberNameToVar memberName) (ctiClassFields classTypeInfo) of
    Just fieldType -> fieldType
    Nothing ->
      case Map.lookup (memberNameToFunName memberName) (ctiClassMethods classTypeInfo) of
        Just methodType -> methodType
        Nothing ->
          let mSuperClassName = getSuperClass className classTypeEnv
          in getClassMemberType (fromJust mSuperClassName) memberName classTypeEnv  -- fromJust may fail

-- | Returns a type of the class field.
--
-- Note: Unsafe. Should be used only after check that the class and the field
-- are defined.
getClassFieldType :: ClassName -> Var -> ClassTypeEnv -> Type
getClassFieldType className fieldName =
  getClassMemberType className (varToMemberName fieldName)

-- | Returns a type of the class method.
--
-- Note: Unsafe. Should be used only after check that the class and the method
-- are defined.
getClassMethodType :: ClassName -> FunName -> ClassTypeEnv -> Type
getClassMethodType className methodName =
  getClassMemberType className (funNameToMemberName methodName)

-- | Returns a super class of the given class if it has one.
--
-- Note: Unsafe. Should be used only after check that class is defined.
getSuperClass :: ClassName -> ClassTypeEnv -> Maybe ClassName
getSuperClass className classTypeEnv =
  ctiMSuperClassName $ fromJust $ Map.lookup className classTypeEnv

-- | Get class type environment as an associative list.
getClassesAssoc :: ClassTypeEnv -> [(ClassName, ClassTypeInfo)]
getClassesAssoc = Map.assocs

-- | Returns an associative list of names and types of all class fields
-- accessible from a given class (including super class fields).
getClassFieldsAssoc :: ClassName -> ClassTypeEnv -> [(Var, Type)]
getClassFieldsAssoc className classTypeEnv =
  let classFieldsAssoc = Map.assocs $ ctiClassFields $ fromJust $ Map.lookup className classTypeEnv
  in case getSuperClass className classTypeEnv of
       Nothing -> classFieldsAssoc
       Just superClassName -> getClassFieldsAssoc superClassName classTypeEnv ++ classFieldsAssoc

-- | Returns an associative list of names and types of all class methods
-- accessible from a given class (including super class methods).
getClassMethodsAssoc :: ClassName -> ClassTypeEnv -> [(FunName, Type)]
getClassMethodsAssoc className classTypeEnv =
  let classMethodsAssoc = Map.assocs $ ctiClassMethods $ fromJust $ Map.lookup className classTypeEnv
  in case getSuperClass className classTypeEnv of
       Nothing -> classMethodsAssoc
       Just superClassName -> getClassMethodsAssoc superClassName classTypeEnv ++ classMethodsAssoc

-- | Returns all information about the class from the environment.
--
-- Note: Unsafe. Should be used only after check that class is defined.
getClassTypeInfo :: ClassName -> ClassTypeEnv -> ClassTypeInfo
getClassTypeInfo className classTypeEnv = fromJust $ Map.lookup className classTypeEnv

-- | Note: Unsafe. Should be used only after check that class is defined.
modifyClassTypeInfo :: ClassName -> (ClassTypeInfo -> ClassTypeInfo) -> (ClassTypeEnv -> ClassTypeEnv)
modifyClassTypeInfo className f classTypeEnv =
  let classTypeInfo = getClassTypeInfo className classTypeEnv
  in Map.insert className (f classTypeInfo) classTypeEnv

-- * Function type environment

type FunTypeEnv = Map.Map FunName FunTypeInfo

-- | All the information about functions that we store in the type environment.
-- Some of the fields are kept just for error messages.
data FunTypeInfo = FunTypeInfo
  { ftiType       :: Type        -- ^ Function type.
  , ftiReturnType :: ReturnType  -- ^ Function return type.
  , ftiArity      :: Int         -- ^ Function arity (number of parameter binders).
  , ftiSrcFunType :: SrcFunType  -- ^ Source type. For error messages.
  }

getFunTypeEnv :: TypeEnv -> FunTypeEnv
getFunTypeEnv = snd . unTypeEnv

isFunctionDefined :: FunName -> FunTypeEnv -> Bool
isFunctionDefined = Map.member

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: FunName -> Type -> ReturnType -> Int -> SrcFunType -> FunTypeEnv -> FunTypeEnv
addFunction funName funType retType arity srcFunType =
  Map.insert funName (FunTypeInfo funType retType arity srcFunType)

-- | Returns all information about the function from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfo :: FunName -> FunTypeEnv -> FunTypeInfo
getFunTypeInfo funName funTypeEnv = fromJust $ Map.lookup funName funTypeEnv

builtInFunTypeInfo :: Type -> ReturnType -> Int -> FunTypeInfo
builtInFunTypeInfo funType retType arity = FunTypeInfo funType retType arity undefined

-- * Local type environment

-- | Local type environment is a stack of type contexts (for handling scopes
-- introduced by blocks like try, when, while etc.).
-- There should always be at least one local type context.
type LocalTypeEnv = [LocalTypeContext]

-- | Local variables, parameters etc. and their types.
type LocalTypeContext = Map.Map Var Type

emptyLocalTypeEnv :: LocalTypeEnv
emptyLocalTypeEnv = [emptyLocalTypeContext]

emptyLocalTypeContext :: LocalTypeContext
emptyLocalTypeContext = Map.empty

-- | Looks through all type contexts in the environment from top to bottom.
isVarBound :: Var -> LocalTypeEnv -> Bool
isVarBound var = or . map (isVarBoundInTypeContext var)

isVarBoundInTypeContext :: Var -> LocalTypeContext -> Bool
isVarBoundInTypeContext = Map.member

addLocalVar :: Var -> Type -> LocalTypeContext -> LocalTypeContext
addLocalVar = Map.insert

-- | Looks through all type contexts in the environment from top to bottom.
getVarType :: Var -> LocalTypeEnv -> Maybe Type
getVarType _ [] = Nothing
getVarType var (ctx:ctxs) =
  case Map.lookup var ctx of
    Just varType -> Just varType
    Nothing -> getVarType var ctxs

-- | Adds an empty local type context on top.
addEmptyLocalTypeContext :: LocalTypeEnv -> LocalTypeEnv
addEmptyLocalTypeContext = (emptyLocalTypeContext :)

-- | Adds a given local type context on top.
addLocalTypeContext :: LocalTypeContext -> LocalTypeEnv -> LocalTypeEnv
addLocalTypeContext = (:)

-- | Removes the top local type context.
removeLocalTypeContext :: LocalTypeEnv -> LocalTypeEnv
removeLocalTypeContext = tail

-- | Modifies the top local type context.
modifyLocalTypeContext :: (LocalTypeContext -> LocalTypeContext)
                       -> LocalTypeEnv -> LocalTypeEnv
modifyLocalTypeContext f (ctx:ctxs) = f ctx : ctxs

