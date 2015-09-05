-- | Module defining different type environments and pure functions for working
-- with them.
module MIL.TypeChecker.TypeEnv
  ( TypeEnv
  , mkTypeEnv
  , initTypeEnv

  , DataTypeEnv
  , DataTypeInfo
  , dtiKind
  , getDataTypeEnv
  , getDataTypeInfo
  , isTypeDefined
  , addType

  , DataConTypeEnv
  , DataConTypeInfo
  , dcontiType
  , dcontiTypeName
  , getDataConTypeEnv
  , getDataConTypeInfo
  , isDataConDefined
  , addDataCon

  , FunTypeEnv
  , getFunTypeEnv
  , isFunctionDefined
  , addFunction
  , getFunType

  , LocalTypeEnv
  , emptyLocalTypeEnv
  , getLocalVars
  , getLocalTypeVars
  , isVarBound
  , isTypeVarBound
  , isVarInLocalEnv
  , isTypeVarInLocalEnv
  , getVarType
  , addLocalVar
  , addLocalTypeVar
  , locallyWithEnv
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
-- 'first' and 'second' are used just to transform components of a pair
import Control.Arrow (first, second)

import MIL.AST
import MIL.BuiltIn

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (DataTypeEnv, DataConTypeEnv, FunTypeEnv) }
  deriving (Show, Eq)

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: DataTypeEnv -> DataConTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv dataTypeEnv dataConTypeEnv funTypeEnv =
  TypeEnv (dataTypeEnv, dataConTypeEnv, funTypeEnv)

-- | Initial type environment.
initTypeEnv :: [Type -> MonadType] -> TypeEnv
initTypeEnv monadTypeCons =
  mkTypeEnv (Map.fromList $ map (second builtInDataTypeInfo) builtInDataTypes)
            (Map.fromList $ map (second $ uncurry DataConTypeInfo) builtInDataCons)
            (Map.fromList $ builtInFunctions monadTypeCons)

-- * Data type environment

type DataTypeEnv = Map.Map TypeName DataTypeInfo

data DataTypeInfo = DataTypeInfo
  { dtiKind        :: Kind
  }
  deriving (Show, Eq)

getDataTypeEnv :: TypeEnv -> DataTypeEnv
getDataTypeEnv typeEnv =
  let (dataTypeEnv, _, _) = unTypeEnv typeEnv
  in dataTypeEnv

-- | Returns all information about the data type from the environment.
--
-- Note: Unsafe. Should be used only after check that data type is defined.
getDataTypeInfo :: TypeName -> DataTypeEnv -> DataTypeInfo
getDataTypeInfo typeName dataTypeEnv =
  fromJust $ Map.lookup typeName dataTypeEnv  -- fromJust may fail

isTypeDefined :: TypeName -> DataTypeEnv -> Bool
isTypeDefined = Map.member

-- | Doesn't check if the type is already in the environment.
-- Will overwrite it in this case.
--
-- Note: data constructors are not available at this point.
addType :: TypeName -> Kind -> DataTypeEnv -> DataTypeEnv
addType typeName kind = Map.insert typeName (DataTypeInfo kind)

builtInDataTypeInfo :: Kind -> DataTypeInfo
builtInDataTypeInfo kind = DataTypeInfo kind

-- * Data constructor type environment

type DataConTypeEnv = Map.Map ConName DataConTypeInfo

data DataConTypeInfo = DataConTypeInfo
  { dcontiType     :: Type      -- ^ Function type of the data constructor.
  , dcontiTypeName :: TypeName  -- ^ Type name of the data type constructor is defined in.
  }
  deriving (Show, Eq)

getDataConTypeEnv :: TypeEnv -> DataConTypeEnv
getDataConTypeEnv typeEnv =
  let (_, dataConTypeEnv, _) = unTypeEnv typeEnv
  in dataConTypeEnv

-- | Returns all information about the data constructor from the environment.
--
-- Note: Unsafe. Should be used only after check that data constructor is defined.
getDataConTypeInfo :: ConName -> DataConTypeEnv -> DataConTypeInfo
getDataConTypeInfo conName dataConTypeEnv =
  fromJust $ Map.lookup conName dataConTypeEnv  -- fromJust may fail

isDataConDefined :: ConName -> DataConTypeEnv -> Bool
isDataConDefined = Map.member

-- | Doesn't check if the constructor is already in the environment.
-- Will overwrite it in this case.
addDataCon :: ConName -> Type -> TypeName -> DataConTypeEnv -> DataConTypeEnv
addDataCon conName conType typeName =
  Map.insert conName (DataConTypeInfo conType typeName)

-- * Function type environment

type FunTypeEnv = Map.Map FunName Type

getFunTypeEnv :: TypeEnv -> FunTypeEnv
getFunTypeEnv typeEnv =
  let (_, _, funTypeEnv) = unTypeEnv typeEnv
  in funTypeEnv

isFunctionDefined :: FunName -> FunTypeEnv -> Bool
isFunctionDefined = Map.member

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: FunName -> Type -> FunTypeEnv -> FunTypeEnv
addFunction = Map.insert

-- | Returns the function type from the environment.
--
-- Note: Unsafe. Should be used only after check that function is defined.
getFunType :: FunName -> FunTypeEnv -> Type
getFunType funName funTypeEnv =
  fromJust $ Map.lookup funName funTypeEnv  -- fromJust may fail

-- * Local type environment

-- | Local type environment (inside functions, lambdas etc.).
-- Consists of variables with their types and a set of type variables (from
-- type lambdas) in scope.
type LocalTypeEnv = (Map.Map Var Type, Set.Set TypeVar)

type LocalVars = Map.Map Var Type

type LocalTypeVars = Set.Set TypeVar

-- | Empty local type environment.
emptyLocalTypeEnv :: LocalTypeEnv
emptyLocalTypeEnv = (Map.empty, Set.empty)

getLocalVars :: LocalTypeEnv -> LocalVars
getLocalVars = fst

getLocalTypeVars :: LocalTypeEnv -> LocalTypeVars
getLocalTypeVars = snd

modifyLocalVars :: (LocalVars -> LocalVars) -> (LocalTypeEnv -> LocalTypeEnv)
modifyLocalVars = first

modifyLocalTypeVars :: (LocalTypeVars -> LocalTypeVars) -> (LocalTypeEnv -> LocalTypeEnv)
modifyLocalTypeVars = second

-- | Check whether a variable is in scope. It can be either local variable name
-- or a function name.
isVarBound :: Var -> LocalTypeEnv -> Bool
isVarBound var = Map.member var . getLocalVars

-- | Check whether a type variable is in scope.
isTypeVarBound :: TypeVar -> LocalTypeEnv -> Bool
isTypeVarBound typeVar = Set.member typeVar . getLocalTypeVars

-- | Function for querying variables in local type environment.
isVarInLocalEnv :: Var -> LocalTypeEnv -> Bool
isVarInLocalEnv var = Map.member var . getLocalVars

-- | Function for querying type variables in local type environment.
isTypeVarInLocalEnv :: TypeVar -> LocalTypeEnv -> Bool
isTypeVarInLocalEnv typeVar = Set.member typeVar . getLocalTypeVars

getVarType :: Var -> LocalTypeEnv -> Maybe Type
getVarType var = Map.lookup var . getLocalVars

-- | Extends local type environment (variables part).
addLocalVar :: Var -> Type -> LocalTypeEnv -> LocalTypeEnv
addLocalVar var varType = modifyLocalVars (Map.insert var varType)

-- | Extends local type environment (type variables part).
addLocalTypeVar :: TypeVar -> LocalTypeEnv -> LocalTypeEnv
addLocalTypeVar typeVar = modifyLocalTypeVars (Set.insert typeVar)

-- | Takes a separate local type environment and produces a transformation
-- function, which merges this environment with a local type environment given
-- as an argument.
-- Should be safe, since we don't allow shadowing.
locallyWithEnv :: LocalTypeEnv -> (LocalTypeEnv -> LocalTypeEnv)
locallyWithEnv localTypeEnv =
  (\(vars, typeVars) -> ( Map.union (getLocalVars localTypeEnv) vars
                        , Set.union (getLocalTypeVars localTypeEnv) typeVars))

