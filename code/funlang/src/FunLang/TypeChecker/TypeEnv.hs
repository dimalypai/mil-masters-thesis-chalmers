-- | Module defining different type environments and pure functions for working
-- with them.
module FunLang.TypeChecker.TypeEnv
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
  , FunTypeInfo
  , ftiType
  , ftiSrcType
  , getFunTypeEnv
  , getFunTypeInfo
  , isFunctionDefined
  , addFunction

  , LocalTypeEnv
  , emptyLocalTypeEnv
  , getLocalVars
  , getLocalTypeVars
  , isVarBound
  , isTypeVarBound
  , addLocalVar
  , addLocalTypeVar
  , getVarType
  , locallyWithEnv
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
-- 'first' and 'second' are used just to transform components of a pair
import Control.Arrow (first, second)

import FunLang.AST
import FunLang.BuiltIn

-- | Type environment.
newtype TypeEnv = TypeEnv { unTypeEnv :: (DataTypeEnv, DataConTypeEnv, FunTypeEnv) }

-- | Initial type environment.
initTypeEnv :: TypeEnv
initTypeEnv = mkTypeEnv (Map.fromList $ map (second builtInDataTypeInfo) builtInDataTypes)
                        (Map.fromList $ map (second $ uncurry DataConTypeInfo) builtInDataCons)
                        (Map.fromList $ map (second builtInFunTypeInfo) builtInFunctions)

-- | Smart constructor for 'TypeEnv'.
mkTypeEnv :: DataTypeEnv -> DataConTypeEnv -> FunTypeEnv -> TypeEnv
mkTypeEnv dataTypeEnv dataConTypeEnv funTypeEnv =
  TypeEnv (dataTypeEnv, dataConTypeEnv, funTypeEnv)

-- * Data type environment

type DataTypeEnv = Map.Map TypeName DataTypeInfo

data DataTypeInfo = DataTypeInfo
  { dtiKind        :: Kind
  , dtiCons        :: [ConName]
  }

getDataTypeEnv :: TypeEnv -> DataTypeEnv
getDataTypeEnv typeEnv =
  let (dataTypeEnv, _, _) = unTypeEnv typeEnv
  in dataTypeEnv

-- | Returns all information about the data type from the environment.
--
-- Note: Unsafe. Should be used only after check that data type is defined.
getDataTypeInfo :: TypeName -> DataTypeEnv -> DataTypeInfo
getDataTypeInfo typeName dataTypeEnv = fromJust $ Map.lookup typeName dataTypeEnv

isTypeDefined :: TypeName -> DataTypeEnv -> Bool
isTypeDefined = Map.member

-- | Doesn't check if the type is already in the environment.
-- Will overwrite it in this case.
--
-- Note: data constructors are not available at this point.
addType :: TypeName -> Kind -> DataTypeEnv -> DataTypeEnv
addType typeName kind = Map.insert typeName (DataTypeInfo kind dataConsStub)

builtInDataTypeInfo :: Kind -> DataTypeInfo
builtInDataTypeInfo kind = DataTypeInfo kind undefined

-- | A placeholder for data constructors in the DataTypeEnv.  There are moments
-- during the type checking when this information is not available yet.
dataConsStub :: [ConName]
dataConsStub = error "Data constructors are not available yet"

-- * Data constructor type environment

type DataConTypeEnv = Map.Map ConName DataConTypeInfo

data DataConTypeInfo = DataConTypeInfo
  { dcontiType     :: Type      -- ^ Function type of the data constructor.
  , dcontiTypeName :: TypeName  -- ^ Type name of the data type constructor is defined in.
  }

getDataConTypeEnv :: TypeEnv -> DataConTypeEnv
getDataConTypeEnv typeEnv =
  let (_, dataConTypeEnv, _) = unTypeEnv typeEnv
  in dataConTypeEnv

-- | Returns all information about the data constructor from the environment.
--
-- Note: Unsafe. Should be used only after check that data constructor is defined.
getDataConTypeInfo :: ConName -> DataConTypeEnv -> DataConTypeInfo
getDataConTypeInfo conName dataConTypeEnv = fromJust $ Map.lookup conName dataConTypeEnv

isDataConDefined :: ConName -> DataConTypeEnv -> Bool
isDataConDefined = Map.member

-- | Doesn't check if the constructor is already in the environment.
-- Will overwrite it in this case.
addDataCon :: ConName -> Type -> TypeName -> DataConTypeEnv -> DataConTypeEnv
addDataCon conName conType typeName = Map.insert conName (DataConTypeInfo conType typeName)

-- * Function type environment

type FunTypeEnv = Map.Map FunName FunTypeInfo

data FunTypeInfo = FunTypeInfo
  {
    -- | Internal representation of the function type.
    ftiType       :: Type
    -- | Source representation of function type. Gets transformed into the
    -- internal representation as the type checking procedes. May be useful for
    -- error messages.
  , ftiSrcType    :: SrcType
  }

getFunTypeEnv :: TypeEnv -> FunTypeEnv
getFunTypeEnv typeEnv =
  let (_, _, funTypeEnv) = unTypeEnv typeEnv
  in funTypeEnv

--- | Returns all information about the function from the environment.
--
--- Note: Unsafe. Should be used only after check that function is defined.
getFunTypeInfo :: FunName -> FunTypeEnv -> FunTypeInfo
getFunTypeInfo funName funTypeEnv = fromJust $ Map.lookup funName funTypeEnv

isFunctionDefined :: FunName -> FunTypeEnv -> Bool
isFunctionDefined = Map.member

-- | Doesn't check if the function is already in the environment.
-- Will overwrite it in this case.
addFunction :: FunName -> Type -> SrcType -> FunTypeEnv -> FunTypeEnv
addFunction funName funType funSrcType = Map.insert funName (FunTypeInfo funType funSrcType)

builtInFunTypeInfo :: Type -> FunTypeInfo
builtInFunTypeInfo funType = FunTypeInfo funType undefined

-- * Local type environment

-- | Local type environment (inside functions, lambdas etc.).
-- Consists of variables with their types and a set of type variables (from
-- type lambdas) in scope.
type LocalTypeEnv = (Map.Map Var Type, Set.Set TypeVar)

type LocalVars = Map.Map Var Type

type LocalTypeVars = Set.Set TypeVar

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

isVarBound :: Var -> LocalTypeEnv -> Bool
isVarBound var = Map.member var . getLocalVars

isTypeVarBound :: TypeVar -> LocalTypeEnv -> Bool
isTypeVarBound typeVar = Set.member typeVar . getLocalTypeVars

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
-- as an argument. Should be safe, since we don't allow shadowing.
locallyWithEnv :: LocalTypeEnv -> (LocalTypeEnv -> LocalTypeEnv)
locallyWithEnv localTypeEnv =
  \(vars, typeVars) -> ( Map.union (getLocalVars localTypeEnv) vars
                       , Set.union (getLocalTypeVars localTypeEnv) typeVars)

