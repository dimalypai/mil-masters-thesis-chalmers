-- | Functions for working with types. Used in the type/lint checker.
module MIL.TypeChecker.Helpers where

import qualified Data.Set as Set
import Data.List (foldl')
import Control.Applicative

import MIL.AST
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TcError
import MIL.BuiltIn
import MIL.Utils

-- | Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- For details see 'checkTypeWithTypeVars'.
checkType :: Type -> TypeCheckM ()
checkType = checkTypeWithTypeVars Set.empty

-- | Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
--
-- For details see 'checkTypeWithTypeVarsOfKind'.
checkTypeWithTypeVars :: Set.Set TypeVar -> Type -> TypeCheckM ()
checkTypeWithTypeVars typeVars = checkTypeWithTypeVarsOfKind typeVars StarK

-- | Checks if the type is well-formed, uses types in scope, has a given kind and
-- that all nested types are well-kinded.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
--
-- For details see inline comments.
checkTypeWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> Type -> TypeCheckM ()
checkTypeWithTypeVarsOfKind typeVars kind t =
  case t of
    TyTypeCon typeName ->
      ifM (isTypeDefinedM typeName)
        (do dataTypeKind <- getDataTypeKindM typeName
            when (dataTypeKind /= kind) $
              throwError $ TypeConIncorrectApp typeName dataTypeKind kind)
        (throwError $ TypeNotDefined typeName)

    TyVar typeVar -> do
      isTyVarBound <- isTypeVarBoundM typeVar
      -- It is important to check in both places, since typeVars is not queried
      -- by 'isTypeVarBoundM'.
      unless (isTyVarBound || typeVar `Set.member` typeVars) $
        throwError $ TypeVarNotInScope typeVar

    TyArrow t1 t2 -> do
      unless (kind == StarK) $
        throwError $ TypeIncorrectKind t StarK kind
      checkTypeWithTypeVars typeVars t1
      checkTypeWithTypeVars typeVars t2

    TyForAll typeVar bodyT -> do
      -- It is important to check in all these places, since it can shadow a
      -- type or another type variable and typeVars is not queried by
      -- 'isTypeDefinedM' and 'isTypeVarBoundM'.
      whenM (isTypeDefinedM $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      isTyVarBound <- isTypeVarBoundM typeVar
      when (isTyVarBound || typeVar `Set.member` typeVars) $
        throwError $ TypeVarShadowsTypeVar typeVar
      -- forall type extends a set of type variables which are in scope
      let typeVars' = Set.insert typeVar typeVars
      checkTypeWithTypeVarsOfKind typeVars' kind bodyT

    TyApp t1 t2 ->
      case t1 of
        TyTypeCon {} -> do
          -- Type constructor is on the left-hand side of the application, it
          -- should have extra * in the kind (on the left) in order for the
          -- type to be well-kinded.
          checkTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) t1

          -- Start from kind *, it is another type constructor (another
          -- application).
          checkTypeWithTypeVars typeVars t2

        TyApp {} -> do
          -- One more application on the left, add * to the kind.
          checkTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) t1

          -- Start from kind *, it is another type constructor (another
          -- application).
          checkTypeWithTypeVars typeVars t2

        TyMonad mt ->
          -- Monad is on the left-hand side of the application, it
          -- should have extra * in the kind (on the left) in order for
          -- the type to be well-kinded
          checkMonadTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) mt

        -- Type variables are always of kind *, so they cannot be applied
        TyVar tv -> throwError $ TypeVarApp tv

        -- Nothing else can be applied
        _ -> throwError $ IllFormedType t

    TyTuple elemTypes -> do
      unless (kind == StarK) $
        throwError $ TypeIncorrectKind t StarK kind
      mapM_ (checkTypeWithTypeVars typeVars) elemTypes

    TyMonad mt -> checkMonadTypeWithTypeVarsOfKind typeVars kind mt

-- | Checks if the monadic type is well-formed, uses types in scope, has a
-- given kind and that all nested types are well-kinded.
--
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
checkMonadTypeWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> MonadType -> TypeCheckM ()
checkMonadTypeWithTypeVarsOfKind typeVars kind mt = do
  -- Monadic types have kind * -> *
  unless (kind == mkKind 1) $
    throwError $ TypeIncorrectKind (TyMonad mt) (mkKind 1) kind
  case mt of
    MTyMonad sm -> checkSingleMonadWithTypeVarsOfKind typeVars (mkKind 1) sm
    MTyMonadCons sm mt' -> do
      checkSingleMonadWithTypeVarsOfKind typeVars (mkKind 1) sm
      checkMonadTypeWithTypeVarsOfKind typeVars (mkKind 1) mt'

-- | Checks if a single monad is well-formed, uses types in scope, has a given
-- kind and that all nested types are well-kinded.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
--
-- For details see inline comments.
checkSingleMonadWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> SingleMonad -> TypeCheckM ()
checkSingleMonadWithTypeVarsOfKind typeVars kind sm =
  case sm of
    SinMonad m ->
      let monadTypeName = builtInMonadTypeName m
          monadKind = getBuiltInMonadKind monadTypeName in
      unless (monadKind == kind) $
        throwError $ TypeConIncorrectApp monadTypeName monadKind kind
    SinMonadApp sm' t -> do
      -- One more application on the left, add * to the kind.
      checkSingleMonadWithTypeVarsOfKind typeVars (StarK :=>: kind) sm'

      -- Start from kind *, it is another type constructor (another
      -- application)
      checkTypeWithTypeVars typeVars t

-- * Type construction

-- | Constructs an arrow type given a result type and a list of parameter
-- types.
tyArrowFromList :: Type -> [Type] -> Type
tyArrowFromList resultType = foldr (\t acc -> TyArrow t acc) resultType

-- | Constructs a type application given a type name and a list of type variables.
tyAppFromList :: TypeName -> [TypeVar] -> Type
tyAppFromList typeName = foldl' (\acc tv -> TyApp acc (TyVar tv)) (TyTypeCon typeName)

-- | Constructs a forall type given a body type and a list of type variables.
tyForAllFromList :: Type -> [TypeVar] -> Type
tyForAllFromList bodyType = foldr (\tv acc -> TyForAll tv acc) bodyType

-- | Takes a scrutinee type and a type of the data constructor (for error
-- message) and transforms a series of type applications and eventual type
-- constructor to a pair of type constructor name and a list of its type
-- arguments. If it encounters other types, it throws an error. Used when
-- checking data constructor pattern.
transformScrutType :: Type -> Type -> TypeCheckM (TypeName, [Type])
transformScrutType scrutType conType = transformScrutType' scrutType []
  where transformScrutType' :: Type -> [Type] -> TypeCheckM (TypeName, [Type])
        transformScrutType' scrutT typeArgs =
          case scrutT of
            TyTypeCon typeName -> return (typeName, typeArgs)
            TyApp t1 t2 -> transformScrutType' t1 (t2:typeArgs)
            -- If the scrutinee has a type other than a type application, then this
            -- pattern can not be type correct. Data constructors have a
            -- monomorphic fully applied type constructor type.
            _ -> throwError $ PatternIncorrectType scrutType conType

-- | Given a data constructor function type, recovers its field types.
-- Takes a list of type arguments to the type constructor (which is fully
-- applied). It is used for substitution when coming across forall types.
conFieldTypesFromType :: Type -> [Type] -> [Type]
conFieldTypesFromType t typeArgs = init $ conFieldTypesFromType' t typeArgs
  where conFieldTypesFromType' (TyForAll typeVar forallBodyType) (tyArg:tyArgs) =
          conFieldTypesFromType' ((typeVar, tyArg) `substTypeIn` forallBodyType) tyArgs
        conFieldTypesFromType' (TyArrow t1 t2)  [] =
          conFieldTypesFromType' t1 [] ++ conFieldTypesFromType' t2 []
        conFieldTypesFromType' tyCon@(TyTypeCon {}) [] = [tyCon]
        conFieldTypesFromType' tyApp@(TyApp {}) [] = [tyApp]
        conFieldTypesFromType' tyVar@(TyVar {}) [] = [tyVar]
        conFieldTypesFromType'          _ _ = error "conFieldTypesFromType: kind checking must have gone wrong"

-- * Alpha equivalence of types.
-- TODO: Pure alpha equivalence.

-- | Decides if two types are equivalent up to a change of type variables bound
-- by forall. It is monadic to be able to expand type aliases.
--
-- For example, forall A . A -> A and forall B . B -> B are alpha equivalent.
class AlphaEq t where
  alphaEq :: t -> t -> TypeCheckM Bool

-- | Most of the cases are straightforward. When we get two forall types, we
-- replace one of the type variables (its free occurences) with another one and
-- check whether the resulting types are alpha equivalent.
-- Use 'TypeM' instance for 'TyMonad'.
instance AlphaEq Type where
  alphaEq (TyTypeCon typeName1) (TyTypeCon typeName2) = return (typeName1 == typeName2)
  alphaEq (TyVar typeVar1) (TyVar typeVar2) = return (typeVar1 == typeVar2)
  alphaEq (TyArrow t11 t12) (TyArrow t21 t22) =
    (&&) <$> (t11 `alphaEq` t21) <*> (t12 `alphaEq` t22)
  alphaEq (TyApp t11 t12) (TyApp t21 t22) =
    (&&) <$> (t11 `alphaEq` t21) <*> (t12 `alphaEq` t22)
  alphaEq (TyForAll tv1 t1) (TyForAll tv2 t2) = t1 `alphaEq` ((tv2, TyVar tv1) `substTypeIn` t2)
  -- TODO: width subtyping
  alphaEq (TyTuple elemTypes1) (TyTuple elemTypes2) = do
    let lengthEq = length elemTypes1 == length elemTypes2
    elemsAlphaEq <- mapM (uncurry alphaEq) (zip elemTypes1 elemTypes2)
    return (lengthEq && and elemsAlphaEq)
  alphaEq (TyMonad tm1) (TyMonad tm2) = tm1 `alphaEq` tm2
  alphaEq _ _ = return False

instance AlphaEq MonadType where
  alphaEq (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
  alphaEq (MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) =
    (&&) <$> (m1 `alphaEq` m2) <*> (tm1 `alphaEq` tm2)
  alphaEq _ _ = return False

instance AlphaEq SingleMonad where
  alphaEq (SinMonad m1) (SinMonad m2) = m1 `alphaEq` m2
  alphaEq (SinMonadApp m1 t1) (SinMonadApp m2 t2) =
    (&&) <$> m1 `alphaEq` m2 <*> (t1 `alphaEq` t2)
  alphaEq _ _ = return False

instance AlphaEq MilMonad where
  alphaEq m1 m2 = return (m1 == m2)

-- * Type substitution.

-- | Replace free occurences of the type variable (first component of the first
-- parameter) with the type (second component of the first parameter) in the
-- type given as a second parameter.
class SubstType t where
  substTypeIn :: (TypeVar, Type) -> t -> t

-- | The case that actually does the job is 'TyVar'. Another interesting case is
-- 'TyForAll'. We don't allow shadowing and all type variables in the type are
-- bound (types are closed), but it is possible to introduce shadowing by
-- substitution:
--
-- @
--   (forall A . forall B . A -> B)[A := forall B . B] =>
--   => forall B . (forall B . B) -> B
-- @
--
-- It is not clear that such type can arise from the program (if it is possible
-- to construct such a term), but we still need to handle this case, hence the
-- otherwise guard clause.
--
-- Use 'TypeM' instance for 'TyMonad'.
instance SubstType Type where
  _ `substTypeIn` t@(TyTypeCon {}) = t
  (typeVar, argType) `substTypeIn` t@(TyVar typeVar')
    | typeVar == typeVar' = argType
    | otherwise           = t
  tvArg `substTypeIn` (TyArrow t1 t2) =
    TyArrow (tvArg `substTypeIn` t1) (tvArg `substTypeIn` t2)
  tvArg `substTypeIn` (TyApp t1 t2) =
    TyApp (tvArg `substTypeIn` t1) (tvArg `substTypeIn` t2)
  tvArg@(typeVar, _) `substTypeIn` forallT@(TyForAll tv t)
    | typeVar /= tv = TyForAll tv (tvArg `substTypeIn` t)
    | otherwise     = forallT
  tvArg `substTypeIn` (TyTuple elemTypes) =
    TyTuple (map (tvArg `substTypeIn`) elemTypes)
  tvArg `substTypeIn` (TyMonad tm) = TyMonad (tvArg `substTypeIn` tm)

instance SubstType MonadType where
  tvArg `substTypeIn` (MTyMonad m) = MTyMonad (tvArg `substTypeIn` m)
  tvArg `substTypeIn` (MTyMonadCons m tm) =
    MTyMonadCons (tvArg `substTypeIn` m) (tvArg `substTypeIn` tm)

instance SubstType SingleMonad where
  _ `substTypeIn` (SinMonad m) = SinMonad m
  tvArg `substTypeIn` (SinMonadApp m t) = SinMonadApp m (tvArg `substTypeIn` t)

instance SubstType MilMonad where
  _ `substTypeIn` m = m

-- * 'TypeM' helpers

-- | Checks if two monads are compatible. This means if one of them is alpha
-- equivalent to another or one of them is a prefix of another.
-- For example, m1 is a prefix of m1 ::: m2.
--
-- This operation is commutative.
compatibleMonadTypes :: MonadType -> MonadType -> TypeCheckM Bool
compatibleMonadTypes (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
compatibleMonadTypes (MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) =
  (&&) <$> (m1 `alphaEq` m2) <*> compatibleMonadTypes tm1 tm2
compatibleMonadTypes (MTyMonad m1) (MTyMonadCons m2 _) = m1 `alphaEq` m2
compatibleMonadTypes tm1 tm2 = compatibleMonadTypes tm2 tm1

-- | Compares two monads in terms of their effects.
-- Main idea: monad cons cell has more effects than a single monad.
--
-- Assumption: 'compatibleMonadTypes' returned True.
--
-- This operation is *not* commutative.
hasMoreEffectsThan :: MonadType -> MonadType -> TypeCheckM Bool
hasMoreEffectsThan (MTyMonadCons _ tm1) (MTyMonadCons _ tm2) = tm1 `hasMoreEffectsThan` tm2
hasMoreEffectsThan (MTyMonadCons {}) (MTyMonad {}) = return True
hasMoreEffectsThan (MTyMonad {}) (MTyMonad {}) = return False
hasMoreEffectsThan t1@(MTyMonad {}) t2@(MTyMonadCons {}) = not <$> t2 `hasMoreEffectsThan` t1

-- | Checks if the first monad is a suffix of the second one or if they are
-- alpha equivalent.
-- For example, m2 is a suffix of m1 ::: m2.
--
-- This operation is *not* commutative.
isMonadSuffixOf :: MonadType -> MonadType -> TypeCheckM Bool
isMonadSuffixOf (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
isMonadSuffixOf t1@(MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) = do
  isSuffix <- (&&) <$> (m1 `alphaEq` m2) <*> tm1 `isMonadSuffixOf` tm2
  isShiftedSuffix <- t1 `isMonadSuffixOf` tm2
  return (isSuffix || isShiftedSuffix)
isMonadSuffixOf t1@(MTyMonad {}) (MTyMonadCons _ tm2) = t1 `isMonadSuffixOf` tm2
isMonadSuffixOf (MTyMonadCons {}) (MTyMonad {}) = return False

-- TODO
containsMonad :: Type -> MonadType -> TypeCheckM Bool
containsMonad = undefined

