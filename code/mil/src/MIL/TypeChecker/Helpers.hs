-- | Functions for working with types. Used in the type checker.
module MIL.TypeChecker.Helpers where

import qualified Data.Set as Set
import Data.List (foldl')
import Control.Applicative

import MIL.AST
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TcError
import MIL.Utils

-- | Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- For details see 'checkTypeWithTypeVars'.
checkType :: Type -> TypeCheckM ()
checkType = checkTypeWithTypeVars Set.empty

-- | Checks if the type is well-formed, well-kinded and uses types in scope.
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions).
--
-- For details see 'checkTypeWithTypeVarsOfKind'.
checkTypeWithTypeVars :: Set.Set TypeVar -> Type -> TypeCheckM ()
checkTypeWithTypeVars typeVars = checkTypeWithTypeVarsOfKind typeVars StarK

-- | Checks if the type is well-formed, uses types in scope, has a given kind and
-- that all nested types are well-kinded.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions).
--
-- Type is ill-formed when something other than a type constructor, another
-- type application or a monad is on the left-hand side of the type
-- application.
--
-- Type is ill-kinded when a type constructor is not fully applied. There is
-- also a check that type variables are not applied (they are of kind *).
--
-- We keep a set of type variables which are currently in scope.
-- There is a kind construction going to, see inline comments.
checkTypeWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> Type -> TypeCheckM ()
checkTypeWithTypeVarsOfKind typeVars kind t =
  case t of
    TyTypeCon typeName -> do
      ifM (isTypeDefinedM typeName)
        (do dataTypeKind <- getDataTypeKindM typeName
            when (dataTypeKind /= kind) $
              throwError $ TypeConIncorrectApp typeName dataTypeKind kind)
        (ifM (isAliasDefinedM typeName)
           (do aliasType <- getAliasTypeM typeName
               -- Aliased types should be checked without the type variables we
               -- have collected in scope, because they were defined in a
               -- different scope.
               -- TODO: what if it is TyForAll or TyArrow?
               checkTypeWithTypeVarsOfKind Set.empty kind aliasType)
           (throwError $ TypeNotDefined typeName))

    TyVar typeVar -> do
      isTyVarBound <- isTypeVarBoundM typeVar
      -- It is important to check in both places, since typeVars is not queried
      -- by 'isTypeVarBoundM'.
      unless (isTyVarBound || typeVar `Set.member` typeVars) $
        throwError $ TypeVarNotInScope typeVar

    TyArrow t1 t2 -> do
      -- Type vars set and kind modifications are local to each side of the
      -- arrow.
      checkTypeWithTypeVarsOfKind typeVars kind t1
      checkTypeWithTypeVarsOfKind typeVars kind t2

    TyForAll typeVar bodyT -> do
      -- It is important to check in all these places, since it can shadow a
      -- type, type alias or another type variable and typeVars is not queried
      -- by 'isTypeOrAliasDefinedM' and 'isTypeVarBoundM'.
      whenM (isTypeOrAliasDefinedM $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      isTyVarBound <- isTypeVarBoundM typeVar
      when (isTyVarBound || typeVar `Set.member` typeVars) $
        throwError $ TypeVarShadowsTypeVar typeVar
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
          checkTypeWithTypeVarsOfKind typeVars StarK t2
        TyApp {} -> do
          -- One more type application on the left, add * to the kind.
          checkTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) t1
          -- Start from kind *, it is another type constructor (another
          -- application).
          checkTypeWithTypeVarsOfKind typeVars StarK t2
        TyMonad tm -> do
          checkTypeMWithTypeVars typeVars tm
          -- Start from kind *, it is another type constructor (another
          -- application).
          checkTypeWithTypeVarsOfKind typeVars StarK t2
        TyVar tv -> throwError $ TypeVarApp tv
        _ -> throwError $ IllFormedType t

    TyTuple elemTypes ->
      -- All types of tuple elements must be of kind *.
      mapM_ (checkTypeWithTypeVarsOfKind typeVars StarK) elemTypes

    TyMonad tm -> checkTypeMWithTypeVars typeVars tm

-- | Checking the monadic type.
checkTypeM :: TypeM -> TypeCheckM ()
checkTypeM = checkTypeMWithTypeVars Set.empty

-- | Checking the monadic type with a set of type variables in scope.
checkTypeMWithTypeVars :: Set.Set TypeVar -> TypeM -> TypeCheckM ()
checkTypeMWithTypeVars typeVars (MTyMonad m) = checkTypeMMonadWithTypeVars typeVars m
checkTypeMWithTypeVars typeVars (MTyMonadCons m tm) = do
  checkTypeMMonadWithTypeVars typeVars m
  checkTypeMWithTypeVars typeVars tm
{-
checkTypeMWithTypeVars _ (MTyAlias typeName) =
  (ifM (isAliasDefinedM typeName)
      (do aliasType <- getAliasTypeM typeName
          -- Aliased types should be checked without the type variables we have
          -- collected in scope, because they were defined in a different
          -- scope. We specify kind *, since we are checking a monad alias.
          checkTypeWithTypeVarsOfKind Set.empty StarK aliasType)
      (throwError $ TypeNotDefined typeName))
-}

-- | For monads with type parameters: perform checking of those types (they
-- must be of kind *). TODO: complete.
checkTypeMMonadWithTypeVars :: Set.Set TypeVar -> TypeMMonad -> TypeCheckM ()
checkTypeMMonadWithTypeVars typeVars (TypeMMilMonad (Error t)) = checkTypeWithTypeVarsOfKind typeVars StarK t
--checkTypeMMonadWithTypeVars _ _ = return ()

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
-- TODO: Pure alpha equivalence with supplied aliases.

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
-- When we have a type name on either of the sides and it happens to be a type
-- alias, we expand it.
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
  alphaEq (TyTypeCon typeName) t = do
    ifM (isAliasDefinedM typeName)
      (do aliasType <- getAliasTypeM typeName
          aliasType `alphaEq` t)
      (return False)
  alphaEq t1 t2@(TyTypeCon {}) = t2 `alphaEq` t1
  alphaEq _ _ = return False

-- | When we have a type alias on either of the sides we expand it.
instance AlphaEq TypeM where
  alphaEq (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
  alphaEq (MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) =
    (&&) <$> (m1 `alphaEq` m2) <*> (tm1 `alphaEq` tm2)
{-
  alphaEq (MTyAlias typeName1) (MTyAlias typeName2) = return (typeName1 == typeName2)
  alphaEq (MTyAlias typeName) tm2 = do
    ifM (isAliasDefinedM typeName)
      (do aliasType <- getAliasTypeM typeName
          case aliasType of
            TyMonad tm1 -> tm1 `alphaEq` tm2
            _ -> return False)
      (return False)
  alphaEq tm1 tm2@(MTyAlias {}) = tm2 `alphaEq` tm1
-}
  alphaEq _ _ = return False

-- | For monads that have type arguments check that these arguments are alpha
-- equivalent. For the others it is just an equality.
instance AlphaEq TypeMMonad where
  alphaEq (TypeMMilMonad (Error t1)) (TypeMMilMonad (Error t2)) = t1 `alphaEq` t2
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

instance SubstType TypeM where
  tvArg `substTypeIn` (MTyMonad m) = MTyMonad (tvArg `substTypeIn` m)
  tvArg `substTypeIn` (MTyMonadCons m tm) =
    MTyMonadCons (tvArg `substTypeIn` m) (tvArg `substTypeIn` tm)

-- | Perform substitution on type parameters if there are any, otherwise, just
-- return a monad.
-- Type aliases are defined in a completely separate scope, so we don't
-- expand them to perform substitution and just keep them unchanged.
instance SubstType TypeMMonad where
  tvArg `substTypeIn` (TypeMMilMonad (Error t)) = TypeMMilMonad $ Error (tvArg `substTypeIn` t)
  _ `substTypeIn` m = m

-- * 'TypeM' helpers

-- | Checks if two monads are compatible. This means if one of them is alpha
-- equivalent to another or one of them is a prefix of another.
-- For example, m1 is a prefix of m1 ::: m2.
--
-- This operation is commutative.
compatibleMonadTypes :: TypeM -> TypeM -> TypeCheckM Bool
compatibleMonadTypes (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
{-
compatibleMonadTypes (MTyAlias typeName1) (MTyAlias typeName2) = do
  aliasTypesCompatible <-
    ifM ((&&) <$> isAliasDefinedM typeName1 <*> isAliasDefinedM typeName2)
      (do aliasType1 <- getAliasTypeM typeName1
          aliasType2 <- getAliasTypeM typeName2
          case (aliasType1, aliasType2) of
            (TyMonad tm1, TyMonad tm2) -> compatibleMonadTypes tm1 tm2
            _ -> return False)
      (return False)
  return (typeName1 == typeName2 || aliasTypesCompatible)
-}
compatibleMonadTypes (MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) =
  (&&) <$> (m1 `alphaEq` m2) <*> compatibleMonadTypes tm1 tm2
compatibleMonadTypes (MTyMonad m1) (MTyMonadCons m2 _) = m1 `alphaEq` m2
{-
compatibleMonadTypes (MTyAlias typeName1) t2@(MTyMonadCons {}) = do
  ifM (isAliasDefinedM typeName1)
    (do aliasType1 <- getAliasTypeM typeName1
        case aliasType1 of
          TyMonad tm1 -> compatibleMonadTypes tm1 t2
          _ -> return False)
    (return False)
compatibleMonadTypes t1@(MTyMonad {}) (MTyAlias typeName2) = do
  ifM (isAliasDefinedM typeName2)
    (do aliasType2 <- getAliasTypeM typeName2
        case aliasType2 of
          TyMonad tm2 -> compatibleMonadTypes t1 tm2
          _ -> return False)
    (return False)
-}
compatibleMonadTypes tm1 tm2 = compatibleMonadTypes tm2 tm1

-- | Compares two monads in terms of their effects.
-- Main idea: monad cons cell has more effects than a single monad.
--
-- Assumption: 'compatibleMonadTypes' returned True.
--
-- This operation is *not* commutative.
hasMoreEffectsThan :: TypeM -> TypeM -> TypeCheckM Bool
hasMoreEffectsThan (MTyMonadCons _ tm1) (MTyMonadCons _ tm2) = tm1 `hasMoreEffectsThan` tm2
hasMoreEffectsThan (MTyMonadCons {}) (MTyMonad {}) = return True
{-
hasMoreEffectsThan t1 (MTyAlias typeName2) = do
  aliasType2 <- getAliasTypeM typeName2
  case aliasType2 of
    TyMonad t2 -> t1 `hasMoreEffectsThan` t2
-}
hasMoreEffectsThan (MTyMonad {}) (MTyMonad {}) = return False
--hasMoreEffectsThan t1@(MTyAlias {}) t2 = not <$> t2 `hasMoreEffectsThan` t1
hasMoreEffectsThan t1@(MTyMonad {}) t2@(MTyMonadCons {}) = not <$> t2 `hasMoreEffectsThan` t1

-- | Checks if the first monad is a suffix of the second one or if they are
-- alpha equivalent.
-- For example, m2 is a suffix of m1 ::: m2.
--
-- This operation is *not* commutative.
isMonadSuffixOf :: TypeM -> TypeM -> TypeCheckM Bool
isMonadSuffixOf (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
{-
isMonadSuffixOf (MTyAlias typeName1) (MTyAlias typeName2) = do
  aliasTypesSuffix <-
    ifM ((&&) <$> isAliasDefinedM typeName1 <*> isAliasDefinedM typeName2)
      (do aliasType1 <- getAliasTypeM typeName1
          aliasType2 <- getAliasTypeM typeName2
          case (aliasType1, aliasType2) of
            (TyMonad tm1, TyMonad tm2) -> tm1 `isMonadSuffixOf` tm2
            _ -> return False)
      (return False)
  return (typeName1 == typeName2 || aliasTypesSuffix)
-}
isMonadSuffixOf t1@(MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) = do
  isSuffix <- (&&) <$> (m1 `alphaEq` m2) <*> tm1 `isMonadSuffixOf` tm2
  isShiftedSuffix <- t1 `isMonadSuffixOf` tm2
  return (isSuffix || isShiftedSuffix)
isMonadSuffixOf t1@(MTyMonad {}) (MTyMonadCons _ tm2) = t1 `isMonadSuffixOf` tm2
{-
isMonadSuffixOf (MTyAlias typeName1) t2@(MTyMonadCons {}) = do
  ifM (isAliasDefinedM typeName1)
    (do aliasType1 <- getAliasTypeM typeName1
        case aliasType1 of
          TyMonad tm1 -> tm1 `isMonadSuffixOf` t2
          _ -> return False)
    (return False)
isMonadSuffixOf t1@(MTyMonadCons {}) (MTyAlias typeName2) = do
  ifM (isAliasDefinedM typeName2)
    (do aliasType2 <- getAliasTypeM typeName2
        case aliasType2 of
          TyMonad tm2 -> t1 `isMonadSuffixOf` tm2
          _ -> return False)
    (return False)
isMonadSuffixOf t1@(MTyMonad {}) (MTyAlias typeName2) = do
  ifM (isAliasDefinedM typeName2)
    (do aliasType2 <- getAliasTypeM typeName2
        case aliasType2 of
          TyMonad tm2 -> t1 `isMonadSuffixOf` tm2
          _ -> return False)
    (return False)
isMonadSuffixOf (MTyAlias typeName1) t2@(MTyMonad {}) = do
  ifM (isAliasDefinedM typeName1)
    (do aliasType1 <- getAliasTypeM typeName1
        case aliasType1 of
          TyMonad tm1 -> tm1 `isMonadSuffixOf` t2
          _ -> return False)
    (return False)
-}
isMonadSuffixOf (MTyMonadCons {}) (MTyMonad {}) = return False

-- TODO
containsMonad :: Type -> TypeM -> TypeCheckM Bool
containsMonad = undefined

