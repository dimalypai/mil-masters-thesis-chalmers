-- | Functions for working with types. Used in the type checker.
module MIL.TypeChecker.Helpers where

import qualified Data.Set as Set
import Data.List (foldl')

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
      ifM (isTypeDefined typeName)
        (do dataTypeInfo <- getDataTypeInfo typeName
            when (dtiKind dataTypeInfo /= kind) $
              throwError $ TypeConIncorrectApp typeName (dtiKind dataTypeInfo) kind)
        (throwError $ TypeNotDefined typeName)

    TyVar typeVar -> do
      isTyVarBound <- isTypeVarBound typeVar
      -- It is important to check in both places, since typeVars is not queried
      -- by 'isTypeVarBound'.
      unless (isTyVarBound || typeVar `Set.member` typeVars) $
        throwError $ TypeVarNotInScope typeVar

    TyArrow t1 t2 -> do
      -- Type vars set and kind modifications are local to each side of the
      -- arrow.
      checkTypeWithTypeVarsOfKind typeVars kind t1
      checkTypeWithTypeVarsOfKind typeVars kind t2

    TyForAll typeVar bodyT -> do
      -- It is important to check in all these places, since it can shadow a
      -- type or another type variable and typeVars is not queried by
      -- 'isTypeDefined' and 'isTypeVarBound'.
      whenM (isTypeDefined $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      isTyVarBound <- isTypeVarBound typeVar
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

    TyMonad tm -> checkTypeMWithTypeVars typeVars tm

-- | Checking the monadic type.
checkTypeM :: TypeM -> TypeCheckM ()
checkTypeM = checkTypeMWithTypeVars Set.empty

-- | Checking the monadic type with a set of type variables in scope.
checkTypeMWithTypeVars :: Set.Set TypeVar -> TypeM -> TypeCheckM ()
checkTypeMWithTypeVars typeVars (MTyMonad m) = checkMilMonadWithTypeVars typeVars m
checkTypeMWithTypeVars typeVars (MTyMonadCons m tm) = do
  checkMilMonadWithTypeVars typeVars m
  checkTypeMWithTypeVars typeVars tm

-- | For monads with type parameters: perform checking of those types (they
-- must be of kind *). All the others are correct.
checkMilMonadWithTypeVars :: Set.Set TypeVar -> MilMonad -> TypeCheckM ()
checkMilMonadWithTypeVars typeVars (State t) = checkTypeWithTypeVarsOfKind typeVars StarK t
checkMilMonadWithTypeVars typeVars (Error t) = checkTypeWithTypeVarsOfKind typeVars StarK t
checkMilMonadWithTypeVars _ _ = return ()

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

-- * Alpha equivalence of types.

-- | Decides if two types are equivalent up to a change of type variables bound
-- by forall.
--
-- For example, forall A . A -> A and forall B . B -> B are alpha equivalent.
class AlphaEq t where
  alphaEq :: t -> t -> Bool

-- | Most of the cases are straightforward. When we get two forall types, we
-- replace one of the type variables (its free occurences) with another one and
-- check whether the resulting types are alpha equivalent.
-- Use 'TypeM' instance for 'TyMonad'.
instance AlphaEq Type where
  alphaEq (TyTypeCon typeName1) (TyTypeCon typeName2) = typeName1 == typeName2
  alphaEq (TyVar typeVar1) (TyVar typeVar2) = typeVar1 == typeVar2
  alphaEq (TyArrow t11 t12) (TyArrow t21 t22) =
    t11 `alphaEq` t21 && t12 `alphaEq` t22
  alphaEq (TyApp t11 t12) (TyApp t21 t22) =
    t11 `alphaEq` t21 && t12 `alphaEq` t22
  alphaEq (TyForAll tv1 t1) (TyForAll tv2 t2) = t1 `alphaEq` ((tv2, TyVar tv1) `substTypeIn` t2)
  alphaEq (TyMonad tm1) (TyMonad tm2) = tm1 `alphaEq` tm2
  alphaEq _ _ = False

instance AlphaEq TypeM where
  alphaEq (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
  alphaEq (MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) =
    m1 `alphaEq` m2 && tm1 `alphaEq` tm2
  alphaEq _ _ = False

-- | For monads that have type arguments check that these arguments are alpha
-- equivalent. For the others it is just an equality.
instance AlphaEq MilMonad where
  alphaEq (State t1) (State t2) = t1 `alphaEq` t2
  alphaEq (Error t1) (Error t2) = t1 `alphaEq` t2
  alphaEq m1 m2 = m1 == m2

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
  tvArg `substTypeIn` (TyMonad tm) = TyMonad (tvArg `substTypeIn` tm)

instance SubstType TypeM where
  tvArg `substTypeIn` (MTyMonad m) = MTyMonad (tvArg `substTypeIn` m)
  tvArg `substTypeIn` (MTyMonadCons m tm) =
    MTyMonadCons (tvArg `substTypeIn` m) (tvArg `substTypeIn` tm)

-- | Perform substitution on type parameters if there are any, otherwise, just
-- return a monad.
instance SubstType MilMonad where
  tvArg `substTypeIn` (State t) = State (tvArg `substTypeIn` t)
  tvArg `substTypeIn` (Error t) = Error (tvArg `substTypeIn` t)
  _ `substTypeIn` m = m

