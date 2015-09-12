-- | Module for working with alpha equivalence of types.
module MIL.TypeChecker.AlphaEq where

import MIL.AST
import MIL.TypeChecker.TypeSubstitution

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
    (t11 `alphaEq` t21) && (t12 `alphaEq` t22)
  alphaEq (TyApp t11 t12) (TyApp t21 t22) =
    (t11 `alphaEq` t21) && (t12 `alphaEq` t22)
  alphaEq (TyForAll tv1 t1) (TyForAll tv2 t2) =
    t1 `alphaEq` ((tv2, TyVar tv1) `substTypeIn` t2)
  alphaEq (TyTuple elemTypes1) (TyTuple elemTypes2) =
    let lengthEq = length elemTypes1 == length elemTypes2
        elemsAlphaEq = zipWith alphaEq elemTypes1 elemTypes2
    in lengthEq && and elemsAlphaEq
  alphaEq (TyMonad tm1) (TyMonad tm2) = tm1 `alphaEq` tm2
  alphaEq _ _ = False

instance AlphaEq MonadType where
  alphaEq (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
  alphaEq (MTyMonadCons m1 tm1) (MTyMonadCons m2 tm2) =
    (m1 `alphaEq` m2) && (tm1 `alphaEq` tm2)
  alphaEq _ _ = False

instance AlphaEq SingleMonad where
  alphaEq (SinMonad m1) (SinMonad m2) = m1 `alphaEq` m2
  alphaEq (SinMonadApp m1 t1) (SinMonadApp m2 t2) =
    (m1 `alphaEq` m2) && (t1 `alphaEq` t2)
  alphaEq _ _ = False

instance AlphaEq MilMonad where
  alphaEq m1 m2 = m1 == m2

