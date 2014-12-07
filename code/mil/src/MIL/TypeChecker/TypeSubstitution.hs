-- | Type substitution helpers module.
module MIL.TypeChecker.TypeSubstitution where

import MIL.AST

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

