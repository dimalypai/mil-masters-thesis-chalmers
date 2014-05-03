-- | Functions for working with types. Used in the type checker.
module FunLang.TypeChecker.Helpers where

import qualified Data.Set as Set

import FunLang.AST
import FunLang.AST.Helpers
import FunLang.BuiltIn
import FunLang.TypeChecker.TypeCheckM
import FunLang.TypeChecker.TcError
import FunLang.Utils

-- | Transforms a source representation of type into an internal one.
-- Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- For details see 'srcTypeToTypeWithTypeVars'.
srcTypeToType :: SrcType -> TypeCheckM Type
srcTypeToType = srcTypeToTypeWithTypeVars Set.empty

-- | Transforms a source representation of type into an internal one.
-- Checks if the type is well-formed, uses types in scope and has a given kind.
--
-- For details see 'srcTypeToTypeWithTypeVarsOfKind'.
srcTypeToTypeOfKind :: Kind -> SrcType -> TypeCheckM Type
srcTypeToTypeOfKind = srcTypeToTypeWithTypeVarsOfKind Set.empty

-- | Transforms a source representation of type into an internal one with a set
-- of type variables already in scope (used for type parameters in data type
-- definitions).
-- Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- For details see 'srcTypeToTypeWithTypeVarsOfKind'.
srcTypeToTypeWithTypeVars :: Set.Set TypeVar -> SrcType -> TypeCheckM Type
srcTypeToTypeWithTypeVars tvars = srcTypeToTypeWithTypeVarsOfKind tvars StarK

-- | Transforms a source representation of type into an internal one with a set
-- of type variables already in scope (used for type parameters in data type
-- definitions).
-- Checks if the type is well-formed, uses types in scope, has a given kind and
-- that all nested types are well-kinded.
--
-- Type is ill-formed when something other than a type constructor or
-- another type application or parenthesised type is on the left-hand side of
-- the type application.
--
-- Type is ill-kinded when a type constructor is not fully applied. There is
-- also a check that type variables are not applied (they are of kind *).
--
-- 'SrcTyCon' which may stand for a type variable or a type constructor is
-- transformed accordingly (if it is in scope). Kind checking is done at this
-- point.
-- 'SrcTyForAll' adds type variable to the scope after checking whether it
-- shadows existing types or type variables in scope.
-- The most interesting case is 'SrcTyApp', which is binary as opposed to the
-- 'TyApp'. We recursively dive into the left-hand side of the application and
-- collect transformed right-hand sides to the arguments list until we hit the
-- 'SrcTyCon'.
-- There is a kind construction going to, see inline comments.
--
-- We keep a set of type variables which are currently in scope.
-- During the transformation we construct a kind that a type constructor
-- should have, see 'SrcTyApp' case where it is modified and 'SrcTyCon'
-- case where it is checked.
srcTypeToTypeWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> SrcType -> TypeCheckM Type
srcTypeToTypeWithTypeVarsOfKind typeVars kind (SrcTyCon srcTypeName) = do
  let typeName = getTypeName srcTypeName
      typeVar = typeNameToTypeVar typeName
  ifM (isTypeDefined typeName)
    (do dataTypeInfo <- getDataTypeInfo typeName
        when (dtiKind dataTypeInfo /= kind) $
          throwError $ TypeConIncorrectApp srcTypeName (dtiKind dataTypeInfo) kind
        return $ TyApp typeName [])
    (do isTyVarBound <- isTypeVarBound typeVar
        -- it is important to check in both places, since typeVars is not
        -- queried by 'isTypeVarBound'
        if isTyVarBound || typeVar `Set.member` typeVars
          then return $ TyVar typeVar
          else throwError $ TypeNotDefined srcTypeName)

srcTypeToTypeWithTypeVarsOfKind typeVars kind st@(SrcTyApp _ stl str) = handleTyApp st stl str kind []
  where handleTyApp stApp st1 st2 k args =
        -- stApp is the whole SrcTyApp - used for error message
        -- st1 and st2 are components of the type application
        -- k is the kind that a type constructor should have
        -- in args we collect the arguments for the internal representation
          case st1 of
            sTyCon@(SrcTyCon srcTypeName) -> do
              -- type constructor is on the left-hand side of the
              -- application, it should have extra * in the kind (on the
              -- left) in order for the type to be well-kinded.
              tyConOrVar <- srcTypeToTypeWithTypeVarsOfKind typeVars (StarK :=>: k) sTyCon
              when (isTypeVar tyConOrVar) $
                throwError $ TypeVarApp (srcTypeNameToTypeVar srcTypeName)
              let typeName = getTyAppTypeName tyConOrVar  -- should not fail
              -- start from kind *, it is another type constructor (another application)
              t2 <- srcTypeToTypeWithTypeVarsOfKind typeVars StarK st2
              return $ TyApp typeName (t2 : args)
            stApp'@(SrcTyApp _ st1' st2') -> do
              -- start from kind *, it is another type constructor (another application)
              t2 <- srcTypeToTypeWithTypeVarsOfKind typeVars StarK st2
              -- one more type application on the left, add * to the kind
              handleTyApp stApp' st1' st2' (StarK :=>: k) (t2 : args)
            -- just strip off the parens and replace the left-hand side
            SrcTyParen _ st' -> handleTyApp stApp st' st2 k args
            _ -> throwError $ IllFormedType stApp

srcTypeToTypeWithTypeVarsOfKind typeVars kind (SrcTyArrow _ st1 st2) = do
  -- type vars set and kind modifications are local to each side of the arrow
  t1 <- srcTypeToTypeWithTypeVarsOfKind typeVars kind st1
  t2 <- srcTypeToTypeWithTypeVarsOfKind typeVars kind st2
  return $ TyArrow t1 t2

srcTypeToTypeWithTypeVarsOfKind typeVars kind (SrcTyForAll _ srcTypeVar st) = do
  let typeVar = getTypeVar srcTypeVar
  -- it is important to check in all these places, since it can shadow a type
  -- or another type variable and typeVars is not queried by 'isTypeDefined'
  -- and 'isTypeVarBound', see `TypeVarShadowsTypeVar` test case
  whenM (isTypeDefined $ typeVarToTypeName typeVar) $
    throwError $ TypeVarShadowsType srcTypeVar
  isTyVarBound <- isTypeVarBound typeVar
  when (isTyVarBound || typeVar `Set.member` typeVars) $
    throwError $ TypeVarShadowsTypeVar srcTypeVar
  let typeVars' = Set.insert typeVar typeVars
  t <- srcTypeToTypeWithTypeVarsOfKind typeVars' kind st
  return $ TyForAll typeVar t

srcTypeToTypeWithTypeVarsOfKind typeVars kind (SrcTyParen _ st) =
  srcTypeToTypeWithTypeVarsOfKind typeVars kind st

-- | Constructs an arrow type given a result type and a list of parameter
-- types.
tyArrowFromList :: Type -> [Type] -> Type
tyArrowFromList resultType = foldr (\t acc -> TyArrow t acc) resultType

-- | Constructs a forall type given a body type and a list of type variables.
tyForAllFromList :: Type -> [TypeVar] -> Type
tyForAllFromList bodyType = foldr (\tv acc -> TyForAll tv acc) bodyType

-- | Alpha equivalence of types.
-- Decides if two types are equivalent up to change of type variables bound by
-- forall.
--
-- For example, forall A . A -> A and forall B . B -> B are alpha equivalent.
--
-- Most of the cases are straightforward. When we get two forall types, we
-- replace one of the type variables (its free occurences) with another one and
-- check whether the resulting types are alpha equivalent.
alphaEq :: Type -> Type -> Bool
alphaEq (TyVar typeVar1) (TyVar typeVar2) = typeVar1 == typeVar2
alphaEq (TyArrow t11 t12) (TyArrow t21 t22) =
  t11 `alphaEq` t21 && t12 `alphaEq` t22
alphaEq (TyApp typeName1 argTypes1) (TyApp typeName2 argTypes2) =
  typeName1 == typeName2 && all (uncurry alphaEq) (zip argTypes1 argTypes2)
alphaEq (TyForAll tv1 t1) (TyForAll tv2 t2) = t1 `alphaEq` ((tv2, TyVar tv1) `substTypeIn` t2)
alphaEq _ _ = False

-- | Type substitution.
--
-- Replace free occurences of the type variable (first component of the first
-- parameter) with the type (second component of the first parameter) in the
-- type given as a second parameter.
-- The case that actually does the job is 'TyVar'. Another interesting case is
-- 'TyForAll'. We don't allow shadowing and all type variables in the type are
-- bound (types are closed), but it is possible to introduce shadowing by
-- substitution:
--
-- (forall A . forall B . A -> B)[A := forall B . B] =>
-- => forall B . (forall B . B) -> B
--
-- It is not clear that such type can arise from the program (if it is possible
-- to construct such a term), but we still need to handle this case, hence the
-- otherwise guard clause.
substTypeIn :: (TypeVar, Type) -> Type -> Type
(typeVar, argType) `substTypeIn` t@(TyVar typeVar')
  | typeVar == typeVar' = argType
  | otherwise           = t
tvArg `substTypeIn` (TyArrow t1 t2) =
  TyArrow (tvArg `substTypeIn` t1) (tvArg `substTypeIn` t2)
tvArg `substTypeIn` (TyApp typeName argTypes) =
  TyApp typeName (map (tvArg `substTypeIn`) argTypes)
tvArg@(typeVar, _) `substTypeIn` forallT@(TyForAll tv t)
  | typeVar /= tv = TyForAll tv (tvArg `substTypeIn` t)
  | otherwise     = forallT

-- | Check if the given type is the 'TyApp' with one of built-in monads.
-- Kind is *not* checked.
isMonadType :: Type -> Bool
isMonadType (TyApp typeName _) = typeName `Set.member` monadTypes
isMonadType                  _ = False

