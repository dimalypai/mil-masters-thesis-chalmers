-- | Functions for working with types. Used in the type checker.
module OOLang.TypeChecker.Helpers where

import Control.Applicative ((<$>), (<*>))

import OOLang.AST
import OOLang.AST.Helpers
import OOLang.TypeChecker.TypeCheckM
import OOLang.TypeChecker.TcError
import OOLang.Utils

-- | Subtyping relation.
-- * It is reflexive (type is a subtype of itself).
--
-- * Pure A `isSubTypeOf` B iff A `isSubTypeOf` B.
--
-- * A `isSubTypeOf` Pure B iff A `isSubTypeOf` B.
--
-- * Pure A `isSubTypeOf` Pure B iff A `isSubTypeOf` B
--
-- Mutables are treated by value, so they can be used in the same places as
-- ordinary types, except for special declaration and assignment operators
-- usage. And this gives us:
--
-- * Mutable A `isSubTypeOf` B iff A `isSubTypeOf` B
--
-- * A `isSubTypeOf` Mutable B iff A `isSubTypeOf` B
--
-- * Mutable A `isSubTypeOf` Mutable B iff A `isSubTypeOf` B
--
-- Inheritance and transitivity:
--
-- * A `isSubTypeOf` B iff class A < B or class A < C and C `isSubTypeOf` B
--
-- References are covariant:
--
-- * Ref A `isSubTypeOf` Ref B iff A `isSubTypeOf` B
--
-- Note: this can't be used for purity checking.
isSubTypeOf :: Type -> Type -> TypeCheckM Bool
t1 `isSubTypeOf` t2 = do
  pureSubType1 <- case t1 of
                    TyPure pt1 -> pt1 `isSubTypeOf` t2
                    _ -> return False
  pureSubType2 <- case t2 of
                    TyPure pt2 -> t1 `isSubTypeOf` pt2
                    _ -> return False
  pureSubType3 <- case (t1, t2) of
                    (TyPure pt1, TyPure pt2) -> pt1 `isSubTypeOf` pt2
                    _ -> return False
  mutableSubType1 <- case t1 of
                       TyMutable pt1 -> pt1 `isSubTypeOf` t2
                       _ -> return False
  mutableSubType2 <- case t2 of
                       TyMutable pt2 -> t1 `isSubTypeOf` pt2
                       _ -> return False
  mutableSubType3 <- case (t1, t2) of
                       (TyMutable pt1, TyMutable pt2) -> pt1 `isSubTypeOf` pt2
                       _ -> return False
  inheritanceSubType <-
    case (t1, t2) of
      (TyClass className1, TyClass className2) -> do
        mSuperClassName <- getSuperClass className1
        case mSuperClassName of
          Nothing -> return False
          Just superClassName -> do
            superSubType <- (TyClass superClassName) `isSubTypeOf` t2
            return (superClassName == className2 || superSubType)
      _ -> return False
  refCovariance <-
    case (t1, t2) of
      (TyRef refT1, TyRef refT2) -> refT1 `isSubTypeOf` refT2
      _ -> return False
  return (t1 == t2
       || pureSubType1 || pureSubType2  || pureSubType3
       || mutableSubType1 || mutableSubType2 || mutableSubType3
       || inheritanceSubType || refCovariance)

-- * Type transformations

-- | Converts a source representation of type to an internal one.
-- Checks if it is well-formed:
-- * all used types are defined.
-- * arrow type is well formed (see 'srcFunTypeToType')
-- * it is not Pure (it is allowed only in function return type)
-- * Maybe, Mutable and Ref can have Mutable or Ref inside only through type arrows
srcTypeToType :: SrcType -> TypeCheckM Type
srcTypeToType (SrcTyUnit  _) = return TyUnit
srcTypeToType (SrcTyBool  _) = return TyBool
srcTypeToType (SrcTyInt   _) = return TyInt
srcTypeToType (SrcTyFloat _) = return TyFloat

srcTypeToType (SrcTyClass srcClassName) = do
  let className = getClassName srcClassName
  unlessM (isClassDefined className) $
    throwError $ ClassNotDefined srcClassName
  return $ TyClass className

srcTypeToType (SrcTyArrow _ st1 st2) =
  TyArrow <$> srcFunParamTypeToType st1 <*> srcFunReturnTypeToType st2
srcTypeToType st@(SrcTyPure {}) = throwError $ PureValue st
srcTypeToType stM@(SrcTyMaybe _ st) =
  if isMutableOrRefNested st
    then throwError $ MutableOrRefNested stM
    else TyMaybe <$> srcTypeToType st
srcTypeToType stM@(SrcTyMutable _ st) =
  if isMutableOrRefNested st
    then throwError $ MutableOrRefNested stM
    else TyMutable <$> srcTypeToType st
srcTypeToType stR@(SrcTyRef _ st) =
  if isMutableOrRefNested st
    then throwError $ MutableOrRefNested stR
    else TyRef <$> srcTypeToType st
srcTypeToType (SrcTyParen _ st) = srcTypeToType st

-- | Traverses a source type and checks if there are Mutable of Ref types nested.
-- Doesn't go into type arrows and Pure.
isMutableOrRefNested :: SrcType -> Bool
isMutableOrRefNested (SrcTyMutable {}) = True
isMutableOrRefNested (SrcTyRef     {}) = True
isMutableOrRefNested (SrcTyMaybe _ st) = isMutableOrRefNested st
isMutableOrRefNested (SrcTyParen _ st) = isMutableOrRefNested st
isMutableOrRefNested                 _ = False

-- | Transforms function type which has variable binders and return type to one
-- big internal type (right associative type arrow without parameter names).
-- Checks if it is well-formed:
-- * parameter types are well-formed ('srcFunParamTypeToType')
-- * return type is well-formed ('srcFunReturnTypeToType')
-- * general rules ('srcTypeToType')
-- Does *not* check parameter names.
srcFunTypeToType :: SrcFunType -> TypeCheckM Type
srcFunTypeToType (FunType _ varBinders srcRetType) = do
  retType <- srcFunReturnTypeToType srcRetType
  paramTypes <- mapM (srcFunParamTypeToType . getBinderType) varBinders
  return $ tyArrowFromList retType paramTypes

-- | Transforms function return type to the internal representation.
-- Checks if it is well-formed:
-- * doesn't use Mutable, Pure
-- * general rules ('srcTypeToType')
srcFunParamTypeToType :: SrcType -> TypeCheckM Type
srcFunParamTypeToType st@(SrcTyPure    {}) = throwError $ PureFunParam st
srcFunParamTypeToType st@(SrcTyMutable {}) = throwError $ MutableFunParam st
srcFunParamTypeToType (SrcTyParen _ st) = srcFunParamTypeToType st
srcFunParamTypeToType st = srcTypeToType st

-- | Transforms function parameter type to the internal representation.
-- Checks if it is well-formed:
-- * doesn't use Mutable
-- * general rules ('srcTypeToType')
srcFunReturnTypeToType :: SrcType -> TypeCheckM Type
srcFunReturnTypeToType (SrcTyPure    _ st) = TyPure <$> srcTypeToType st
srcFunReturnTypeToType st@(SrcTyMutable {}) = throwError $ MutableFunReturnType st
srcFunReturnTypeToType (SrcTyParen   _ st) = srcFunReturnTypeToType st
srcFunReturnTypeToType st = srcTypeToType st

-- | Constructs an arrow type given a result type and a list of parameter
-- types.
tyArrowFromList :: Type -> [Type] -> Type
tyArrowFromList resultType = foldr (\t acc -> TyArrow t acc) resultType

