-- | Functions for working with source representation of types. Used in the type checker.
module MIL.TypeChecker.SrcTypeHelpers where

import qualified Data.Set as Set
import Control.Applicative

import MIL.AST
import MIL.AST.Builder
import MIL.AST.Helpers
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TcError
import MIL.BuiltIn
import MIL.Utils

-- | Transforms source representation of a type to an internal one.
-- Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- For details see 'srcTypeToTypeWithTypeVars'.
srcTypeToType :: SrcType -> TypeCheckM Type
srcTypeToType = srcTypeToTypeWithTypeVars Set.empty

-- | Transforms source representation of a type to an internal one.
-- Checks if the type is well-formed, well-kinded and uses types in scope.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
--
-- For details see 'srcTypeToTypeWithTypeVarsOfKind'.
srcTypeToTypeWithTypeVars :: Set.Set TypeVar -> SrcType -> TypeCheckM Type
srcTypeToTypeWithTypeVars typeVars = srcTypeToTypeWithTypeVarsOfKind typeVars StarK

-- | Transforms source representation of a type to an internal one.
-- Checks if the type is well-formed, uses types in scope, has a given kind and
-- that all nested types are well-kinded.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
--
-- For details see inline comments.
srcTypeToTypeWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> SrcType -> TypeCheckM Type
srcTypeToTypeWithTypeVarsOfKind typeVars kind srcType =
  case srcType of
    SrcTyTypeCon typeName ->
      ifM (isTypeDefinedM typeName)
        (do dataTypeKind <- getDataTypeKindM typeName
            when (dataTypeKind /= kind) $
              throwError $ TypeConIncorrectApp typeName dataTypeKind kind
            return $ TyTypeCon typeName)
        (do let typeVar = typeNameToTypeVar typeName
            isTyVarBound <- isTypeVarBoundM typeVar
            -- it is important to check in both places, since typeVars is not
            -- queried by 'isTypeVarBoundM'
            if isTyVarBound || typeVar `Set.member` typeVars
              then return $ TyVar typeVar
              -- Built-in monads are represented by type constructors
              else if isBuiltInMonad typeName
                     then do
                       let monadKind = getBuiltInMonadKind typeName
                       when (monadKind /= kind) $
                         throwError $ TypeConIncorrectApp typeName monadKind kind
                       return $ TyMonad $ MTyMonad $ SinMonad (mkBuiltInMonad typeName)
                     else throwError $ TypeNotDefined typeName)

    SrcTyArrow st1 st2 -> do
      unless (kind == StarK) $
        throwError $ SrcTypeIncorrectKind srcType StarK kind
      t1 <- srcTypeToTypeWithTypeVars typeVars st1
      t2 <- srcTypeToTypeWithTypeVars typeVars st2
      return $ TyArrow t1 t2

    SrcTyForAll typeVar st -> do
      -- It is important to check in all these places, since it can shadow a
      -- type or another type variable and typeVars is not queried by
      -- 'isTypeDefinedM' and 'isTypeVarBoundM'
      whenM (isTypeDefinedM $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      isTyVarBound <- isTypeVarBoundM typeVar
      when (isTyVarBound || typeVar `Set.member` typeVars) $
        throwError $ TypeVarShadowsTypeVar typeVar
      -- forall type extends a set of type variables which are in scope
      let typeVars' = Set.insert typeVar typeVars
      t <- srcTypeToTypeWithTypeVarsOfKind typeVars' kind st
      return $ TyForAll typeVar t

    SrcTyApp st1 st2 ->
      case st1 of
        SrcTyTypeCon typeName -> do
          -- Type constructor is on the left-hand side of the application, it
          -- should have extra * in the kind (on the left) in order for the
          -- type to be well-kinded.
          t1 <- srcTypeToTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) st1

          -- Type variables are always of kind *, so they cannot be applied
          when (isTypeVar t1) $
            throwError $ TypeVarApp (typeNameToTypeVar typeName)

          -- Start from kind *, it is another type constructor (another
          -- application)
          t2 <- srcTypeToTypeWithTypeVars typeVars st2
          return $ TyApp t1 t2

        SrcTyApp {} -> do
          -- One more application on the left, add * to the kind
          t1 <-
            -- Try monadic type first
            catchError (TyMonad <$> srcMonadTypeToTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) st1)
              (\_ -> srcTypeToTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) st1)

          -- Start from kind *, it is another type constructor (another
          -- application)
          t2 <- srcTypeToTypeWithTypeVars typeVars st2
          return $ TyApp t1 t2

        SrcTyMonadCons {} -> do
          -- Monad is on the left-hand side of the application, it
          -- should have extra * in the kind (on the left) in order for
          -- the type to be well-kinded
          t1 <- srcTypeToTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) st1

          -- Start from kind *, it is another type constructor (another
          -- application)
          t2 <- srcTypeToTypeWithTypeVars typeVars st2
          return $ TyApp t1 t2

        -- Nothing else can be applied
        _ -> throwError $ IllFormedSrcType srcType

    SrcTyTuple sts -> do
      unless (kind == StarK) $
        throwError $ SrcTypeIncorrectKind srcType StarK kind
      ts <- mapM (srcTypeToTypeWithTypeVars typeVars) sts
      return $ TyTuple ts

    SrcTyMonadCons st1 st2 -> do
      -- Monadic types have kind * -> *
      unless (kind == mkKind 1) $
        throwError $ SrcTypeIncorrectKind srcType (mkKind 1) kind
      mt1 <- srcMonadTypeToTypeWithTypeVarsOfKind typeVars (mkKind 1) st1
      case mt1 of
        MTyMonad m1 -> do
          mt2 <- srcMonadTypeToTypeWithTypeVarsOfKind typeVars (mkKind 1) st2
          return $ TyMonad (MTyMonadCons m1 mt2)
        -- Monad cons can contain only simple monads on the left
        MTyMonadCons {} -> throwError $ MonadConsOnTheLeft srcType

-- | Transforms source representation of a monadic type to an internal one.
--
-- Checks if the type is well-formed, uses types in scope, has a kind
-- * => * and that all nested types are well-kinded.
--
-- For details see 'srcMonadTypeToTypeWithTypeVarsOfKind'.
srcMonadTypeToType :: SrcType -> TypeCheckM MonadType
srcMonadTypeToType = srcMonadTypeToTypeWithTypeVarsOfKind Set.empty (mkKind 1)

-- | Transforms source representation of a monadic type to an internal one.
--
-- Checks if the type is well-formed, uses types in scope, has a given kind and
-- that all nested types are well-kinded.
--
-- Takes a set of type variables which are already in scope (used for type
-- parameters in data type definitions and forall types).
--
-- For details see inline comments.
srcMonadTypeToTypeWithTypeVarsOfKind :: Set.Set TypeVar -> Kind -> SrcType -> TypeCheckM MonadType
srcMonadTypeToTypeWithTypeVarsOfKind typeVars kind srcType =
  case srcType of
    SrcTyTypeCon typeName ->
      -- Built-in monads are represented by type constructors
      if isBuiltInMonad typeName
        then do
          let monadKind = getBuiltInMonadKind typeName
          when (monadKind /= kind) $
            throwError $ TypeConIncorrectApp typeName monadKind kind
          return $ MTyMonad $ SinMonad (mkBuiltInMonad typeName)
        else throwError $ NotMonadicType (TyTypeCon typeName)

    SrcTyApp st1 st2 -> do
      -- One more application on the left, add * to the kind.
      (MTyMonad m1) <- srcMonadTypeToTypeWithTypeVarsOfKind typeVars (StarK :=>: kind) st1

      -- Start from kind *, it is another type constructor (another
      -- application)
      t2 <- srcTypeToTypeWithTypeVars typeVars st2
      return $ MTyMonad $ SinMonadApp m1 t2

    SrcTyMonadCons st1 st2 -> do
      -- Monadic types have kind * -> *
      (MTyMonad m1) <- srcMonadTypeToTypeWithTypeVarsOfKind typeVars (mkKind 1) st1
      mt2 <- srcMonadTypeToTypeWithTypeVarsOfKind typeVars (mkKind 1) st2
      return $ MTyMonadCons m1 mt2

    -- Only types of the shapes above can represent monads
    _ -> throwError $ NotMonadicSrcType srcType

