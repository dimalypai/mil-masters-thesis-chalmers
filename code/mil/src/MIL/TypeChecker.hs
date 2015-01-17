-- | MIL type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
--
-- Produces a typed representation of a program and a type environment.
--
-- TODO: Look carefully at all alphaEq usages. Maybe something more (with
-- monads) is needed.
module MIL.TypeChecker
  ( typeCheck
  , TypeEnv
  , TcError
  , prPrint
  ) where

import qualified Data.Set as Set
import Control.Applicative
import Data.Maybe (listToMaybe)

import MIL.AST
import MIL.AST.TypeAnnotated
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TypeEnv
import MIL.TypeChecker.TcError
import MIL.TypeChecker.Common
import MIL.TypeChecker.SrcTypeHelpers
import MIL.TypeChecker.Helpers
import MIL.TypeChecker.AlphaEq
import MIL.Utils

-- | Main entry point to the TypeChecker.
typeCheck :: SrcProgram -> Either TcError (TyProgram, TypeEnv)
typeCheck srcProgram = runTypeCheckM (tcProgram srcProgram) initTypeEnv

-- | Entry point into the type checking of the program.
tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram (Program (srcTypeDefs, srcFunDefs)) = do
  collectDefs
    srcTypeDefs collectTypeDef
    srcFunDefs collectFunDef
  -- Type parameters and function types have been checked.
  -- Now first information about definitions is in the environment:
  -- + type names and their kinds
  -- + function names and their types
  -- Source types in these places are transformed.
  checkMain
  tyTypeDefs <- mapM tcTypeDef srcTypeDefs
  tyFunDefs <- mapM tcFunDef srcFunDefs
  return $ Program (tyTypeDefs, tyFunDefs)

-- | Checks if the function is already defined.
-- Transforms a function type from source to internal representation.
-- Checks that the specified function type is correct (well-formed,
-- well-kinded and uses types in scope).
-- Adds the function and its type to the environment.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef funName srcFunType _) = do
  whenM (isFunctionDefinedM funName) $
    throwError $ FunctionAlreadyDefined funName
  funType <- srcTypeToType srcFunType
  addFunctionM funName funType

-- | Checks data constructors and adds them to the environment together with
-- their function types.
-- Checks that type parameters don't shadow types.
-- Data constructors are checked with type parameters in scope.
tcTypeDef :: SrcTypeDef -> TypeCheckM TyTypeDef
tcTypeDef (TypeDef typeName typeVars srcConDefs) = do
  checkShadowing typeVars
  tyConDefs <- mapM (tcConDef typeName typeVars) srcConDefs
  return $ TypeDef typeName typeVars tyConDefs

-- | Checks that constructor fields are correct (well-formed, well-kinded and
-- use types in scope).
-- Transforms constructor fields from source to internal type representation.
-- Constructs a function type for the data constructor.
-- Adds the constructor with its type to the environment.
tcConDef :: TypeName -> [TypeVar] -> SrcConDef -> TypeCheckM TyConDef
tcConDef typeName typeVars (ConDef conName srcConFields) = do
  whenM (isDataConDefinedM conName) $
    throwError $ ConAlreadyDefined conName
  tyConFields <- mapM (srcTypeToTypeWithTypeVars $ Set.fromList typeVars) srcConFields
  let conResultType = tyAppFromList typeName typeVars
      conArrType = tyArrowFromList conResultType tyConFields
      conType = tyForAllFromList conArrType typeVars
  addDataConM conName conType typeName
  return $ ConDef conName tyConFields

-- | Checks that the type of the body is consistent with the specified function
-- type.
-- TODO: dependency analysis, NonTerm? Recursion does not necessarily mean non termination?
-- But it would be safe.
tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef funName srcFunType srcBodyExpr) = do
  tyBodyExpr <- tcExpr srcBodyExpr
  funType <- srcTypeToType srcFunType
  let bodyType = getTypeOf tyBodyExpr
  unless (bodyType `isCompatibleWith` funType) $
    throwError $ FunBodyIncorrectType funName funType bodyType
  return $ FunDef funName funType tyBodyExpr

-- | Expression type checking.
-- Returns typed expression.
tcExpr :: SrcExpr -> TypeCheckM TyExpr
tcExpr expr =
  case expr of
    LitE lit -> return $ LitE lit

    VarE var -> do
      -- it can be both a local variable and a global function
      unlessM (isVarBoundM var) $
        throwError $ VarNotBound var
      varType <- getVarTypeM var
      return $ VarE (VarBinder (var, varType))

    LambdaE srcVarBinder srcBodyExpr -> do
      let var = getBinderVar srcVarBinder
      whenM (isVarBoundM var) $
        throwError $ VarShadowing var
      varType <- srcTypeToType (getBinderType srcVarBinder)
      -- Extend local type environment with the variable introduced by the
      -- lambda.
      -- This is safe, since we ensure above that all variable and function
      -- names are distinct.
      -- Perform the type checking of the body in this extended environment.
      let localTypeEnv = addLocalVar var varType emptyLocalTypeEnv
      tyBodyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)
      return $ LambdaE (VarBinder (var, varType)) tyBodyExpr

    AppE srcAppExpr srcArgExpr -> do
      tyAppExpr <- tcExpr srcAppExpr
      tyArgExpr <- tcExpr srcArgExpr
      let appType = getTypeOf tyAppExpr
          argType = getTypeOf tyArgExpr
      case appType of
        TyArrow paramType _resultType ->
          if not (argType `isCompatibleWith` paramType)
            then throwError $ IncorrectFunArgType paramType argType
            else return $ AppE tyAppExpr tyArgExpr
        _ -> throwError $ NotFunctionType appType

    TypeLambdaE typeVar srcBodyExpr -> do
      -- it is important to check in all these places, since it can shadow a
      -- type or another type variable
      whenM (isTypeDefinedM $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      whenM (isTypeVarBoundM typeVar) $
        throwError $ TypeVarShadowsTypeVar typeVar
      -- Extend local type environment with the type variable introduced by the
      -- type lambda.
      -- This is safe, since we ensure that all type variables and type names
      -- in scope are distinct.
      -- Perform the type checking of the body in this extended environment.
      let localTypeEnv = addLocalTypeVar typeVar emptyLocalTypeEnv
      tyBodyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)
      return $ TypeLambdaE typeVar tyBodyExpr

    TypeAppE srcAppExpr srcTypeArg -> do
      -- Type application can be performed only with forall on the left-hand
      -- side.
      -- We replace free occurences of the type variable bound by the forall
      -- inside its body with the right-hand side type.
      tyAppExpr <- tcExpr srcAppExpr
      let appType = getTypeOf tyAppExpr
      case appType of
        TyForAll {} -> do
          typeArg <- srcTypeToType srcTypeArg
          return $ TypeAppE tyAppExpr typeArg
        _ -> throwError $ NotForallTypeApp appType

    ConNameE conName _ -> do
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined conName
      conType <- getDataConTypeM conName
      return $ ConNameE conName conType

    LetE srcVarBinder srcBindExpr srcBodyExpr -> do
      let var = getBinderVar srcVarBinder
      whenM (isVarBoundM var) $
        throwError $ VarShadowing var

      varType <- srcTypeToType (getBinderType srcVarBinder)

      tyBindExpr <- tcExpr srcBindExpr
      -- Extend local type environment with the variable introduced by the
      -- bind.
      -- This is safe, since we ensure above that all variable and function
      -- names are distinct.
      let localTypeEnv = addLocalVar var varType emptyLocalTypeEnv
      tyBodyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)

      let bindExprType = getTypeOf tyBindExpr
          bodyExprType = getTypeOf tyBodyExpr

      unless (isMonadicExpr tyBindExpr) $
        throwError $ ExprHasNonMonadicType bindExprType
      unless (isMonadicExpr tyBodyExpr) $
        throwError $ ExprHasNonMonadicType bodyExprType

      let tyVarBinder = VarBinder (var, varType)
      checkMonadicBinding tyBindExpr tyVarBinder

      let bindMonadType = getMonadTypeFromApp bindExprType
      let bodyMonadType = getMonadTypeFromApp bodyExprType
      unless (bodyMonadType `isCompatibleMonadWith` bindMonadType) $
        throwError $ IncorrectMonad bindMonadType bodyMonadType

      return $ LetE tyVarBinder tyBindExpr tyBodyExpr

    ReturnE st srcRetExpr -> do
      tyRetExpr <- tcExpr srcRetExpr
      mt <- srcMonadTypeToType st
      return $ ReturnE mt tyRetExpr

    LiftE srcExpr st1 st2 -> do
      tyExpr <- tcExpr srcExpr
      unless (isMonadicExpr tyExpr) $
        throwError $ ExprHasNonMonadicType (getTypeOf tyExpr)

      mt1 <- srcMonadTypeToType st1
      mt2 <- srcMonadTypeToType st2

      let exprMonadType = getMonadTypeFromApp (getTypeOf tyExpr)
      unless (exprMonadType `alphaEq` mt1) $
        throwError $ IncorrectMonad mt1 exprMonadType

      unless (mt1 `isMonadSuffixOf` mt2) $
        throwError $ IncorrectLifting mt1 mt2

      return $ LiftE tyExpr mt1 mt2

    CaseE srcScrutExpr srcCaseAlts -> do
      tyScrutExpr <- tcExpr srcScrutExpr
      tyCaseAlts <- tcCaseAlts (getTypeOf tyScrutExpr) srcCaseAlts
      return $ CaseE tyScrutExpr tyCaseAlts

    TupleE srcElems -> TupleE <$> mapM tcExpr srcElems

-- | Takes a scrutinee (an expression we are pattern matching on) type and a
-- list of source case alternatives (which is not empty).
tcCaseAlts :: Type -> [SrcCaseAlt] -> TypeCheckM [TyCaseAlt]
tcCaseAlts scrutType srcCaseAlts = do
  tyCaseAlts <- mapM (tcCaseAlt scrutType) srcCaseAlts
  -- There is at least one case alternative and all types should be the same.
  let caseExprType = getTypeOf (head tyCaseAlts)
  let mCaseAltWithIncorrectType =
        listToMaybe $
          filter (\tyCaseAlt -> not (getTypeOf tyCaseAlt `alphaEq` caseExprType))
            tyCaseAlts
  case mCaseAltWithIncorrectType of
    Just caseAltWithIncorrectType ->
      throwError $ CaseAltIncorrectType caseExprType (getTypeOf caseAltWithIncorrectType)
    Nothing -> return ()
  return tyCaseAlts

-- | Takes a scrutinee type and a source case alternative, type checkes a
-- pattern against the scrutinee type and type checks the alternative's body
-- with variables bound by the pattern added to the local type environment.
tcCaseAlt :: Type -> SrcCaseAlt -> TypeCheckM TyCaseAlt
tcCaseAlt scrutType (CaseAlt (srcPat, srcExpr)) = do
  (tyPat, localTypeEnv) <- tcPattern scrutType srcPat emptyLocalTypeEnv
  tyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcExpr)
  return $ CaseAlt (tyPat, tyExpr)

-- | Takes a scrutinee type, a source pattern and a local type environment.
-- Type checks the pattern against the scrutinee type and returns a typed
-- pattern and an extended local type environment (with variables bound by the
-- pattern).
-- The most interesting cases are 'ConP' and 'TupleP'.
tcPattern :: Type -> SrcPattern -> LocalTypeEnv -> TypeCheckM (TyPattern, LocalTypeEnv)
tcPattern scrutType srcPat localTypeEnv =
  case srcPat of
    LitP lit -> do
      let litType = getTypeOf lit
      unless (litType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType litType
      return (LitP lit, localTypeEnv)

    VarP srcVarBinder -> do
      let (VarBinder (var, srcVarType)) = srcVarBinder
      isBound <- isVarBoundM var
      -- It is important to check in both places, since localTypeEnv is not
      -- queried by 'isVarBoundM'.
      when (isBound || isVarInLocalEnv var localTypeEnv) $
        throwError $ VarShadowing var
      varType <- srcTypeToType srcVarType
      unless (varType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType varType
      return (VarP $ VarBinder (var, varType), addLocalVar var varType localTypeEnv)

    ConP conName srcVarBinders -> do
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined conName

      conType <- getDataConTypeM conName
      conTypeName <- getDataConTypeNameM conName
      (scrutTypeName, scrutTypeArgs) <- transformScrutType scrutType conType
      when (conTypeName /= scrutTypeName) $
        throwError $ PatternIncorrectType scrutType conType

      let conFieldTypes = conFieldTypesFromType conType scrutTypeArgs
      when (length srcVarBinders /= length conFieldTypes) $
        throwError $ ConPatternIncorrectNumberOfFields (length conFieldTypes) (length srcVarBinders)

      -- Constructor field patterns are checked as variable patterns.
      -- Typed representations are built backwards for performance reasons.
      (revTyVarBinders, localTypeEnv') <-
        foldM (\(revTyVBs, localTyEnv) (svb, fieldType) -> do
          (VarP tvb, localTyEnv') <- tcPattern fieldType (VarP svb) localTyEnv
          return (tvb : revTyVBs, localTyEnv'))
        ([], localTypeEnv) (zip srcVarBinders conFieldTypes)
      return (ConP conName (reverse revTyVarBinders), localTypeEnv')

    TupleP srcVarBinders -> do
      unless (isTupleType scrutType) $
        throwError $ PatternTupleType scrutType

      let (TyTuple elemTypes) = scrutType
      when (length srcVarBinders /= length elemTypes) $
        throwError $ TuplePatternIncorrectNumberOfElements (length elemTypes) (length srcVarBinders)

      -- Tuple element patterns are checked as variable patterns.
      -- Typed representations are built backwards for performance reasons.
      (revTyVarBinders, localTypeEnv') <-
        foldM (\(revTyVBs, localTyEnv) (svb, elemType) -> do
          (VarP tvb, localTyEnv') <- tcPattern elemType (VarP svb) localTyEnv
          return (tvb : revTyVBs, localTyEnv'))
        ([], localTypeEnv) (zip srcVarBinders elemTypes)
      return (TupleP (reverse revTyVarBinders), localTypeEnv')

    DefaultP -> return (DefaultP, localTypeEnv)

