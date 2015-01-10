-- | MIL lint module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
--
-- The main goal is to construct a type environment from a typed program
-- and to ensure that the program is well-typed. Can help to ensure that
-- transformations preserve typing.
--
-- TODO: Look carefully at all alphaEq usages. Maybe something more (with
-- monads) is needed.
module MIL.LintChecker
  ( lintCheck
  , TcError
  , TypeEnv
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
import MIL.TypeChecker.Helpers
import MIL.TypeChecker.AlphaEq
import MIL.BuiltIn
import MIL.Utils

-- | Main entry point to the LintChecker.
lintCheck :: TyProgram -> Either TcError TypeEnv
lintCheck program = snd <$> runTypeCheckM (lcProgram program) initTypeEnv

-- | Entry point into the lint checking of the program.
lcProgram :: TyProgram -> TypeCheckM ()
lcProgram (Program (typeDefs, funDefs)) = do
  collectDefs
    typeDefs collectTypeDef
    funDefs collectFunDef
  -- Type parameters and function types have been checked.
  -- Now first information about definitions is in the environment:
  -- + type names and their kinds
  -- + function names and their types
  checkMain
  mapM_ lcTypeDef typeDefs
  mapM_ lcFunDef funDefs

-- | Checks if the function is already defined.
-- Checks that the specified function type is correct (well-formed,
-- well-kinded and uses types in scope).
-- Adds the function and its type to the environment.
collectFunDef :: TyFunDef -> TypeCheckM ()
collectFunDef (FunDef funName funType _) = do
  whenM (isFunctionDefinedM funName) $
    throwError $ FunctionAlreadyDefined funName
  checkType funType
  addFunctionM funName funType

-- | Checks data constructors and adds them to the environment together with
-- their function types.
-- Checks that type parameters don't shadow types.
-- Data constructors are checked with type parameters in scope.
lcTypeDef :: TyTypeDef -> TypeCheckM ()
lcTypeDef (TypeDef typeName typeVars conDefs) = do
  checkShadowing typeVars
  mapM_ (lcConDef typeName typeVars) conDefs

-- | Checks that constructor fields are correct (well-formed, well-kinded and
-- use types in scope).
-- Constructs a function type for the data constructor.
-- Adds the constructor with its type to the environment.
lcConDef :: TypeName -> [TypeVar] -> TyConDef -> TypeCheckM ()
lcConDef typeName typeVars (ConDef conName conFields) = do
  whenM (isDataConDefinedM conName) $
    throwError $ ConAlreadyDefined conName
  mapM_ (checkTypeWithTypeVars $ Set.fromList typeVars) conFields
  let conResultType = tyAppFromList typeName typeVars
      conArrType = tyArrowFromList conResultType conFields
      conType = tyForAllFromList conArrType typeVars
  addDataConM conName conType typeName

-- | Checks that the type of the body is consistent with the specified function
-- type.
-- TODO: dependency analysis, NonTerm
lcFunDef :: TyFunDef -> TypeCheckM ()
lcFunDef (FunDef funName funType bodyExpr) = do
  lcExpr bodyExpr
  let bodyType = getTypeOf bodyExpr
  unless (bodyType `alphaEq` funType) $
    throwError $ FunBodyIncorrectType funName funType bodyType

-- | Expression lint checking.
lcExpr :: TyExpr -> TypeCheckM ()
lcExpr expr =
  case expr of
    LitE {} -> return ()

    VarE varBinder -> do
      let var = getBinderVar varBinder
      -- it can be both a local variable and a global function
      unlessM (isVarBoundM var) $
        throwError $ VarNotBound var
      varType <- getVarTypeM var
      let binderType = getTypeOf varBinder
      checkType binderType
      unless (binderType `alphaEq` varType) $
        throwError $ VarIncorrectType var varType binderType

    LambdaE varBinder bodyExpr -> do
      let var = getBinderVar varBinder
      whenM (isVarBoundM var) $
        throwError $ VarShadowing var
      let varType = getTypeOf varBinder
      checkType varType
      -- Extend local type environment with the variable introduced by the
      -- lambda.
      -- This is safe, since we ensure above that all variable and function
      -- names are distinct.
      -- Perform the type checking of the body in this extended environment.
      let localTypeEnv = addLocalVar var varType emptyLocalTypeEnv
      locallyWithEnvM localTypeEnv (lcExpr bodyExpr)

    AppE appExpr argExpr -> do
      lcExpr appExpr
      lcExpr argExpr
      let appType = getTypeOf appExpr
          argType = getTypeOf argExpr
      case appType of
        TyArrow paramType _resultType ->
          unless (argType `alphaEq` paramType) $
            throwError $ IncorrectFunArgType paramType argType
        _ -> throwError $ NotFunctionType appType

    TypeLambdaE typeVar bodyExpr -> do
      -- it is important to check in all these places, since it can shadow a
      -- type, type alias or another type variable
      whenM (isTypeDefinedM $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      whenM (isTypeVarBoundM typeVar) $
        throwError $ TypeVarShadowsTypeVar typeVar
      -- Extend local type environment with the type variable introduced by the
      -- type lambda.
      -- This is safe, since we ensure that all type variables and type names
      -- in scope are distinct.
      -- Perform the lint checking of the body in this extended environment.
      let localTypeEnv = addLocalTypeVar typeVar emptyLocalTypeEnv
      locallyWithEnvM localTypeEnv (lcExpr bodyExpr)

    TypeAppE appExpr typeArg -> do
      -- Type application can be performed only with forall on the left-hand
      -- side.
      -- We replace free occurences of the type variable bound by the forall
      -- inside its body with the right-hand side type.
      lcExpr appExpr
      let appType = getTypeOf appExpr
      case appType of
        TyForAll {} -> checkType typeArg
        _ -> throwError $ NotForallTypeApp appType

    -- See Note [Data constructor type checking].
    ConNameE conName conType -> do
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined conName
      dataConType <- getDataConTypeM conName
      unless (conType `alphaEq` dataConType) $
        throwError $ ConIncorrectType conName dataConType conType

    LetE varBinder bindExpr bodyExpr -> do
      let var = getBinderVar varBinder
          varType = getBinderType varBinder
      whenM (isVarBoundM var) $
        throwError $ VarShadowing var

      checkType varType

      lcExpr bindExpr
      -- Extend local type environment with the variable introduced by the
      -- bind.
      -- This is safe, since we ensure above that all variable and function
      -- names are distinct.
      let localTypeEnv = addLocalVar var varType emptyLocalTypeEnv
      locallyWithEnvM localTypeEnv (lcExpr bodyExpr)

      unless (isMonadicExpr bindExpr) $
        throwError $ ExprHasNonMonadicType (getTypeOf bindExpr)
      unless (isMonadicExpr bodyExpr) $
        throwError $ ExprHasNonMonadicType (getTypeOf bodyExpr)

{-
    LetE varBinder bindExpr bodyExpr -> do
      bindExprTm <-
        case bindExprType of
          TyApp (TyMonad tm) a -> do
            unlessM (a `alphaEq` varType) $
              throwError $ IncorrectExprType (TyApp (TyMonad tm) varType) bindExprType
            return tm
          _ -> throwError $ ExprHasNonMonadicType bindExprType
      case bodyType of
        TyApp (TyMonad bodyExprTm) bodyResultType -> do
          unlessM (compatibleMonadTypes bodyExprTm bindExprTm) $
            throwError $ IncorrectMonad bindExprTm bodyExprTm
          ifM (bindExprTm `hasMoreEffectsThan` bodyExprTm)
            (return $ TyApp (TyMonad bindExprTm) bodyResultType)
            (return $ TyApp (TyMonad bodyExprTm) bodyResultType)
        _ -> throwError $ ExprHasNonMonadicType bodyType
-}

    ReturnE mt retExpr -> do
      checkMonadType mt
      lcExpr retExpr

{-
    LiftE e tm1 tm2 -> do
      checkTypeM tm1
      checkTypeM tm2
      -- TODO: not really suffix? just somewhere inside?
      unlessM (tm1 `isMonadSuffixOf` tm2) $
        throwError $ IncorrectLifting tm1 tm2
      eType <- tcExpr e
      let (TyApp (TyMonad eMonad) eMonadResultType) = eType
      -- TODO: something more than alphaEq?
      unlessM (eMonad `alphaEq` tm1) $
        throwError $ OtherError "Incorrect lifting"
      return $ TyApp (TyMonad tm2) eMonadResultType

    LetRecE bindings bodyExpr -> undefined
-}

    CaseE scrutExpr caseAlts -> do
      lcExpr scrutExpr
      lcCaseAlts (getTypeOf scrutExpr) caseAlts

    TupleE elems -> mapM_ lcExpr elems

-- | Takes a scrutinee (an expression we are pattern matching on) type and a
-- list of case alternatives (which is not empty).
lcCaseAlts :: Type -> [TyCaseAlt] -> TypeCheckM ()
lcCaseAlts scrutType caseAlts = do
  mapM_ (lcCaseAlt scrutType) caseAlts
  -- There is at least one case alternative and all types should be the same.
  let caseExprType = getTypeOf (head caseAlts)
  -- TODO: more than alphaEq: monad prefix
  let mCaseAltWithIncorrectType =
        listToMaybe $
          filter (\caseAlt -> not (getTypeOf caseAlt `alphaEq` caseExprType))
            caseAlts
  case mCaseAltWithIncorrectType of
    Just caseAltWithIncorrectType ->
      throwError $ CaseAltIncorrectType caseExprType (getTypeOf caseAltWithIncorrectType)
    Nothing -> return ()

-- | Takes a scrutinee type and a case alternative, lint checkes a pattern
-- against the scrutinee type and lint checks the alternative's body with
-- variables bound by the pattern added to the local type environment.
lcCaseAlt :: Type -> TyCaseAlt -> TypeCheckM ()
lcCaseAlt scrutType (CaseAlt (pat, expr)) = do
  localTypeEnv <- lcPattern scrutType pat emptyLocalTypeEnv
  locallyWithEnvM localTypeEnv (lcExpr expr)

-- | Takes a scrutinee type, a pattern and a local type environment.
-- Lint checks the pattern against the scrutinee type and returns an extended
-- local type environment (with variables bound by the pattern).
-- The most interesting cases are 'ConP' and 'TupleP'.
lcPattern :: Type -> TyPattern -> LocalTypeEnv -> TypeCheckM LocalTypeEnv
lcPattern scrutType pat localTypeEnv =
  case pat of
    LitP lit -> do
      let litType = getTypeOf lit
      unless (litType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType litType
      return localTypeEnv

    VarP varBinder -> do
      let (VarBinder (var, varType)) = varBinder
      isBound <- isVarBoundM var
      -- It is important to check in both places, since localTypeEnv is not
      -- queried by 'isVarBoundM'.
      when (isBound || isVarInLocalEnv var localTypeEnv) $
        throwError $ VarShadowing var
      checkType varType
      unless (varType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType varType
      return $ addLocalVar var varType localTypeEnv

    ConP conName varBinders -> do
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined conName

      conType <- getDataConTypeM conName
      conTypeName <- getDataConTypeNameM conName
      (scrutTypeName, scrutTypeArgs) <- transformScrutType scrutType conType
      when (conTypeName /= scrutTypeName) $
        throwError $ PatternIncorrectType scrutType conType

      let conFieldTypes = conFieldTypesFromType conType scrutTypeArgs
      when (length varBinders /= length conFieldTypes) $
        throwError $ ConPatternIncorrectNumberOfFields (length conFieldTypes) (length varBinders)

      -- Constructor field patterns are checked as variable patterns.
      foldM (\localTyEnv (vb, fieldType) -> lcPattern fieldType (VarP vb) localTyEnv)
        localTypeEnv (zip varBinders conFieldTypes)

    TupleP varBinders -> do
      unless (isTupleType scrutType) $
        throwError $ PatternTupleType scrutType

      let (TyTuple elemTypes) = scrutType
      when (length varBinders /= length elemTypes) $
        throwError $ TuplePatternIncorrectNumberOfElements (length elemTypes) (length varBinders)

      -- Tuple element patterns are checked as variable patterns.
      foldM (\localTyEnv (vb, elemType) -> lcPattern elemType (VarP vb) localTyEnv)
        localTypeEnv (zip varBinders elemTypes)

    DefaultP -> return localTypeEnv

-- | Note [Data constructor lint checking]:
--
-- The first intention is to perform 'checkType' on the type a data constructor
-- is annotated with. But there is a problem with type variables scope.
-- Function types of the data constructors from polymorphic data types contain
-- forall types. If there is a type lambda and an occurence of such a data
-- constructor inside it and there is a clash between type variable names,
-- 'checkType' will throw a shadowing error.
-- It seems to be safe to not perform this check, since later we do a check for
-- alpha-equivalence against the constructor type from the environment, and so
-- incorrectly annotated constructors will be caught.

