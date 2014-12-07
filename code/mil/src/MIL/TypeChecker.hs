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

import MIL.AST
import MIL.AST.TypeAnnotated
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TypeEnv
import MIL.TypeChecker.TcError
import MIL.TypeChecker.Common
import MIL.TypeChecker.SrcTypeHelpers
import MIL.TypeChecker.Helpers
import MIL.BuiltIn
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
-- TODO: dependency analysis, NonTerm
tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef funName srcFunType srcBodyExpr) = do
  tyBodyExpr <- tcExpr srcBodyExpr
  funType <- srcTypeToType srcFunType
  let bodyType = getTypeOf tyBodyExpr
  unlessM (bodyType `alphaEq` funType) $
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

{-
  case expr of
    AppE exprApp exprArg -> do
      appType <- tcExpr exprApp
      argType <- tcExpr exprArg
      case appType of
        TyArrow paramType resultType ->
          ifM (not <$> (argType `alphaEq` paramType))
            (throwError $ IncorrectFunArgType paramType argType)
            (return resultType)
        _ -> throwError $ NotFunctionType appType

    TypeLambdaE typeVar bodyExpr -> do
      -- it is important to check in all these places, since it can shadow a
      -- type, type alias or another type variable
      whenM (isTypeOrAliasDefinedM $ typeVarToTypeName typeVar) $
        throwError $ TypeVarShadowsType typeVar
      whenM (isTypeVarBound typeVar) $
        throwError $ TypeVarShadowsTypeVar typeVar
      -- Extend local type environment with the type variable introduced by the
      -- type lambda.
      -- This is safe, since we ensure that all type variables and type names
      -- in scope are distinct.
      -- Perform the type checking of the body in this extended environment.
      let localTypeEnv = addLocalTypeVar typeVar emptyLocalTypeEnv
      bodyType <- locallyWithEnv localTypeEnv (tcExpr bodyExpr)
      let tyLambdaType = tyForAllFromList bodyType [typeVar]
      return tyLambdaType

    TypeAppE exprApp typeArg -> do
      -- Type application can be performed only with forall on the left-hand
      -- side.
      -- We replace free occurences of the type variable bound by the forall
      -- inside its body with the right-hand side type.
      appType <- tcExpr exprApp
      case appType of
        TyForAll typeVar forallBodyType -> do
          checkType typeArg
          let resultType = (typeVar, typeArg) `substTypeIn` forallBodyType
          return resultType
        _ -> throwError $ NotForallTypeApp appType

    -- See Note [Data constructor type checking].
    ConNameE conName conType -> do
      unlessM (isDataConDefined conName) $
        throwError $ ConNotDefined conName
      dataConType <- getDataConTypeM conName
      unlessM (conType `alphaEq` dataConType) $
        throwError $ ConIncorrectType conName dataConType conType
      return conType

    LetE varBinder bindExpr bodyExpr -> do
      let var = getBinderVar varBinder
          varType = getTypeOf varBinder
      checkType varType
      bindExprType <- tcExpr bindExpr
      bindExprTm <-
        case bindExprType of
          TyApp (TyMonad tm) a -> do
            unlessM (a `alphaEq` varType) $
              throwError $ IncorrectExprType (TyApp (TyMonad tm) varType) bindExprType
            return tm
          _ -> throwError $ ExprHasNonMonadicType bindExprType
      bodyType <-
        if (var /= Var "_")
          then do whenM (isVarBound var) $
                    throwError $ VarShadowing var
                  -- Extend local type environment with the variable introduced by the
                  -- bind.
                  -- This is safe, since we ensure above that all variable and function
                  -- names are distinct.
                  -- Perform the type checking of the body in this extended environment.
                  let localTypeEnv = addLocalVar var varType emptyLocalTypeEnv
                  locallyWithEnv localTypeEnv (tcExpr bodyExpr)
          else tcExpr bodyExpr
      case bodyType of
        TyApp (TyMonad bodyExprTm) bodyResultType -> do
          unlessM (compatibleMonadTypes bodyExprTm bindExprTm) $
            throwError $ IncorrectMonad bindExprTm bodyExprTm
          ifM (bindExprTm `hasMoreEffectsThan` bodyExprTm)
            (return $ TyApp (TyMonad bindExprTm) bodyResultType)
            (return $ TyApp (TyMonad bodyExprTm) bodyResultType)
        _ -> throwError $ ExprHasNonMonadicType bodyType

    ReturnE tm retExpr -> do
      checkTypeM tm
      retExprType <- tcExpr retExpr
      return $ TyApp (TyMonad tm) retExprType

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

    CaseE scrutExpr caseAlts -> do
      scrutExprType <- tcExpr scrutExpr
      tcCaseAlts scrutExprType caseAlts

    TupleE tElems -> do
      elemTypes <- mapM tcExpr tElems
      return $ TyTuple elemTypes

-- | Takes a scrutinee (an expression we are pattern matching on) type and a
-- list of case alternatives (which is not empty). Returns a type of their
-- bodies and so of the whole case expression (they all must agree).
tcCaseAlts :: Type -> [SrcCaseAlt] -> TypeCheckM [TyCaseAlt]
tcCaseAlts scrutType caseAlts = do
  caseAltTypes <- mapM (tcCaseAlt scrutType) caseAlts
  -- There is at least one case alternative and all types should be the same
  let caseExprType = head caseAltTypes
  -- TODO: more than alphaEq: monad prefix
  mIncorrectTypeAlt <- findM (\t -> not <$> (t `alphaEq` caseExprType)) caseAltTypes
  case mIncorrectTypeAlt of
    Just incorrectAltType ->
      throwError $ CaseAltIncorrectType caseExprType incorrectAltType
    Nothing -> return ()
  return caseExprType

-- | Takes a scrutinee type and a case alternative, type checkes a pattern
-- against the scrutinee type and type checks the alternative's body with
-- variables bound by the pattern added to the local type environment.
-- Returns the type of the case alternative body.
tcCaseAlt :: Type -> SrcCaseAlt -> TypeCheckM TyCaseAlt
tcCaseAlt scrutType (CaseAlt (pat, expr)) = do
  localTypeEnv <- tcPattern scrutType pat emptyLocalTypeEnv
  locallyWithEnv localTypeEnv (tcExpr expr)

-- | Takes a scrutinee type, a pattern and a local type environment. Type
-- checks the pattern against the scrutinee type and returns an extended local
-- type environment (with variables bound by the pattern).
-- The most interesting case is 'ConP'.
tcPattern :: Type -> SrcPattern -> LocalTypeEnv -> TypeCheckM (TyPattern, LocalTypeEnv)
tcPattern scrutType pat localTypeEnv =
  case pat of
    LitP lit -> do
      let litType = getTypeOf lit
      unlessM (litType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType litType
      return localTypeEnv

    VarP varBinder -> do
      let (VarBinder (var, varType)) = varBinder
      isBound <- isVarBound var
      -- it is important to check in both places, since localTypeEnv is not
      -- queried by 'isVarBound'
      when (isBound || isVarInLocalEnv var localTypeEnv) $
        throwError $ VarShadowing var
      unlessM (varType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType varType
      return $ addLocalVar var varType localTypeEnv

    ConP conName varBinders -> do
      unlessM (isDataConDefined conName) $
        throwError $ ConNotDefined conName
      conType <- getDataConTypeM conName
      conTypeName <- getDataConTypeNameM conName
      (scrutTypeName, scrutTypeArgs) <- transformScrutType scrutType conType
      when (conTypeName /= scrutTypeName) $
        throwError $ PatternIncorrectType scrutType conType
      let conFieldTypes = conFieldTypesFromType conType scrutTypeArgs
      when (length varBinders /= length conFieldTypes) $
        throwError $ ConPatternIncorrectNumberOfFields (length conFieldTypes) (length varBinders)
      foldM (\localTyEnv (vb, fieldType) -> tcPattern fieldType (VarP vb) localTyEnv)
        localTypeEnv (zip varBinders conFieldTypes)

    DefaultP -> return localTypeEnv
-}
-- | Note [Data constructor type checking]:
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

