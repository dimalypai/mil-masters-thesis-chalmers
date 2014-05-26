-- | Type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
--
-- Type checking produces a type environment with information about global
-- definitions and annotates local variable occurences with their types. So,
-- when we say type checked and it is applicable to annotate something it also
-- means `annotated`.
module FunLang.TypeChecker
  ( typeCheck
  , typeCheckStage
  , typeOf
  , TypeEnv
  , initTypeEnv
  , TcError
  , prPrint
  ) where

import qualified Data.Set as Set
import Data.List (find)
import Control.Applicative ((<$>))

import FunLang.AST
import FunLang.AST.Helpers
import FunLang.TypeChecker.TypeCheckM
import FunLang.TypeChecker.TypeEnv
import FunLang.TypeChecker.TcError
import FunLang.TypeChecker.Helpers
import FunLang.SrcAnnotated
import FunLang.BuiltIn
import FunLang.Utils

-- | Main batch entry point to the TypeChecker.
-- In the case of success returns a typed program and a type environment.
typeCheck :: SrcProgram -> Either TcError (TyProgram, TypeEnv)
typeCheck srcProgram = runTypeCheckM (tcProgram srcProgram) initTypeEnv

-- | Main staging entry point to the TypeChecker.
-- Takes a type environment to begin with. May be used for adding chunks of the
-- program and type checking them together with previously type checked chunks.
-- In the case of success returns a typed program and a type environment.
typeCheckStage :: SrcProgram -> TypeEnv -> Either TcError (TyProgram, TypeEnv)
typeCheckStage srcProgram typeEnv = runTypeCheckM (tcProgram srcProgram) typeEnv

-- | Type checks a given source expression in a given type environment.
-- In the case of success - returns its type.
typeOf :: SrcExpr -> TypeEnv -> Either TcError Type
typeOf srcExpr typeEnv = fmap (snd . fst) $ runTypeCheckM (tcExpr srcExpr) typeEnv

-- | Entry point into the type checking of the program.
-- Returns a type checked program.
tcProgram :: SrcProgram -> TypeCheckM TyProgram
tcProgram (Program s typeDefs funDefs) = do
  collectDefs typeDefs funDefs
  -- Type parameters and function types have been checked.
  -- Now first information about definitions is in the environment:
  -- + type names and their kinds
  -- + function names and their types
  checkMain
  mapM_ tcTypeDef typeDefs
  tyFunDefs <- mapM tcFunDef funDefs
  return $ Program s typeDefs tyFunDefs

-- | In order to be able to handle (mutually) recursive definitions, we need to
-- do an additional first pass to collect type names with their kinds and
-- function names and type signatures.
--
-- It collects:
--
-- * type names and their kinds
--
-- * function names and their types
--
-- It also does checking of type parameters and function types.
collectDefs :: [SrcTypeDef] -> [SrcFunDef] -> TypeCheckM ()
collectDefs typeDefs funDefs = do
  -- It is essential that we collect types before functions, because function
  -- types may mention defined data types.
  mapM_ collectTypeDef typeDefs
  mapM_ collectFunDef funDefs

-- | Checks if the type is already defined.
-- Checks that all type variables are distinct.
-- Adds the type name and its kind to the type environment.
collectTypeDef :: SrcTypeDef -> TypeCheckM ()
collectTypeDef (TypeDef _ srcTypeName srcTypeVars _) = do
  let typeName = getTypeName srcTypeName
  whenM (isTypeDefinedM typeName) $
    throwError $ TypeAlreadyDefined srcTypeName
  let typeVars = map getTypeVar srcTypeVars
  foldM_ (\tvs (tv, stv) ->
            if tv `Set.member` tvs
              then throwError $ TypeParamAlreadyDefined stv
              else return $ Set.insert tv tvs)
         Set.empty (zip typeVars srcTypeVars)
  let kind = mkKind (length srcTypeVars)
  addTypeM typeName kind

-- | Checks if the function is already defined.
-- Checks that the specified function type is correct (well-formed,
-- well-kinded and uses types in scope).
-- Adds the function and its type to the environment.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef _ srcFunName funSrcType _) = do
  let funName = getFunName srcFunName
  whenM (isFunctionDefinedM funName) $
    throwError $ FunctionAlreadyDefined srcFunName
  funType <- srcTypeToType funSrcType
  addFunctionM funName funType funSrcType

-- | Program needs to have an entry point: `main : IO Unit`.
checkMain :: TypeCheckM ()
checkMain = do
  unlessM (isFunctionDefinedM $ FunName "main") $
    throwError MainNotDefined
  funTypeInfo <- getFunTypeInfoM $ FunName "main"
  let mainType = ioType unitType
  unless (ftiType funTypeInfo `alphaEq` mainType) $
    throwError $ MainIncorrectType (ftiSrcType funTypeInfo)

-- | Checks data constructors and adds them to the environment together with
-- their function types.
-- Data constructors are checked with type parameters in scope.
tcTypeDef :: SrcTypeDef -> TypeCheckM ()
tcTypeDef (TypeDef _ srcTypeName srcTypeVars srcConDefs) = do
  let typeVars = map getTypeVar srcTypeVars
  mapM_ (tcConDef (getTypeName srcTypeName) typeVars) srcConDefs

-- | Checks that constructor fields are correct (well-formed, well-kinded and
-- use types in scope).
-- Constructs a function type for the data constructor.
-- Adds the constructor with its type to the environment.
tcConDef :: TypeName -> [TypeVar] -> SrcConDef -> TypeCheckM ()
tcConDef typeName typeVars (ConDef _ srcConName srcConFields) = do
  let conName = getConName srcConName
  whenM (isDataConDefinedM conName) $
    throwError $ ConAlreadyDefined srcConName
  conFields <- mapM (srcTypeToTypeWithTypeVars $ Set.fromList typeVars) srcConFields
  let conResultType = TyApp typeName (map TyVar typeVars)
      conArrType = tyArrowFromList conResultType conFields
      conType = foldr (\tv acc -> TyForAll tv acc) conArrType typeVars
  addDataConM conName conType typeName

-- | Checks all function equations and their consistency (TODO).
-- Returns type checked function definition.
tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef s srcFunName funSrcType srcFunEqs) = do
  let funName = getFunName srcFunName
  funType <- srcTypeToType funSrcType
  tyFunEqs <- mapM (tcFunEq funName funType funSrcType) srcFunEqs
  return $ FunDef s srcFunName funSrcType tyFunEqs

-- | Checks that function equation belongs to the correct function definition.
-- Checks equation body.
-- Checks that the type of the body is consistent with the specified function
-- type.
-- Takes a function source type for error reporting.
-- Returns type checked function equation.
tcFunEq :: FunName -> Type -> SrcType -> SrcFunEq -> TypeCheckM TyFunEq
tcFunEq funName funType funSrcType (FunEq s srcFunName patterns srcBodyExpr) = do
  when (getFunName srcFunName /= funName) $
    throwError $ FunEqIncorrectName srcFunName funName
  (tyBodyExpr, bodyType) <-
    case srcBodyExpr of
      DoE ds srcStmts -> do
        unless (isMonadType funType) $
          throwError $ DoBlockNotMonad funType (getSrcSpan funSrcType)
        -- kinds have been checked, so it should not fail
        let funMonad = getMonadType funType
        (tyStmts, lastStmtType) <- tcDoBlock srcStmts funMonad
        return (DoE ds tyStmts, lastStmtType)
      _ -> tcExpr srcBodyExpr
  unless (bodyType `alphaEq` funType) $
    throwError $ FunEqBodyIncorrectType srcBodyExpr funName funType bodyType
  return $ FunEq s srcFunName [] tyBodyExpr

-- | Expression type checking.
-- Returns a type checked expression together with its type.
tcExpr :: SrcExpr -> TypeCheckM (TyExpr, Type)
tcExpr srcExpr =
  case srcExpr of
    LitE srcLit -> return (LitE srcLit, typeOfLiteral $ getLiteral srcLit)

    VarE s var -> do
      -- it can be both a local variable and a global function
      unlessM (isVarBoundM var) $
        throwError $ VarNotBound var s
      varType <- getVarTypeM var
      return (VarE s (VarTy (var, varType)), varType)

    LambdaE s varBinders srcBodyExpr -> do
      -- collect variables bound by the lambda together with their types
      (localTypeEnv, revParamTypes) <-
        foldM (\(localTyEnv, revTyList) vb -> do
            let var = getVar (getBinderVar vb)
            isBound <- isVarBoundM var
            -- it is important to check in both places, since localTyEnv
            -- is not queried by 'isVarBoundM', see `LambdaParamsDup` test case
            when (isBound || isVarBound var localTyEnv) $
              throwError $ VarShadowing (getBinderVar vb)
            varType <- srcTypeToType (getBinderType vb)
            return $ (addLocalVar var varType localTyEnv, varType : revTyList))
          (emptyLocalTypeEnv, []) varBinders

      -- extend local type environment with variables introduced by the lambda
      -- this is safe, since we ensure above that all variable and function names are distinct
      -- perform the type checking of the body in this extended environment
      (tyBodyExpr, bodyType) <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)
      let paramTypes = reverse revParamTypes
      let lambdaType = tyArrowFromList bodyType paramTypes
      return (LambdaE s varBinders tyBodyExpr, lambdaType)

    TypeLambdaE s srcTypeVars srcBodyExpr -> do
      -- collect type variables bound by the type lambda
      localTypeEnv <-
        foldM (\localTyEnv stv -> do
            let typeVar = getTypeVar stv
            -- it is important to check in all these places, since it can
            -- shadow a type or another type variable and localTyEnv is not
            -- queried by 'isTypeDefinedM' and 'isTypeVarBoundM', see
            -- `TypeLambdaParamsDup` test case
            whenM (isTypeDefinedM $ typeVarToTypeName typeVar) $
              throwError $ TypeVarShadowsType stv
            isTyVarBound <- isTypeVarBoundM typeVar
            when (isTyVarBound || isTypeVarBound typeVar localTyEnv) $
              throwError $ TypeVarShadowsTypeVar stv
            return $ addLocalTypeVar typeVar localTyEnv)
          emptyLocalTypeEnv srcTypeVars

      -- extend local type environment with type variables introduced by the type lambda
      -- this is safe, since we ensure that all type variables and type names in scope are distinct
      -- perform the type checking of the body in this extended environment
      (tyBodyExpr, bodyType) <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)
      let typeVars = map getTypeVar srcTypeVars
      let tyLambdaType = tyForAllFromList bodyType typeVars
      return (TypeLambdaE s srcTypeVars tyBodyExpr, tyLambdaType)

    TypeAppE s srcAppExpr srcArgType -> do
      -- type application can be performed only with forall on the left-hand
      -- side
      -- we replace free occurences of the type variable bound by the forall
      -- inside its body with the right-hand side type
      (tyAppExpr, appType) <- tcExpr srcAppExpr
      case appType of
        TyForAll typeVar forallBodyType -> do
          argType <- srcTypeToType srcArgType
          let resultType = (typeVar, argType) `substTypeIn` forallBodyType
          return (TypeAppE s tyAppExpr srcArgType, resultType)
        _ -> throwError $ NotForallTypeApp appType (getSrcSpan srcAppExpr)

    ConNameE srcConName -> do
      let conName = getConName srcConName
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined srcConName
      dataConTypeInfo <- getDataConTypeInfoM conName
      return (ConNameE srcConName, dcontiType dataConTypeInfo)

    CaseE s srcScrutExpr srcCaseAlts -> do
      (tyScrutExpr, scrutType) <- tcExpr srcScrutExpr
      (tyCaseAlts, caseExprType) <- tcCaseAlts scrutType srcCaseAlts
      return (CaseE s tyScrutExpr tyCaseAlts, caseExprType)

    BinOpE s srcBinOp srcExpr1 srcExpr2 -> do
      let op = getBinOp srcBinOp
      (tyExpr1, tyExpr2, resultType) <- tcBinOp op srcExpr1 srcExpr2
      return (BinOpE s srcBinOp tyExpr1 tyExpr2, resultType)

    ParenE s srcSubExpr -> do
      (tySubExpr, exprType) <- tcExpr srcSubExpr
      return (ParenE s tySubExpr, exprType)

    ThrowE s srcThrowType -> do
      throwType <- srcTypeToType srcThrowType
      return (ThrowE s srcThrowType, throwType)

    DoE {} -> error "do-block is illegal in this context"

-- | Takes a scrurinee (an expression we are pattern matching on) type and a
-- list of case alternatives (which is not empty). Returns a list of type
-- checked case alternatives and a type of their bodies and so of the whole
-- case expression (they all must agree).
tcCaseAlts :: Type -> [SrcCaseAlt] -> TypeCheckM ([TyCaseAlt], Type)
tcCaseAlts scrutType srcCaseAlts = do
  (tyCaseAlts, caseAltTypes) <- unzip <$> mapM (tcCaseAlt scrutType) srcCaseAlts
  -- There is at least one case alternative and all types should be the same
  let caseExprType = head caseAltTypes
  let mIncorrectTypeAlt = find (\(t,_) -> not (t `alphaEq` caseExprType)) (zip caseAltTypes tyCaseAlts)
  case mIncorrectTypeAlt of
    Just (incorrectAltType, caseAlt) ->
      throwError $ CaseAltIncorrectType caseExprType incorrectAltType (getSrcSpan caseAlt)
    Nothing -> return ()
  return (tyCaseAlts, caseExprType)

-- | Takes a scrutinee type and a case alternative, type checkes a pattern
-- against the scrutinee type and type checks the alternative's body with
-- variables bound by the pattern added to the local type environment.
-- Returns the type checked case alternative and its body type.
tcCaseAlt :: Type -> SrcCaseAlt -> TypeCheckM (TyCaseAlt, Type)
tcCaseAlt scrutType (CaseAlt s pat srcExpr) = do
  localTypeEnv <- tcPattern scrutType pat emptyLocalTypeEnv
  (tyExpr, exprType) <- locallyWithEnvM localTypeEnv (tcExpr srcExpr)
  return (CaseAlt s pat tyExpr, exprType)

-- | Takes a scrutinee type, a pattern and a local type environment. Type
-- checks the pattern against the scrutinee type and returns the extended local
-- type environment (with variables bound by the pattern).
-- The most interesting case is 'ConP'.
tcPattern :: Type -> SrcPattern -> LocalTypeEnv -> TypeCheckM LocalTypeEnv
tcPattern scrutType pat localTypeEnv =
  case pat of
    LitP srcLit -> do
      let litType = typeOfLiteral $ getLiteral srcLit
      unless (litType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType litType (getSrcSpan srcLit)
      return localTypeEnv

    VarP varBinder -> do
      let var = getVar (getBinderVar varBinder)
      isBound <- isVarBoundM var
      -- it is important to check in both places, since localTypeEnv is not
      -- queried by 'isVarBoundM', see `VarPatternShadowsNested` test case
      when (isBound || isVarBound var localTypeEnv) $
        throwError $ VarShadowing (getBinderVar varBinder)
      varType <- srcTypeToType (getBinderType varBinder)
      unless (varType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType varType (getSrcSpan varBinder)
      return $ addLocalVar var varType localTypeEnv

    ConP s srcConName subPats -> do
      let conName = getConName srcConName
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined srcConName
      dataConTypeInfo <- getDataConTypeInfoM conName
      let conType = dcontiType dataConTypeInfo
          conTypeName = dcontiTypeName dataConTypeInfo
      case scrutType of
        TyApp scrutTypeName scrutTypeArgs -> do
          when (conTypeName /= scrutTypeName) $
            throwError $ PatternIncorrectType scrutType conType s
          let conFieldTypes = conFieldTypesFromType conType scrutTypeArgs
          when (length subPats /= length conFieldTypes) $
            throwError $ ConPatternIncorrectNumberOfFields (length conFieldTypes) (length subPats) s
          foldM (\localTyEnv (p, fieldType) -> tcPattern fieldType p localTyEnv)
            localTypeEnv (zip subPats conFieldTypes)
        -- If the scrutinee has a type other than a type application, then this
        -- pattern can not be type correct. Data constructors have a
        -- monomorphic fully applied type constructor type.
        _ -> throwError $ PatternIncorrectType scrutType conType s

    DefaultP _ -> return localTypeEnv
    ParenP _ pat' -> tcPattern scrutType pat' localTypeEnv

-- Binary operations type checking

-- | Type checks a given binary operation. Takes operands.
-- Returns a triple of type checked operands and a type of the result.
tcBinOp :: BinOp -> SrcExpr -> SrcExpr -> TypeCheckM (TyExpr, TyExpr, Type)
tcBinOp op srcExpr1 srcExpr2 = do
  tyExpr1WithType@(tyExpr1, _) <- tcExpr srcExpr1
  tyExpr2WithType@(tyExpr2, _) <- tcExpr srcExpr2
  resultType <- case op of
    App -> tcApp tyExpr1WithType tyExpr2WithType
    Catch -> tcCatch tyExpr1WithType tyExpr2WithType
  return (tyExpr1, tyExpr2, resultType)

-- | Type synonym for binary operation type checking functions.
-- They take two pairs of type checked operands with their types and return a
-- type of the result.
type BinOpTc = (TyExpr, Type) -> (TyExpr, Type) -> TypeCheckM Type

-- | Function application type checking.
tcApp :: BinOpTc
tcApp (tyExpr1, expr1Type) (tyExpr2, expr2Type) =
  case expr1Type of
    TyArrow argType resultType ->
      if not (expr2Type `alphaEq` argType)
        then throwError $ IncorrectFunArgType tyExpr2 argType expr2Type
        else return resultType
    _ -> throwError $ NotFunctionType tyExpr1 expr1Type

tcCatch :: BinOpTc
tcCatch (_, expr1Type) (tyExpr2, expr2Type) =
  if expr1Type `alphaEq` expr2Type
    then return expr1Type
    else throwError $ IncorrectExprType expr1Type expr2Type (getSrcSpan tyExpr2)

-- | Do-block type checking.
-- Takes a monad in which it should operate.
-- Returns type checked statements together with the type of the last one (if
-- it is not bind, otherwise - throws an error).
-- There is at least one statement.
tcDoBlock :: [SrcStmt] -> Type -> TypeCheckM ([TyStmt], Type)
tcDoBlock [BindS s _ _] _ = throwError $ BindLastStmt s
tcDoBlock (srcStmt:srcStmts) funMonad = do
  case srcStmt of
    ExprS s srcExpr -> do
      (tyExpr, exprType) <- tcExpr srcExpr
      unless (isMonadType exprType) $
        throwError $ NotMonadicType funMonad exprType (getSrcSpan srcExpr)
      -- kind checking has already been done, should not fail
      let exprMonad = getMonadType exprType
      unless (exprMonad `alphaEq` funMonad) $
        throwError $ IncorrectMonad funMonad exprMonad s

      let tyStmt = ExprS s tyExpr
      (tyStmts, lastStmtType) <-
        if null srcStmts
          then return ([], exprType)
          else tcDoBlock srcStmts funMonad
      return (tyStmt : tyStmts, lastStmtType)

    BindS s varBinder srcExpr -> do
      let var = getVar (getBinderVar varBinder)
      whenM (isVarBoundM var) $
        throwError $ VarShadowing (getBinderVar varBinder)
      varType <- srcTypeToType (getBinderType varBinder)

      (tyExpr, exprType) <- tcExpr srcExpr

      unless (isMonadType exprType) $
        throwError $ NotMonadicType funMonad exprType (getSrcSpan srcExpr)
      -- kind checking has already been done, should not fail
      let exprMonad = getMonadType exprType
      unless (exprMonad `alphaEq` funMonad) $
        throwError $ IncorrectMonad funMonad exprMonad (getSrcSpan srcExpr)
      -- kind checking has already been done, should not fail
      unless (varType `alphaEq` getMonadResultType exprType) $
        throwError $
          IncorrectExprType (applyMonadType funMonad varType)
                            exprType
                            (getSrcSpan srcExpr)

      let localTypeEnv = addLocalVar var varType emptyLocalTypeEnv
      let tyStmt = BindS s varBinder tyExpr
      -- srcStmts is not empty, otherwise we would have thrown an error in the
      -- first equation
      (tyStmts, lastStmtType) <- locallyWithEnvM localTypeEnv (tcDoBlock srcStmts funMonad)
      return (tyStmt : tyStmts, lastStmtType)

    ReturnS s srcExpr -> do
      (tyExpr, exprType) <- tcExpr srcExpr
      let tyStmt = ReturnS s tyExpr
          stmtType = applyMonadType funMonad exprType
      (tyStmts, lastStmtType) <-
        if null srcStmts
          then return ([], stmtType)
          else tcDoBlock srcStmts funMonad
      return (tyStmt : tyStmts, lastStmtType)
tcDoBlock [] _ = error "do-block can't be empty"

