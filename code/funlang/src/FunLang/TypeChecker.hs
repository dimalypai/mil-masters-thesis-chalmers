-- | Type checking module. Most of the functions follow the AST structure.
-- Built using the API from 'TypeCheckM'.
--
-- Type checking produces a type environment with information about global
-- definitions and annotates some syntax nodes with their types. So, when we
-- say type checked and it is applicable to annotate something it also means
-- `annotated`.
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
import FunLang.AST.SrcAnnotated
import FunLang.AST.TypeAnnotated
import FunLang.TypeChecker.TypeCheckM
import FunLang.TypeChecker.TypeEnv
import FunLang.TypeChecker.TcError
import FunLang.TypeChecker.Helpers
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
typeOf srcExpr typeEnv = fmap (getTypeOf . fst) $ runTypeCheckM (tcExpr srcExpr) typeEnv

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
  tyBodyExpr <-
    case srcBodyExpr of
      DoE ds _ srcStmts -> do
        unless (isMonadType funType) $
          throwError $ DoBlockNotMonad funType (getSrcSpan funSrcType)
        -- kinds have been checked, so it should not fail
        let funMonad = getMonadType funType
        (tyStmts, lastStmtType) <- tcDoBlock srcStmts funMonad
        return $ DoE ds lastStmtType tyStmts
      _ -> tcExpr srcBodyExpr
  let bodyType = getTypeOf tyBodyExpr
  unless (bodyType `alphaEq` funType) $
    throwError $ FunEqBodyIncorrectType srcBodyExpr funName funType bodyType
  return $ FunEq s srcFunName [] tyBodyExpr

-- | Expression type checking.
-- Returns a type checked expression.
tcExpr :: SrcExpr -> TypeCheckM TyExpr
tcExpr srcExpr =
  case srcExpr of
    LitE srcLit -> LitE <$> tcLit srcLit

    VarE s _ var -> do
      -- it can be both a local variable and a global function
      unlessM (isVarBoundM var) $
        throwError $ VarNotBound var s
      varType <- getVarTypeM var
      return $ VarE s varType var

    LambdaE s _ srcVarBinders srcBodyExpr -> do
      -- collect variables bound by the lambda together with their types
      (localTypeEnv, revParamTypes, revTyVarBinders) <-
        foldM (\(localTyEnv, revTyList, revTyVbs) svb -> do
            let (VarBinder vs _ srcVar srcVarType) = svb
            let var = getVar srcVar
            isBound <- isVarBoundM var
            -- it is important to check in both places, since localTyEnv
            -- is not queried by 'isVarBoundM', see `LambdaParamsDup` test case
            when (isBound || isVarBound var localTyEnv) $
              throwError $ VarShadowing srcVar
            varType <- srcTypeToType srcVarType
            let tyVarBinder = VarBinder vs varType srcVar srcVarType
            return $ (addLocalVar var varType localTyEnv, varType : revTyList, tyVarBinder : revTyVbs))
          (emptyLocalTypeEnv, [], []) srcVarBinders

      -- extend local type environment with variables introduced by the lambda
      -- this is safe, since we ensure above that all variable and function names are distinct
      -- perform the type checking of the body in this extended environment
      tyBodyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)
      let paramTypes = reverse revParamTypes
      let lambdaType = tyArrowFromList (getTypeOf tyBodyExpr) paramTypes
      return $ LambdaE s lambdaType (reverse revTyVarBinders) tyBodyExpr

    TypeLambdaE s _ srcTypeVars srcBodyExpr -> do
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
      tyBodyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcBodyExpr)
      let typeVars = map getTypeVar srcTypeVars
      let tyLambdaType = tyForAllFromList (getTypeOf tyBodyExpr) typeVars
      return $ TypeLambdaE s tyLambdaType srcTypeVars tyBodyExpr

    TypeAppE s _ srcAppExpr srcArgType -> do
      -- type application can be performed only with forall on the left-hand
      -- side
      -- we replace free occurences of the type variable bound by the forall
      -- inside its body with the right-hand side type
      tyAppExpr <- tcExpr srcAppExpr
      let appType = getTypeOf tyAppExpr
      case appType of
        TyForAll typeVar forallBodyType -> do
          argType <- srcTypeToType srcArgType
          let resultType = (typeVar, argType) `substTypeIn` forallBodyType
          return $ TypeAppE s resultType tyAppExpr srcArgType
        _ -> throwError $ NotForallTypeApp appType (getSrcSpan srcAppExpr)

    ConNameE _ srcConName -> do
      let conName = getConName srcConName
      unlessM (isDataConDefinedM conName) $
        throwError $ ConNotDefined srcConName
      dataConTypeInfo <- getDataConTypeInfoM conName
      return $ ConNameE (dcontiType dataConTypeInfo) srcConName

    CaseE s _ srcScrutExpr srcCaseAlts -> do
      tyScrutExpr <- tcExpr srcScrutExpr
      (tyCaseAlts, caseExprType) <- tcCaseAlts (getTypeOf tyScrutExpr) srcCaseAlts
      return $ CaseE s caseExprType tyScrutExpr tyCaseAlts

    BinOpE s _ srcBinOp srcExpr1 srcExpr2 -> do
      let op = getBinOp srcBinOp
      (tyExpr1, tyExpr2, resultType) <- tcBinOp op srcExpr1 srcExpr2
      return $ BinOpE s resultType srcBinOp tyExpr1 tyExpr2

    ParenE s srcSubExpr -> do
      tySubExpr <- tcExpr srcSubExpr
      return $ ParenE s tySubExpr

    ThrowE s _ srcThrowType -> do
      throwType <- srcTypeToType srcThrowType
      return $ ThrowE s throwType srcThrowType

    DoE {} -> error "do-block is illegal in this context"

tcLit :: SrcLiteral -> TypeCheckM TyLiteral
tcLit srcLit =
  case srcLit of
    UnitLit s _ -> return $ UnitLit s (typeOfLiteral srcLit)
    IntLit s _ i -> return $ IntLit s (typeOfLiteral srcLit) i
    FloatLit s _ f str -> return $ FloatLit s (typeOfLiteral srcLit) f str
    StringLit s _ str -> return $ StringLit s (typeOfLiteral srcLit) str

-- | Takes a scrutinee (an expression we are pattern matching on) type and a
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
tcCaseAlt scrutType (CaseAlt s srcPat srcExpr) = do
  (tyPat, localTypeEnv) <- tcPattern scrutType srcPat emptyLocalTypeEnv
  tyExpr <- locallyWithEnvM localTypeEnv (tcExpr srcExpr)
  return (CaseAlt s tyPat tyExpr, getTypeOf tyExpr)

-- | Takes a scrutinee type, a pattern and a local type environment. Type
-- checks the pattern against the scrutinee type and returns a type checked
-- pattern and the extended local type environment (with variables bound by the
-- pattern).
-- The most interesting case is 'ConP'.
tcPattern :: Type -> SrcPattern -> LocalTypeEnv -> TypeCheckM (TyPattern, LocalTypeEnv)
tcPattern scrutType srcPat localTypeEnv =
  case srcPat of
    LitP srcLit -> do
      tyLit <- tcLit srcLit
      let litType = getTypeOf tyLit
      unless (litType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType litType (getSrcSpan srcLit)
      return (LitP tyLit, localTypeEnv)

    VarP srcVarBinder -> do
      let (VarBinder vs _ srcVar srcVarType) = srcVarBinder
      let var = getVar srcVar
      isBound <- isVarBoundM var
      -- it is important to check in both places, since localTypeEnv is not
      -- queried by 'isVarBoundM', see `VarPatternShadowsNested` test case
      when (isBound || isVarBound var localTypeEnv) $
        throwError $ VarShadowing srcVar
      varType <- srcTypeToType srcVarType
      unless (varType `alphaEq` scrutType) $
        throwError $ PatternIncorrectType scrutType varType vs
      let tyVarBinder = VarBinder vs varType srcVar srcVarType
      return (VarP tyVarBinder, addLocalVar var varType localTypeEnv)

    ConP s _ srcConName subPats -> do
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
          (revTySubPats, localTypeEnv') <-
            foldM (\(revTyPats, localTyEnv) (sp, fieldType) -> do
                (tp, localTyEnv') <- tcPattern fieldType sp localTyEnv
                return (tp:revTyPats, localTyEnv'))
              ([], localTypeEnv) (zip subPats conFieldTypes)
          return (ConP s conType srcConName (reverse revTySubPats), localTypeEnv')
        -- If the scrutinee has a type other than a type application, then this
        -- pattern can not be type correct. Data constructors have a
        -- monomorphic fully applied type constructor type.
        _ -> throwError $ PatternIncorrectType scrutType conType s

    DefaultP s _ -> return (DefaultP s scrutType, localTypeEnv)
    ParenP _ srcPat' -> tcPattern scrutType srcPat' localTypeEnv

-- Binary operations type checking

-- | Type checks a given binary operation. Takes operands.
-- Returns a triple of type checked operands and a type of the result.
tcBinOp :: BinOp -> SrcExpr -> SrcExpr -> TypeCheckM (TyExpr, TyExpr, Type)
tcBinOp op srcExpr1 srcExpr2 = do
  tyExpr1 <- tcExpr srcExpr1
  tyExpr2 <- tcExpr srcExpr2
  resultType <- case op of
    App -> tcApp tyExpr1 tyExpr2
    Add -> tcArith tyExpr1 tyExpr2
    Sub -> tcArith tyExpr1 tyExpr2
    Mul -> tcArith tyExpr1 tyExpr2
    Div -> tcArith tyExpr1 tyExpr2
    Equal -> tcCompare tyExpr1 tyExpr2
    NotEq -> tcCompare tyExpr1 tyExpr2
    Less -> tcCompare tyExpr1 tyExpr2
    Greater -> tcCompare tyExpr1 tyExpr2
    LessEq -> tcCompare tyExpr1 tyExpr2
    GreaterEq -> tcCompare tyExpr1 tyExpr2
    Catch -> tcCatch tyExpr1 tyExpr2
  return (tyExpr1, tyExpr2, resultType)

-- | Type synonym for binary operation type checking functions.
-- They take two type checked operands and return a type of the result.
type BinOpTc = TyExpr -> TyExpr -> TypeCheckM Type

-- | Function application type checking.
tcApp :: BinOpTc
tcApp tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2 in
  case expr1Type of
    TyArrow argType resultType ->
      if not (expr2Type `alphaEq` argType)
        then throwError $ IncorrectFunArgType tyExpr2 argType expr2Type
        else return resultType
    _ -> throwError $ NotFunctionType tyExpr1 expr1Type

tcArith :: BinOpTc
tcArith tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2 in
  case (isArithType expr1Type, isArithType expr2Type, expr2Type `alphaEq` expr1Type) of
    (False, _, _) -> throwError $ NotArithType expr1Type (getSrcSpan tyExpr1)
    (True, False, _) -> throwError $ NotArithType expr2Type (getSrcSpan tyExpr2)
    (True, True, False) -> throwError $ IncorrectExprType expr1Type expr2Type (getSrcSpan tyExpr2)
    (True, True, True) -> return expr1Type

tcCompare :: BinOpTc
tcCompare tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2 in
  case (isComparableType expr1Type, isComparableType expr2Type, expr2Type `alphaEq` expr1Type) of
    (False, _, _) -> throwError $ NotComparableType expr1Type (getSrcSpan tyExpr1)
    (True, False, _) -> throwError $ NotComparableType expr2Type (getSrcSpan tyExpr2)
    (True, True, False) -> throwError $ IncorrectExprType expr1Type expr2Type (getSrcSpan tyExpr2)
    (True, True, True) -> return boolType

tcCatch :: BinOpTc
tcCatch tyExpr1 tyExpr2 =
  let expr1Type = getTypeOf tyExpr1
      expr2Type = getTypeOf tyExpr2 in
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
      tyExpr <- tcExpr srcExpr
      let exprType = getTypeOf tyExpr
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

    BindS s srcVarBinder srcExpr -> do
      let (VarBinder vs _ srcVar srcVarType) = srcVarBinder
      let var = getVar srcVar
      whenM (isVarBoundM var) $
        throwError $ VarShadowing srcVar
      varType <- srcTypeToType srcVarType

      tyExpr <- tcExpr srcExpr
      let exprType = getTypeOf tyExpr

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
      let tyVarBinder = VarBinder vs varType srcVar srcVarType
      let tyStmt = BindS s tyVarBinder tyExpr
      -- srcStmts is not empty, otherwise we would have thrown an error in the
      -- first equation
      (tyStmts, lastStmtType) <- locallyWithEnvM localTypeEnv (tcDoBlock srcStmts funMonad)
      return (tyStmt : tyStmts, lastStmtType)

    ReturnS s _ srcExpr -> do
      tyExpr <- tcExpr srcExpr
      let exprType = getTypeOf tyExpr
          stmtType = applyMonadType funMonad exprType
          tyStmt = ReturnS s stmtType tyExpr
      (tyStmts, lastStmtType) <-
        if null srcStmts
          then return ([], stmtType)
          else tcDoBlock srcStmts funMonad
      return (tyStmt : tyStmts, lastStmtType)
tcDoBlock [] _ = error "do-block can't be empty"

