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

import FunLang.AST
import FunLang.TypeChecker.TypeCheckM
import FunLang.TypeChecker.TcError
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
  whenM (isTypeDefined $ getTypeName srcTypeName) $
    throwError $ TypeAlreadyDefined srcTypeName
  let typeVars = map getTypeVar srcTypeVars
  foldM_ (\tvs (tv, stv) ->
            if tv `Set.member` tvs
              then throwError $ TypeParamAlreadyDefined stv
              else return $ Set.insert tv tvs)
         Set.empty (zip typeVars srcTypeVars)
  let kind = mkKind (length srcTypeVars)
  addType srcTypeName kind

-- | Checks if the function is already defined.
-- Checks that the specified function type is correct (well-formed,
-- well-kinded and uses types in scope).
-- Adds the function and its type to the environment.
collectFunDef :: SrcFunDef -> TypeCheckM ()
collectFunDef (FunDef _ srcFunName funSrcType _) = do
  whenM (isFunctionDefined $ getFunName srcFunName) $
    throwError $ FunctionAlreadyDefined srcFunName
  funType <- srcTypeToType funSrcType
  addFunction srcFunName funType funSrcType

-- | Program needs to have an entry point: `main : IO Unit`.
checkMain :: TypeCheckM ()
checkMain = do
  unlessM (isFunctionDefined $ FunName "main") $
    throwError MainNotDefined
  funTypeInfo <- getFunTypeInfo $ FunName "main"
  let mainType = ioType unitType
  when (ftiType funTypeInfo /= mainType) $
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
  whenM (isDataConDefined $ getConName srcConName) $
    throwError $ ConAlreadyDefined srcConName
  conFields <- mapM (srcTypeToTypeWithTypeVars $ Set.fromList typeVars) srcConFields
  let conResultType = TyApp typeName (map TyVar typeVars)
      conArrType = foldr (\t acc -> TyArrow t acc) conResultType conFields
      conType = foldr (\tv acc -> TyForAll tv acc) conArrType typeVars
  addDataCon srcConName conType typeName

-- | Checks all function equations and their consistency (TODO).
-- Returns type checked function definition.
tcFunDef :: SrcFunDef -> TypeCheckM TyFunDef
tcFunDef (FunDef s srcFunName funSrcType srcFunEqs) = do
  let funName = getFunName srcFunName
  funType <- srcTypeToType funSrcType
  tyFunEqs <- mapM (tcFunEq funName funType) srcFunEqs
  return $ FunDef s srcFunName funSrcType tyFunEqs

-- | Checks that function equation belongs to the correct function definition.
-- Checks equation body.
-- Checks that the type of the body is consistent with the specified function
-- type.
-- Returns type checked function equation.
tcFunEq :: FunName -> Type -> SrcFunEq -> TypeCheckM TyFunEq
tcFunEq funName funType (FunEq s srcFunName patterns srcBodyExpr) = do
  when (getFunName srcFunName /= funName) $
    throwError $ FunEqIncorrectName srcFunName funName
  (tyBodyExpr, bodyType) <- tcExpr srcBodyExpr
  when (bodyType /= funType) $
    throwError $ FunEqBodyIncorrectType srcBodyExpr funName funType bodyType
  return $ FunEq s srcFunName [] tyBodyExpr

-- | Expression type checking.
-- Returns a type checked expression together with its type.
tcExpr :: SrcExpr -> TypeCheckM (TyExpr, Type)
tcExpr srcExpr =
  case srcExpr of
    LitE srcLit -> return (LitE srcLit, typeOfLiteral $ getLiteral srcLit)

    ConNameE srcConName -> do
      let conName = getConName srcConName
      unlessM (isDataConDefined conName) $
        throwError $ ConNotDefined srcConName
      dataConTypeInfo <- getDataConTypeInfo (getConName srcConName)
      return (ConNameE srcConName, dcontiType dataConTypeInfo)

    DoE s srcStmts -> do
      (tyStmts, stmtTypes) <- fmap unzip (mapM tcStmt srcStmts)
      -- There is at least one statement, the type of the last one is the type
      -- of the whole expression
      return (DoE s tyStmts, last stmtTypes)

    ParenE s srcSubExpr -> do
      (tySubExpr, exprType) <- tcExpr srcSubExpr
      return (ParenE s tySubExpr, exprType)


-- | Statement type checking.
-- Returns a type checked statement together with its type.
tcStmt :: SrcStmt -> TypeCheckM (TyStmt, Type)
tcStmt srcStmt =
  case srcStmt of
    ReturnS s srcType srcExpr -> do
      retType <- srcTypeToTypeOfKind monadKind srcType
      let typeName = getTyAppTypeName retType  -- should not fail
      when (typeName `Set.notMember` monadTypes) $
        throwError $ NotMonad srcType
      (tyExpr, exprType) <- tcExpr srcExpr
      return (ReturnS s srcType tyExpr, TyApp typeName [exprType])

-- Type transformations

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
    (if typeVar `Set.member` typeVars
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
  whenM (isTypeDefined $ typeVarToTypeName typeVar) $
    throwError $ TypeVarShadowsType srcTypeVar
  when (typeVar `Set.member` typeVars) $
    throwError $ TypeVarShadowsTypeVar srcTypeVar
  let typeVars' = Set.insert typeVar typeVars
  t <- srcTypeToTypeWithTypeVarsOfKind typeVars' kind st
  return $ TyForAll typeVar t

srcTypeToTypeWithTypeVarsOfKind typeVars kind (SrcTyParen _ st) =
  srcTypeToTypeWithTypeVarsOfKind typeVars kind st

