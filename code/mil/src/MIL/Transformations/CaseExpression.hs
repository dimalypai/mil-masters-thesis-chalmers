-- | Module defining case expression transformations.
module MIL.Transformations.CaseExpression where

import MIL.AST
import Data.List (find, foldl1')
import Data.Maybe (fromJust)

eliminateConstantCase :: TyProgram -> TyProgram
eliminateConstantCase (Program (typeDefs, funDefs)) =
  Program (typeDefs, map eliminateConstantCaseFun funDefs)

eliminateConstantCaseFun :: TyFunDef -> TyFunDef
eliminateConstantCaseFun (FunDef funName funType funBody) =
  FunDef funName funType (eliminateConstantCaseExpr funBody)

eliminateConstantCaseExpr :: TyExpr -> TyExpr
eliminateConstantCaseExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (eliminateConstantCaseExpr e)
    AppE e1 e2 -> AppE (eliminateConstantCaseExpr e1) (eliminateConstantCaseExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (eliminateConstantCaseExpr e)
    TypeAppE e t -> TypeAppE (eliminateConstantCaseExpr e) t
    LetE varBinder e1 e2 -> LetE varBinder (eliminateConstantCaseExpr e1) (eliminateConstantCaseExpr e2)
    ReturnE tm e -> ReturnE tm (eliminateConstantCaseExpr e)
    LiftE e tm1 tm2 -> LiftE (eliminateConstantCaseExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, eliminateConstantCaseExpr be)) binders) (eliminateConstantCaseExpr e)
    CaseE e caseAlts ->
      -- Relies on the fact that patterns are non-overlapping
      case e of
        LitE lit ->
          case find (\(CaseAlt (p, _)) -> case p of
            LitP lit' -> lit' == lit
            _ -> False) caseAlts of
            Just (CaseAlt (_, caseAltBody)) -> eliminateConstantCaseExpr caseAltBody
            Nothing -> CaseE (eliminateConstantCaseExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateConstantCaseExpr ae)) caseAlts)
        ConNameE conName _ ->
          case find (\(CaseAlt (p, _)) -> case p of
            ConP conName' [] -> conName' == conName
            _ -> False) caseAlts of
            Just (CaseAlt (_, caseAltBody)) -> eliminateConstantCaseExpr caseAltBody
            Nothing -> CaseE (eliminateConstantCaseExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateConstantCaseExpr ae)) caseAlts)
        _ -> CaseE (eliminateConstantCaseExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateConstantCaseExpr ae)) caseAlts)
    TupleE es -> TupleE (map eliminateConstantCaseExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

extractCommonBind :: TyProgram -> TyProgram
extractCommonBind (Program (typeDefs, funDefs)) =
  Program (typeDefs, map extractCommonBindFun funDefs)

extractCommonBindFun :: TyFunDef -> TyFunDef
extractCommonBindFun (FunDef funName funType funBody) =
  FunDef funName funType (extractCommonBindExpr funBody)

extractCommonBindExpr :: TyExpr -> TyExpr
extractCommonBindExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (extractCommonBindExpr e)
    AppE e1 e2 -> AppE (extractCommonBindExpr e1) (extractCommonBindExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (extractCommonBindExpr e)
    TypeAppE e t -> TypeAppE (extractCommonBindExpr e) t
    LetE varBinder e1 e2 -> LetE varBinder (extractCommonBindExpr e1) (extractCommonBindExpr e2)
    ReturnE tm e -> ReturnE tm (extractCommonBindExpr e)
    LiftE e tm1 tm2 -> LiftE (extractCommonBindExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, extractCommonBindExpr be)) binders) (extractCommonBindExpr e)
    CaseE e caseAlts ->
      let (varBinders, letBindExprs, letBodyExprs) =
            unzip3 $
              map (\(CaseAlt (_, ae)) ->
                case ae of
                  LetE vb bindE bodyE -> (Just vb, Just bindE, Just bodyE)
                  _ -> (Nothing, Nothing, Nothing)) caseAlts

          mCommonVarBinder =
            foldl1' (\mCommonVb mVb ->
              case (mCommonVb, mVb) of
                (Just commonVb, Just vb) ->
                  if commonVb == vb
                    then Just commonVb
                    else Nothing
                _ -> Nothing) varBinders

          mCommonLetBodyExpr =
            foldl1' (\mCommonLetBodyE mLetBodyE ->
              case (mCommonLetBodyE, mLetBodyE) of
                (Just commonLetBodyE, Just letBodyE) ->
                  if commonLetBodyE == letBodyE
                    then Just commonLetBodyE
                    else Nothing
                _ -> Nothing) letBodyExprs

      in case (mCommonVarBinder, mCommonLetBodyExpr) of
        (Just commonVarBinder, Just commonLetBodyExpr) ->
          let newCaseAlts =
                map (\((CaseAlt (p, _)), mLetBindE) -> CaseAlt (p, fromJust mLetBindE))
                  (zip caseAlts letBindExprs)
          in LetE commonVarBinder (CaseE e newCaseAlts) commonLetBodyExpr
        _ -> CaseE (extractCommonBindExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, extractCommonBindExpr ae)) caseAlts)
    TupleE es -> TupleE (map extractCommonBindExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

