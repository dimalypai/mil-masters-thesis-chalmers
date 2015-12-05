-- | Module defining case expression transformations.
module MIL.Transformations.CaseExpression where

import Data.Generics.Uniplate.Data

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
eliminateConstantCaseExpr = descendBi f
  where
    f (CaseE e caseAlts) =
      -- Relies on the fact that patterns are non-overlapping
      case e of
        LitE lit ->
          case find (\(CaseAlt (p, _)) -> case p of
            LitP lit' -> lit' == lit
            _ -> False) caseAlts of
            Just (CaseAlt (_, caseAltBody)) -> eliminateConstantCaseExpr caseAltBody
            Nothing ->
              CaseE (eliminateConstantCaseExpr e)
               (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateConstantCaseExpr ae)) caseAlts)
        ConNameE conName _ ->
          case find (\(CaseAlt (p, _)) -> case p of
            ConP conName' [] -> conName' == conName
            _ -> False) caseAlts of
            Just (CaseAlt (_, caseAltBody)) -> eliminateConstantCaseExpr caseAltBody
            Nothing ->
              CaseE (eliminateConstantCaseExpr e)
              (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateConstantCaseExpr ae)) caseAlts)
        _ -> CaseE (eliminateConstantCaseExpr e)
               (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateConstantCaseExpr ae)) caseAlts)
    f expr = descend f expr

extractCommonBind :: TyProgram -> TyProgram
extractCommonBind (Program (typeDefs, funDefs)) =
  Program (typeDefs, map extractCommonBindFun funDefs)

extractCommonBindFun :: TyFunDef -> TyFunDef
extractCommonBindFun (FunDef funName funType funBody) =
  FunDef funName funType (extractCommonBindExpr funBody)

extractCommonBindExpr :: TyExpr -> TyExpr
extractCommonBindExpr = descendBi f
  where
    f (CaseE e caseAlts) =
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
    f expr = descend f expr

