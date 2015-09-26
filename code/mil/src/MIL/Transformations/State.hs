-- | Module defining State monad transformations.
module MIL.Transformations.State where

import MIL.AST
import MIL.AST.Helpers
import MIL.Transformations.Helpers

exchangeNew :: TyProgram -> TyProgram
exchangeNew (Program (typeDefs, funDefs)) =
  Program (typeDefs, map exchangeNewFun funDefs)

exchangeNewFun :: TyFunDef -> TyFunDef
exchangeNewFun (FunDef funName funType funBody) =
  FunDef funName funType (exchangeNewExpr funBody)

exchangeNewExpr :: TyExpr -> TyExpr
exchangeNewExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (exchangeNewExpr e)
    AppE e1 e2 -> AppE (exchangeNewExpr e1) (exchangeNewExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (exchangeNewExpr e)
    TypeAppE e t -> TypeAppE (exchangeNewExpr e) t
    LetE varBinder e1 e2 ->
      case e2 of
        LetE varBinder' e1' e2' ->
          case (e1, e1') of
            (AppE (TypeAppE (VarE (VarBinder (Var "new_ref", _))) _) (LitE {}), AppE (TypeAppE (VarE (VarBinder (Var "new_ref", _))) _) (LitE {})) ->
              LetE varBinder' (exchangeNewExpr e1') (LetE varBinder (exchangeNewExpr e1) (exchangeNewExpr e2'))
            _ -> LetE varBinder (exchangeNewExpr e1) (exchangeNewExpr e2)
        _ -> LetE varBinder (exchangeNewExpr e1) (exchangeNewExpr e2)
    ReturnE tm e -> ReturnE tm (exchangeNewExpr e)
    LiftE e tm1 tm2 -> LiftE (exchangeNewExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, exchangeNewExpr be)) binders) (exchangeNewExpr e)
    CaseE e caseAlts -> CaseE (exchangeNewExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, exchangeNewExpr ae)) caseAlts)
    TupleE es -> TupleE (map exchangeNewExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr


exchangeRead :: TyProgram -> TyProgram
exchangeRead (Program (typeDefs, funDefs)) =
  Program (typeDefs, map exchangeReadFun funDefs)

exchangeReadFun :: TyFunDef -> TyFunDef
exchangeReadFun (FunDef funName funType funBody) =
  FunDef funName funType (exchangeReadExpr funBody)

exchangeReadExpr :: TyExpr -> TyExpr
exchangeReadExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (exchangeReadExpr e)
    AppE e1 e2 -> AppE (exchangeReadExpr e1) (exchangeReadExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (exchangeReadExpr e)
    TypeAppE e t -> TypeAppE (exchangeReadExpr e) t
    LetE varBinder e1 e2 ->
      case e2 of
        LetE varBinder' e1' e2' | getBinderVar varBinder `isNotUsedIn` e1' ->
          case (e1, e1') of
            (AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _) _, AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _) _) ->
              LetE varBinder' (exchangeReadExpr e1') (LetE varBinder (exchangeReadExpr e1) (exchangeReadExpr e2'))
            _ -> LetE varBinder (exchangeReadExpr e1) (exchangeReadExpr e2)
        _ -> LetE varBinder (exchangeReadExpr e1) (exchangeReadExpr e2)
    ReturnE tm e -> ReturnE tm (exchangeReadExpr e)
    LiftE e tm1 tm2 -> LiftE (exchangeReadExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, exchangeReadExpr be)) binders) (exchangeReadExpr e)
    CaseE e caseAlts -> CaseE (exchangeReadExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, exchangeReadExpr ae)) caseAlts)
    TupleE es -> TupleE (map exchangeReadExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr


useRead :: TyProgram -> TyProgram
useRead (Program (typeDefs, funDefs)) =
  Program (typeDefs, map useReadFun funDefs)

useReadFun :: TyFunDef -> TyFunDef
useReadFun (FunDef funName funType funBody) =
  FunDef funName funType (useReadExpr funBody)

useReadExpr :: TyExpr -> TyExpr
useReadExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (useReadExpr e)
    AppE e1 e2 -> AppE (useReadExpr e1) (useReadExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (useReadExpr e)
    TypeAppE e t -> TypeAppE (useReadExpr e) t
    LetE varBinder e1 e2 ->
      case e2 of
        LetE varBinder' e1' e2' ->
          case (e1, e1') of
            (  AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _) (VarE (VarBinder (refVar1, _)))
             , AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _) (VarE (VarBinder (refVar2, _)))) | refVar1 == refVar2 ->
              LetE varBinder (useReadExpr e1)
                (LetE varBinder' (ReturnE (MTyMonad (SinMonad State)) (VarE varBinder)) (useReadExpr e2'))
            _ -> LetE varBinder (useReadExpr e1) (useReadExpr e2)
        _ -> LetE varBinder (useReadExpr e1) (useReadExpr e2)
    ReturnE tm e -> ReturnE tm (useReadExpr e)
    LiftE e tm1 tm2 -> LiftE (useReadExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, useReadExpr be)) binders) (useReadExpr e)
    CaseE e caseAlts -> CaseE (useReadExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, useReadExpr ae)) caseAlts)
    TupleE es -> TupleE (map useReadExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr


useWrite :: TyProgram -> TyProgram
useWrite = undefined

