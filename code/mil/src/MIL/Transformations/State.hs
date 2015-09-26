-- | Module defining State monad transformations.
module MIL.Transformations.State where

import MIL.AST

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
exchangeRead = undefined

useRead :: TyProgram -> TyProgram
useRead = undefined

useWrite :: TyProgram -> TyProgram
useWrite = undefined

