-- | Module defining Id monad transformations.
module MIL.Transformations.Id where

import MIL.AST
import MIL.AST.Helpers
import MIL.AST.TypeAnnotated
import MIL.Transformations.Helpers

exchange :: TyProgram -> TyProgram
exchange (Program (typeDefs, funDefs)) =
  Program (typeDefs, map exchangeFun funDefs)

exchangeFun :: TyFunDef -> TyFunDef
exchangeFun (FunDef funName funType funBody) =
  FunDef funName funType (exchangeExpr funBody)

exchangeExpr :: TyExpr -> TyExpr
exchangeExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (exchangeExpr e)
    AppE e1 e2 -> AppE (exchangeExpr e1) (exchangeExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (exchangeExpr e)
    TypeAppE e t -> TypeAppE (exchangeExpr e) t
    LetE varBinder e1 e2 ->
      case e2 of
        LetE varBinder' e1' e2' | getBinderVar varBinder `isNotUsedIn` e1' ->
          case getTypeOf expr of
            TyApp (TyMonad (MTyMonad (SinMonad Id))) _ ->
              LetE varBinder' (exchangeExpr e1') (LetE varBinder (exchangeExpr e1) (exchangeExpr e2'))
            _ -> LetE varBinder (exchangeExpr e1) (exchangeExpr e2)
        _ -> LetE varBinder (exchangeExpr e1) (exchangeExpr e2)
    ReturnE tm e -> ReturnE tm (exchangeExpr e)
    LiftE e tm1 tm2 -> LiftE (exchangeExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, exchangeExpr be)) binders) (exchangeExpr e)
    CaseE e caseAlts -> CaseE (exchangeExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, exchangeExpr ae)) caseAlts)
    TupleE es -> TupleE (map exchangeExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

