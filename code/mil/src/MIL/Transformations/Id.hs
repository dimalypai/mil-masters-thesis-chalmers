-- | Module defining Id monad transformations.
module MIL.Transformations.Id where

import MIL.AST
import MIL.AST.Helpers
import MIL.AST.TypeAnnotated

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

isNotUsedIn :: Var -> TyExpr -> Bool
v `isNotUsedIn` e =
  case e of
    VarE v' -> v /= getBinderVar v'
    LitE {} -> False
    ConNameE {} -> False
    LambdaE _varBinder e' -> v `isNotUsedIn` e'
    AppE e1 e2 -> (v `isNotUsedIn` e1) && (v `isNotUsedIn` e2)
    TypeLambdaE _typeVar e' -> v `isNotUsedIn` e'
    TypeAppE e' _t -> v `isNotUsedIn` e'
    LetE _varBinder e1 e2 -> (v `isNotUsedIn` e1) && (v `isNotUsedIn` e2)
    ReturnE _tm e' -> v `isNotUsedIn` e'
    LiftE e' _tm1 _tm2 -> v `isNotUsedIn` e'
    LetRecE binders e' -> all (\(_vb, be) -> v `isNotUsedIn` be) binders && (v `isNotUsedIn` e')
    CaseE e' caseAlts -> (v `isNotUsedIn` e') && all (\(CaseAlt (_p, ae)) -> v `isNotUsedIn` ae) caseAlts
    TupleE es -> all (\e' -> v `isNotUsedIn` e') es

