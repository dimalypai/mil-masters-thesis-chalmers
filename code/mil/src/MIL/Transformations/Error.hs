-- | Module defining Error (exceptions) transformations.
module MIL.Transformations.Error where

import MIL.AST

eliminateThrowCatch :: TyProgram -> TyProgram
eliminateThrowCatch (Program (typeDefs, funDefs)) =
  Program (typeDefs, map eliminateThrowCatchFun funDefs)

eliminateThrowCatchFun :: TyFunDef -> TyFunDef
eliminateThrowCatchFun (FunDef funName funType funBody) =
  FunDef funName funType (eliminateThrowCatchExpr funBody)

eliminateThrowCatchExpr :: TyExpr -> TyExpr
eliminateThrowCatchExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (eliminateThrowCatchExpr e)

    AppE e1 e2 ->
      case e1 of
        AppE (TypeAppE (TypeAppE (VarE (VarBinder (Var catchName, _))) (TyTypeCon (TypeName "Unit"))) _)
             (AppE (TypeAppE (TypeAppE (VarE (VarBinder (Var "throw_error", _))) _) _) _) |
             catchName == "catch_error_1" || catchName == "catch_error_2" ->
          let (LambdaE _ handlerBody) = e2
          in eliminateThrowCatchExpr handlerBody
        _ -> AppE (eliminateThrowCatchExpr e1) (eliminateThrowCatchExpr e2)

    TypeLambdaE typeVar e -> TypeLambdaE typeVar (eliminateThrowCatchExpr e)
    TypeAppE e t -> TypeAppE (eliminateThrowCatchExpr e) t
    LetE varBinder e1 e2 -> LetE varBinder (eliminateThrowCatchExpr e1) (eliminateThrowCatchExpr e2)
    ReturnE tm e -> ReturnE tm (eliminateThrowCatchExpr e)
    LiftE e tm1 tm2 -> LiftE (eliminateThrowCatchExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, eliminateThrowCatchExpr be)) binders) (eliminateThrowCatchExpr e)
    CaseE e caseAlts -> CaseE (eliminateThrowCatchExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, eliminateThrowCatchExpr ae)) caseAlts)
    TupleE es -> TupleE (map eliminateThrowCatchExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

