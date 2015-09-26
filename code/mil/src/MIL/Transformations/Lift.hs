-- | Module defining lift operator transformations.
module MIL.Transformations.Lift where

import MIL.AST
import MIL.TypeChecker.AlphaEq

liftIdentity :: TyProgram -> TyProgram
liftIdentity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map liftIdentityFun funDefs)

liftIdentityFun :: TyFunDef -> TyFunDef
liftIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (liftIdentityExpr funBody)

liftIdentityExpr :: TyExpr -> TyExpr
liftIdentityExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (liftIdentityExpr e)
    AppE e1 e2 -> AppE (liftIdentityExpr e1) (liftIdentityExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (liftIdentityExpr e)
    TypeAppE e t -> TypeAppE (liftIdentityExpr e) t
    LetE varBinder e1 e2 -> LetE varBinder (liftIdentityExpr e1) (liftIdentityExpr e2)
    ReturnE tm e -> ReturnE tm (liftIdentityExpr e)
    LiftE e tm1 tm2 ->
      if tm1 `alphaEq` tm2
        then e
        else LiftE (liftIdentityExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, liftIdentityExpr be)) binders) (liftIdentityExpr e)
    CaseE e caseAlts -> CaseE (liftIdentityExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, liftIdentityExpr ae)) caseAlts)
    TupleE es -> TupleE (map liftIdentityExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

composeLift :: TyProgram -> TyProgram
composeLift (Program (typeDefs, funDefs)) =
  Program (typeDefs, map composeLiftFun funDefs)

composeLiftFun :: TyFunDef -> TyFunDef
composeLiftFun (FunDef funName funType funBody) =
  FunDef funName funType (composeLiftExpr funBody)

composeLiftExpr :: TyExpr -> TyExpr
composeLiftExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (composeLiftExpr e)
    AppE e1 e2 -> AppE (composeLiftExpr e1) (composeLiftExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (composeLiftExpr e)
    TypeAppE e t -> TypeAppE (composeLiftExpr e) t
    LetE varBinder e1 e2 -> LetE varBinder (composeLiftExpr e1) (composeLiftExpr e2)
    ReturnE tm e -> ReturnE tm (composeLiftExpr e)
    LiftE e tm1 tm2 ->
      case e of
        LiftE e' tm1' _ -> LiftE e' tm1' tm2
        _ -> LiftE (composeLiftExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, composeLiftExpr be)) binders) (composeLiftExpr e)
    CaseE e caseAlts -> CaseE (composeLiftExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, composeLiftExpr ae)) caseAlts)
    TupleE es -> TupleE (map composeLiftExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

