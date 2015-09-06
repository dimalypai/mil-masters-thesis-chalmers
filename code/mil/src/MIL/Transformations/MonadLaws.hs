-- | Module defining transformations based on monad laws.
module MIL.Transformations.MonadLaws where

import MIL.AST

leftIdentity :: TyProgram -> TyProgram
leftIdentity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map leftIdentityFun funDefs)

leftIdentityFun :: TyFunDef -> TyFunDef
leftIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (leftIdentityExpr funBody)

leftIdentityExpr :: TyExpr -> TyExpr
leftIdentityExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (leftIdentityExpr e)
    AppE e1 e2 -> AppE (leftIdentityExpr e1) (leftIdentityExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (leftIdentityExpr e)
    TypeAppE e t -> TypeAppE (leftIdentityExpr e) t
    LetE varBinder e1 e2 ->
      case e1 of
        ReturnE tm e -> (VarE varBinder, leftIdentityExpr e) `replaceExprIn` (leftIdentityExpr e2)
        _ -> LetE varBinder (leftIdentityExpr e1) (leftIdentityExpr e2)
    ReturnE tm e -> ReturnE tm (leftIdentityExpr e)
    LiftE e tm1 tm2 -> LiftE (leftIdentityExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, leftIdentityExpr be)) binders) (leftIdentityExpr e)
    CaseE e caseAlts -> CaseE (leftIdentityExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, leftIdentityExpr ae)) caseAlts)
    TupleE es -> TupleE (map leftIdentityExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

rightIdentity :: TyProgram -> TyProgram
rightIdentity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map rightIdentityFun funDefs)

rightIdentityFun :: TyFunDef -> TyFunDef
rightIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (rightIdentityExpr funBody)

rightIdentityExpr :: TyExpr -> TyExpr
rightIdentityExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (rightIdentityExpr e)
    AppE e1 e2 -> AppE (rightIdentityExpr e1) (rightIdentityExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (rightIdentityExpr e)
    TypeAppE e t -> TypeAppE (rightIdentityExpr e) t
    LetE varBinder e1 e2 ->
      case e2 of
        ReturnE tm (VarE vb) | vb == varBinder -> (rightIdentityExpr e1)
        _ -> LetE varBinder (rightIdentityExpr e1) (rightIdentityExpr e2)
    ReturnE tm e -> ReturnE tm (rightIdentityExpr e)
    LiftE e tm1 tm2 -> LiftE (rightIdentityExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, rightIdentityExpr be)) binders) (rightIdentityExpr e)
    CaseE e caseAlts -> CaseE (rightIdentityExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, rightIdentityExpr ae)) caseAlts)
    TupleE es -> TupleE (map rightIdentityExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

associativity :: TyProgram -> TyProgram
associativity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map associativityFun funDefs)

associativityFun :: TyFunDef -> TyFunDef
associativityFun (FunDef funName funType funBody) =
  FunDef funName funType (associativityExpr funBody)

associativityExpr :: TyExpr -> TyExpr
associativityExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (associativityExpr e)
    AppE e1 e2 -> AppE (associativityExpr e1) (associativityExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (associativityExpr e)
    TypeAppE e t -> TypeAppE (associativityExpr e) t
    LetE varBinder e1 e2 ->
      case e1 of
        LetE varBinder' e1' e2' ->
          LetE varBinder' (associativityExpr e1') (LetE varBinder (associativityExpr e2') (associativityExpr e2))
        _ -> LetE varBinder (associativityExpr e1) (associativityExpr e2)
    ReturnE tm e -> ReturnE tm (associativityExpr e)
    LiftE e tm1 tm2 -> LiftE (associativityExpr e) tm1 tm2
    LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, associativityExpr be)) binders) (associativityExpr e)
    CaseE e caseAlts -> CaseE (associativityExpr e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, associativityExpr ae)) caseAlts)
    TupleE es -> TupleE (map associativityExpr es)
    LitE {} -> expr
    ConNameE {} -> expr
    VarE {} -> expr

-- | TODO: name capturing
replaceExprIn :: (TyExpr, TyExpr) -> TyExpr -> TyExpr
re@(oldExpr, newExpr) `replaceExprIn` expr =
  if (expr == oldExpr)
    then newExpr
    else case expr of
           LambdaE varBinder e -> LambdaE varBinder (re `replaceExprIn` e)
           AppE e1 e2 -> AppE (re `replaceExprIn` e1) (re `replaceExprIn` e2)
           TypeLambdaE typeVar e -> TypeLambdaE typeVar (re `replaceExprIn` e)
           TypeAppE e t -> TypeAppE (re `replaceExprIn` e) t
           LetE varBinder e1 e2 -> LetE varBinder (re `replaceExprIn` e1) (re `replaceExprIn` e2)
           ReturnE tm e -> ReturnE tm (re `replaceExprIn` e)
           LiftE e tm1 tm2 -> LiftE (re `replaceExprIn` e) tm1 tm2
           LetRecE binders e -> LetRecE (map (\(vb, be) -> (vb, re `replaceExprIn` be)) binders) (re `replaceExprIn` e)
           CaseE e caseAlts -> CaseE (re `replaceExprIn` e) (map (\(CaseAlt (p, ae)) -> CaseAlt (p, re `replaceExprIn` ae)) caseAlts)
           TupleE es -> TupleE (map (replaceExprIn re) es)
           LitE {} -> expr
           ConNameE {} -> expr
           VarE {} -> expr

