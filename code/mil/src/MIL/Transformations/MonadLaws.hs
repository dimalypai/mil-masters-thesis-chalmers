-- | Module defining transformations based on monad laws.
module MIL.Transformations.MonadLaws where

import Data.Generics.Uniplate.Data

import MIL.AST

leftIdentity :: TyProgram -> TyProgram
leftIdentity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map leftIdentityFun funDefs)

leftIdentityFun :: TyFunDef -> TyFunDef
leftIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (leftIdentityExpr funBody)

leftIdentityExpr :: TyExpr -> TyExpr
leftIdentityExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e1 of
        ReturnE _tm e ->
          (VarE varBinder, leftIdentityExpr e) `replaceExprIn` (leftIdentityExpr e2)
        _ -> LetE varBinder (leftIdentityExpr e1) (leftIdentityExpr e2)
    f expr = descend f expr

rightIdentity :: TyProgram -> TyProgram
rightIdentity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map rightIdentityFun funDefs)

rightIdentityFun :: TyFunDef -> TyFunDef
rightIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (rightIdentityExpr funBody)

rightIdentityExpr :: TyExpr -> TyExpr
rightIdentityExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e2 of
        ReturnE _tm (VarE vb) | vb == varBinder -> (rightIdentityExpr e1)
        _ -> LetE varBinder (rightIdentityExpr e1) (rightIdentityExpr e2)
    f expr = descend f expr

associativity :: TyProgram -> TyProgram
associativity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map associativityFun funDefs)

associativityFun :: TyFunDef -> TyFunDef
associativityFun (FunDef funName funType funBody) =
  FunDef funName funType (associativityExpr funBody)

associativityExpr :: TyExpr -> TyExpr
associativityExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e1 of
        LetE varBinder' e1' e2' ->
          LetE varBinder' (associativityExpr e1')
            (LetE varBinder (associativityExpr e2') (associativityExpr e2))
        _ -> LetE varBinder (associativityExpr e1) (associativityExpr e2)
    f expr = descend f expr

-- | TODO: name capturing?
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

