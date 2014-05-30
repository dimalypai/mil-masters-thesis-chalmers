-- | Module defining transformations based on monad laws.
module MIL.Transformations.MonadLaws where

import MIL.AST

leftIdentity :: Program -> Program
leftIdentity (Program (typeDefs, aliasDefs, funDefs)) =
  Program (typeDefs, aliasDefs, map leftIdentityFun funDefs)

leftIdentityFun :: FunDef -> FunDef
leftIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (leftIdentityExpr funBody)

leftIdentityExpr :: Expr -> Expr
leftIdentityExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (leftIdentityExpr e)
    AppE e1 e2 -> AppE (leftIdentityExpr e1) (leftIdentityExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (leftIdentityExpr e)
    TypeAppE e t -> TypeAppE (leftIdentityExpr e) t
    LetE varBinder e1 e2 ->
      case e1 of
        -- TODO: leftIdentityExpr on top?
        -- What if it is not used? Depends on the effect
        ReturnE tm e -> (VarE varBinder, leftIdentityExpr e) `replaceExprIn` (leftIdentityExpr e2)
        _ -> LetE varBinder (leftIdentityExpr e1) (leftIdentityExpr e2)
    ReturnE tm e -> ReturnE tm (leftIdentityExpr e)
    LiftE e tm1 tm2 -> LiftE (leftIdentityExpr e) tm1 tm2
    _ -> expr

rightIdentity :: Program -> Program
rightIdentity (Program (typeDefs, aliasDefs, funDefs)) =
  Program (typeDefs, aliasDefs, map rightIdentityFun funDefs)

rightIdentityFun :: FunDef -> FunDef
rightIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (rightIdentityExpr funBody)

rightIdentityExpr :: Expr -> Expr
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
    _ -> expr

associativity :: Program -> Program
associativity (Program (typeDefs, aliasDefs, funDefs)) =
  Program (typeDefs, aliasDefs, map associativityFun funDefs)

associativityFun :: FunDef -> FunDef
associativityFun (FunDef funName funType funBody) =
  FunDef funName funType (associativityExpr funBody)

associativityExpr :: Expr -> Expr
associativityExpr expr =
  case expr of
    LambdaE varBinder e -> LambdaE varBinder (associativityExpr e)
    AppE e1 e2 -> AppE (associativityExpr e1) (associativityExpr e2)
    TypeLambdaE typeVar e -> TypeLambdaE typeVar (associativityExpr e)
    TypeAppE e t -> TypeAppE (associativityExpr e) t
    LetE {} -> undefined
    ReturnE tm e -> ReturnE tm (associativityExpr e)
    LiftE e tm1 tm2 -> LiftE (associativityExpr e) tm1 tm2
    _ -> expr

-- | TODO: name capturing
replaceExprIn :: (Expr, Expr) -> Expr -> Expr
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
           _ -> expr

