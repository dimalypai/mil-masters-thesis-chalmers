-- | Module defining Id monad transformations.
module MIL.Transformations.Id where

import Data.Generics.Uniplate.Data

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
exchangeExpr = descendBi f
  where
    f expr@(LetE varBinder e1 e2) =
      case e2 of
        LetE varBinder' e1' e2' | getBinderVar varBinder `isNotUsedIn` e1' ->
          case getTypeOf expr of
            TyApp (TyMonad (MTyMonad (SinMonad Id))) _ ->
              LetE varBinder' (exchangeExpr e1')
                (LetE varBinder (exchangeExpr e1) (exchangeExpr e2'))
            _ -> LetE varBinder (exchangeExpr e1) (exchangeExpr e2)
        _ -> LetE varBinder (exchangeExpr e1) (exchangeExpr e2)
    f expr = descend f expr

deadCodeElimination :: TyProgram -> TyProgram
deadCodeElimination (Program (typeDefs, funDefs)) =
  Program (typeDefs, map deadCodeEliminationFun funDefs)

deadCodeEliminationFun :: TyFunDef -> TyFunDef
deadCodeEliminationFun (FunDef funName funType funBody) =
  FunDef funName funType (deadCodeEliminationExpr funBody)

deadCodeEliminationExpr :: TyExpr -> TyExpr
deadCodeEliminationExpr = descendBi f
  where
    f expr@(LetE varBinder e1 e2) =
      case getTypeOf expr of
        TyApp (TyMonad (MTyMonad (SinMonad Id))) _ ->
          if getBinderVar varBinder `isNotUsedIn` e2
            then e2
            else LetE varBinder (deadCodeEliminationExpr e1) (deadCodeEliminationExpr e2)
        _ -> LetE varBinder (deadCodeEliminationExpr e1) (deadCodeEliminationExpr e2)
    f expr = descend f expr

