module MIL.Transformations.IdExprMonadElimination
  ( idExprMonadElimination
  ) where

import MIL.AST

idExprMonadElimination :: Program -> Program
idExprMonadElimination (Program (typeDefs, aliasDefs, funDefs)) =
  Program (typeDefs, aliasDefs, map idExprMEliminateFunDef funDefs)

-- TODO: very very naive
idExprMEliminateFunDef :: FunDef -> FunDef
idExprMEliminateFunDef funDef@(FunDef funName funType bodyExpr) =
  case funType of
    TyApp (TyMonad (MTyMonad Id)) funResType ->
      case bodyExpr of
        ReturnE _ retExpr -> FunDef funName funResType retExpr
    _ -> funDef

