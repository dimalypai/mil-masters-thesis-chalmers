module MIL.Transformations.IdExprMonadElimination
  ( idExprMonadElimination
  ) where

import MIL.AST

idExprMonadElimination :: TyProgram -> TyProgram
idExprMonadElimination (Program (typeDefs, aliasDefs, funDefs)) =
  Program (typeDefs, aliasDefs, map idExprMEliminateFunDef funDefs)

-- TODO: very very naive
idExprMEliminateFunDef :: TyFunDef -> TyFunDef
idExprMEliminateFunDef funDef@(FunDef funName funType bodyExpr) =
  case funType of
    TyApp (TyMonad (MTyMonad (TypeMMilMonad Id))) funResType ->
      case bodyExpr of
        ReturnE _ retExpr -> FunDef funName funResType retExpr
    _ -> funDef

