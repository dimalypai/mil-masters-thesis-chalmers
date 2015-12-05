-- | Module defining State monad transformations.
module MIL.Transformations.State where

import Data.Generics.Uniplate.Data

import MIL.AST
import MIL.AST.Helpers
import MIL.Transformations.Helpers

exchangeNew :: TyProgram -> TyProgram
exchangeNew (Program (typeDefs, funDefs)) =
  Program (typeDefs, map exchangeNewFun funDefs)

exchangeNewFun :: TyFunDef -> TyFunDef
exchangeNewFun (FunDef funName funType funBody) =
  FunDef funName funType (exchangeNewExpr funBody)

exchangeNewExpr :: TyExpr -> TyExpr
exchangeNewExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e2 of
        LetE varBinder' e1' e2' | getBinderVar varBinder `isNotUsedIn` e1' ->
          case (e1, e1') of
            (AppE (TypeAppE (VarE (VarBinder (Var "new_ref", _))) _) _,
             AppE (TypeAppE (VarE (VarBinder (Var "new_ref", _))) _) _) ->
              LetE varBinder' (exchangeNewExpr e1')
                (LetE varBinder (exchangeNewExpr e1) (exchangeNewExpr e2'))
            _ -> LetE varBinder (exchangeNewExpr e1) (exchangeNewExpr e2)
        _ -> LetE varBinder (exchangeNewExpr e1) (exchangeNewExpr e2)
    f expr = descend f expr


exchangeRead :: TyProgram -> TyProgram
exchangeRead (Program (typeDefs, funDefs)) =
  Program (typeDefs, map exchangeReadFun funDefs)

exchangeReadFun :: TyFunDef -> TyFunDef
exchangeReadFun (FunDef funName funType funBody) =
  FunDef funName funType (exchangeReadExpr funBody)

exchangeReadExpr :: TyExpr -> TyExpr
exchangeReadExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e2 of
        LetE varBinder' e1' e2' | getBinderVar varBinder `isNotUsedIn` e1' ->
          case (e1, e1') of
            (AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _) _,
             AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _) _) ->
              LetE varBinder' (exchangeReadExpr e1')
                (LetE varBinder (exchangeReadExpr e1) (exchangeReadExpr e2'))
            _ -> LetE varBinder (exchangeReadExpr e1) (exchangeReadExpr e2)
        _ -> LetE varBinder (exchangeReadExpr e1) (exchangeReadExpr e2)
    f expr = descend f expr


useRead :: TyProgram -> TyProgram
useRead (Program (typeDefs, funDefs)) =
  Program (typeDefs, map useReadFun funDefs)

useReadFun :: TyFunDef -> TyFunDef
useReadFun (FunDef funName funType funBody) =
  FunDef funName funType (useReadExpr funBody)

useReadExpr :: TyExpr -> TyExpr
useReadExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e2 of
        LetE varBinder' e1' e2' ->
          case (e1, e1') of
            (AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _)
                  (VarE (VarBinder (refVar1, _))),
             AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _)
                  (VarE (VarBinder (refVar2, _)))) | refVar1 == refVar2 ->
              LetE varBinder (useReadExpr e1)
                (LetE varBinder' (ReturnE (MTyMonad (SinMonad State))
                                    (VarE varBinder))
                   (useReadExpr e2'))
            _ -> LetE varBinder (useReadExpr e1) (useReadExpr e2)
        _ -> LetE varBinder (useReadExpr e1) (useReadExpr e2)
    f expr = descend f expr


useWrite :: TyProgram -> TyProgram
useWrite (Program (typeDefs, funDefs)) =
  Program (typeDefs, map useWriteFun funDefs)

useWriteFun :: TyFunDef -> TyFunDef
useWriteFun (FunDef funName funType funBody) =
  FunDef funName funType (useWriteExpr funBody)

useWriteExpr :: TyExpr -> TyExpr
useWriteExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e2 of
        LetE varBinder' e1' e2' ->
          case (e1, e1') of
            (AppE (AppE (TypeAppE (VarE (VarBinder (Var "write_ref", _))) _)
                        (VarE (VarBinder (refVar1, _)))) refContentExpr,
             AppE (TypeAppE (VarE (VarBinder (Var "read_ref", _))) _)
                  (VarE (VarBinder (refVar2, _)))) | refVar1 == refVar2 ->
              LetE varBinder (useWriteExpr e1)
                (LetE varBinder' (ReturnE (MTyMonad (SinMonad State))
                                    refContentExpr)
                   (useWriteExpr e2'))
            _ -> LetE varBinder (useWriteExpr e1) (useWriteExpr e2)
        _ -> LetE varBinder (useWriteExpr e1) (useWriteExpr e2)
    f expr = descend f expr

