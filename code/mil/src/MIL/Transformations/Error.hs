-- | Module defining Error (exceptions) transformations.
module MIL.Transformations.Error where

import Data.Generics.Uniplate.Data

import MIL.AST

eliminateThrowCatch :: TyProgram -> TyProgram
eliminateThrowCatch (Program (typeDefs, funDefs)) =
  Program (typeDefs, map eliminateThrowCatchFun funDefs)

eliminateThrowCatchFun :: TyFunDef -> TyFunDef
eliminateThrowCatchFun (FunDef funName funType funBody) =
  FunDef funName funType (eliminateThrowCatchExpr funBody)

eliminateThrowCatchExpr :: TyExpr -> TyExpr
eliminateThrowCatchExpr = descendBi f
  where
    f (AppE e1 e2) =
      case e1 of
        AppE (TypeAppE (TypeAppE (VarE (VarBinder (Var catchName, _))) (TyTypeCon (TypeName "Unit"))) _)
             (AppE (TypeAppE (TypeAppE (VarE (VarBinder (Var "throw_error", _))) _) _) _) |
             catchName == "catch_error_1" || catchName == "catch_error_2" ->
          let (LambdaE _ handlerBody) = e2
          in eliminateThrowCatchExpr handlerBody
        _ -> AppE (eliminateThrowCatchExpr e1) (eliminateThrowCatchExpr e2)
    f expr = descend f expr

