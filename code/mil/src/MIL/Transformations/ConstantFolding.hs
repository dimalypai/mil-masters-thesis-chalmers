-- | Module defining constant folding transformations.
module MIL.Transformations.ConstantFolding where

import Data.Generics.Uniplate.Data

import MIL.AST

foldConstants :: TyProgram -> TyProgram
foldConstants (Program (typeDefs, funDefs)) =
  Program (typeDefs, map foldConstantsFun funDefs)

foldConstantsFun :: TyFunDef -> TyFunDef
foldConstantsFun (FunDef funName funType funBody) =
  FunDef funName funType (foldConstantsExpr funBody)

foldConstantsExpr :: TyExpr -> TyExpr
foldConstantsExpr = descendBi f
  where
    f expr@(AppE e1 e2) =
      case (e1, e2) of
        (AppE (VarE (VarBinder (Var funName, _))) (LitE lit1), LitE lit2) ->
          case funName of
            "add_int" ->
              case (lit1, lit2) of
                (IntLit i1, IntLit i2) -> LitE $ IntLit (i1 + i2)
                _ -> error "foldConstantsExpr: Incorrect literals for add_int"
            "add_float" ->
              case (lit1, lit2) of
                (FloatLit f1, FloatLit f2) -> LitE $ FloatLit (f1 + f2)
                _ -> error "foldConstantsExpr: Incorrect literals for add_float"
            "sub_int" ->
              case (lit1, lit2) of
                (IntLit i1, IntLit i2) -> LitE $ IntLit (i1 - i2)
                _ -> error "foldConstantsExpr: Incorrect literals for sub_int"
            "sub_float" ->
              case (lit1, lit2) of
                (FloatLit f1, FloatLit f2) -> LitE $ FloatLit (f1 - f2)
                _ -> error "foldConstantsExpr: Incorrect literals for sub_float"
            "mul_int" ->
              case (lit1, lit2) of
                (IntLit i1, IntLit i2) -> LitE $ IntLit (i1 * i2)
                _ -> error "foldConstantsExpr: Incorrect literals for mul_int"
            "mul_float" ->
              case (lit1, lit2) of
                (FloatLit f1, FloatLit f2) -> LitE $ FloatLit (f1 * f2)
                _ -> error "foldConstantsExpr: Incorrect literals for mul_float"
            _ -> expr
        _ -> AppE (foldConstantsExpr e1) (foldConstantsExpr e2)
    f expr = descend f expr

