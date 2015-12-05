-- | Module defining lift operator transformations.
module MIL.Transformations.Lift where

import Data.Generics.Uniplate.Data

import MIL.AST
import MIL.TypeChecker.AlphaEq

liftIdentity :: TyProgram -> TyProgram
liftIdentity (Program (typeDefs, funDefs)) =
  Program (typeDefs, map liftIdentityFun funDefs)

liftIdentityFun :: TyFunDef -> TyFunDef
liftIdentityFun (FunDef funName funType funBody) =
  FunDef funName funType (liftIdentityExpr funBody)

liftIdentityExpr :: TyExpr -> TyExpr
liftIdentityExpr = descendBi f
  where
    f (LiftE e tm1 tm2) =
      if tm1 `alphaEq` tm2
        then e
        else LiftE (liftIdentityExpr e) tm1 tm2
    f expr = descend f expr

composeLift :: TyProgram -> TyProgram
composeLift (Program (typeDefs, funDefs)) =
  Program (typeDefs, map composeLiftFun funDefs)

composeLiftFun :: TyFunDef -> TyFunDef
composeLiftFun (FunDef funName funType funBody) =
  FunDef funName funType (composeLiftExpr funBody)

composeLiftExpr :: TyExpr -> TyExpr
composeLiftExpr = descendBi f
  where
    f (LiftE e tm1 tm2) =
      case e of
        LiftE e' tm1' _ -> LiftE e' tm1' tm2
        _ -> LiftE (composeLiftExpr e) tm1 tm2
    f expr = descend f expr

