-- | Module defining transformation helper functions.
module MIL.Transformations.Helpers where

import MIL.AST
import MIL.AST.Helpers

isNotUsedIn :: Var -> TyExpr -> Bool
v `isNotUsedIn` e =
  case e of
    VarE v' -> v /= getBinderVar v'
    LitE {} -> True
    ConNameE {} -> True
    LambdaE _varBinder e' -> v `isNotUsedIn` e'
    AppE e1 e2 -> (v `isNotUsedIn` e1) && (v `isNotUsedIn` e2)
    TypeLambdaE _typeVar e' -> v `isNotUsedIn` e'
    TypeAppE e' _t -> v `isNotUsedIn` e'
    LetE _varBinder e1 e2 -> (v `isNotUsedIn` e1) && (v `isNotUsedIn` e2)
    ReturnE _tm e' -> v `isNotUsedIn` e'
    LiftE e' _tm1 _tm2 -> v `isNotUsedIn` e'
    LetRecE binders e' -> all (\(_vb, be) -> v `isNotUsedIn` be) binders && (v `isNotUsedIn` e')
    CaseE e' caseAlts -> (v `isNotUsedIn` e') && all (\(CaseAlt (_p, ae)) -> v `isNotUsedIn` ae) caseAlts
    TupleE es -> all (\e' -> v `isNotUsedIn` e') es

