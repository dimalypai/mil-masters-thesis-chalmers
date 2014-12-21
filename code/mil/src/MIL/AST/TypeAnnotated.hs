{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module that defines an interface for working with type annotations of the
-- AST. Note that only typed representations should be instances of
-- 'TypeAnnotated'.
module MIL.AST.TypeAnnotated where

import MIL.AST
import MIL.BuiltIn

-- | AST nodes for which it makes sense to have a type should be an instance of
-- the 'TypeAnnotated' type class.
-- Note: It does not handle any error cases, expressions should be well-typed.
class TypeAnnotated ast where
  getTypeOf :: ast -> Type

instance TypeAnnotated TyExpr where
  getTypeOf (LitE lit) = getTypeOf lit
  getTypeOf (VarE varBinder) = getTypeOf varBinder
  getTypeOf (LambdaE varBinder bodyExpr) =
    TyArrow (getTypeOf varBinder) (getTypeOf bodyExpr)
  getTypeOf (AppE appExpr _argExpr) =
    let TyArrow _paramType resultType = getTypeOf appExpr
    in resultType

instance TypeAnnotated Literal where
  getTypeOf UnitLit      = unitType
  getTypeOf IntLit    {} = intType
  getTypeOf FloatLit  {} = floatType
  getTypeOf CharLit   {} = charType

instance TypeAnnotated TyCaseAlt where
  getTypeOf (CaseAlt (_,e)) = getTypeOf e

instance TypeAnnotated TyPattern where
  getTypeOf (LitP lit) = getTypeOf lit
  getTypeOf (VarP tyVarBinder) = getTypeOf tyVarBinder

instance TypeAnnotated TyVarBinder where
  getTypeOf (VarBinder (_,t)) = t

