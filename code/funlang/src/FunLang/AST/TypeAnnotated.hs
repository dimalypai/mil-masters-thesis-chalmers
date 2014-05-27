{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'TypeAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module FunLang.AST.TypeAnnotated
  ( module FunLang.TypeAnnotated
  ) where

import FunLang.AST
import FunLang.TypeAnnotated

instance TypeAnnotated Pattern where
  getTypeOf (LitP lit) = getTypeOf lit
  getTypeOf (VarP varBinder) = getTypeOf varBinder
  getTypeOf (ConP _ t _ _) = t
  getTypeOf (DefaultP _ t) = t
  getTypeOf (ParenP _ pat) = getTypeOf pat

instance TypeAnnotated Expr where
  getTypeOf (LitE lit) = getTypeOf lit
  getTypeOf (VarE _ t _) = t
  getTypeOf (LambdaE _ t _ _) = t
  getTypeOf (TypeLambdaE _ t _ _) = t
  getTypeOf (TypeAppE _ t _ _) = t
  getTypeOf (ConNameE t _) = t
  getTypeOf (CaseE _ t _ _) = t
  getTypeOf (LetE _ _ e) = getTypeOf e
  getTypeOf (DoE _ t _) = t
  getTypeOf (BinOpE _ t _ _ _) = t
  getTypeOf (ThrowE _ t _) = t
  getTypeOf (ParenE _ e) = getTypeOf e

instance TypeAnnotated Literal where
  getTypeOf (UnitLit _ t) = t
  getTypeOf (IntLit _ t _) = t
  getTypeOf (FloatLit _ t _ _) = t
  getTypeOf (StringLit _ t _) = t

-- | Bind statement should not be asked for type.
instance TypeAnnotated Stmt where
  getTypeOf (ExprS _ e) = getTypeOf e
  getTypeOf (ReturnS _ t _) = t

instance TypeAnnotated VarBinder where
  getTypeOf (VarBinder _ t _ _) = t

