{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'TypeAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module OOLang.AST.TypeAnnotated
  ( module OOLang.TypeAnnotated
  ) where

import OOLang.AST
import OOLang.TypeAnnotated

instance TypeAnnotated Stmt where
  getTypeOf (DeclS {}) = TyUnit
  getTypeOf (ExprS _ e) = getTypeOf e
  getTypeOf (AssignS {}) = TyUnit
  -- Type of try block should be the same as the type of catch block.
  -- finally does not return a value, so it's type is not considered.
  getTypeOf (TryS _ tryStmts _ _) = getTypeOf $ last tryStmts
  getTypeOf (ThrowS _ t _) = t

instance TypeAnnotated Expr where
  getTypeOf (LitE lit) = getTypeOf lit
  getTypeOf (VarE _ t _ _) = t
  getTypeOf (MemberAccessE _ t _ _ _) = t
  getTypeOf (MemberAccessMaybeE _ t _ _ _) = t
  getTypeOf (ClassAccessE _ t _ _) = t
  getTypeOf (NewRefE _ t _) = t
  getTypeOf (DerefE _ t _) = t
  getTypeOf (BinOpE _ t _ _ _ _) = t
  getTypeOf (JustE _ t _) = t
  getTypeOf (ParenE _ e) = getTypeOf e

instance TypeAnnotated Literal where
  getTypeOf (UnitLit _ t) = t
  getTypeOf (BoolLit _ t _) = t
  getTypeOf (IntLit _ t _) = t
  getTypeOf (FloatLit _ t _ _) = t
  getTypeOf (StringLit _ t _) = t
  getTypeOf (NothingLit _ t _) = t

instance TypeAnnotated Init where
  getTypeOf (Init _ _ e) = getTypeOf e

instance TypeAnnotated VarBinder where
  getTypeOf (VarBinder _ t _ _) = t

