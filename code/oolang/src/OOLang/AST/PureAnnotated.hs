{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'PureAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module OOLang.AST.PureAnnotated
  ( module OOLang.PureAnnotated
  ) where

import OOLang.AST
import OOLang.PureAnnotated

instance PureAnnotated Stmt where
  getPurityOf (DeclS _ decl) = getPurityOf decl
  getPurityOf (ExprS _ e) = getPurityOf e
  getPurityOf (AssignS _ _ _ _ p) = p
  getPurityOf (TryS _ tryStmts catchStmts finallyStmts) =
    and (map getPurityOf tryStmts ++
         map getPurityOf catchStmts ++
         map getPurityOf finallyStmts)

instance PureAnnotated Expr where
  getPurityOf (LitE {}) = True
  getPurityOf (VarE _ _ _ p) = p
  getPurityOf (MemberAccessE _ _ _ _ p) = p
  getPurityOf (MemberAccessMaybeE _ _ _ _ p) = p
  -- Constructor is always pure
  getPurityOf (ClassAccessE {}) = True
  -- Reference operations are always impure
  getPurityOf (NewRefE {}) = False
  getPurityOf (DerefE {}) = False
  getPurityOf (BinOpE _ _ _ _ _ p) = p
  getPurityOf (JustE _ _ e) = getPurityOf e
  getPurityOf (ParenE _ e) = getPurityOf e

instance PureAnnotated Declaration where
  getPurityOf (Decl _ _ _ p) = p

instance PureAnnotated Init where
  getPurityOf (Init _ _ e) = getPurityOf e

