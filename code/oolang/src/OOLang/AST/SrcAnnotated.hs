{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'SrcAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module OOLang.AST.SrcAnnotated
  ( module OOLang.SrcAnnotated
  ) where

import OOLang.AST
import OOLang.SrcAnnotated

instance SrcAnnotated (TopDef v) where
  ann (TopClassDef cd) = ann cd
  ann (TopFunDef fd)   = ann fd

instance SrcAnnotated (ClassDef v) where
  ann (ClassDef s _ _ _) = s

instance SrcAnnotated (FunDef v) where
  ann (FunDef s _ _ _) = s

instance SrcAnnotated (Stmt v) where
  ann (DeclS s _) = s
  ann (ExprS s _) = s

instance SrcAnnotated (Expr v) where
  ann (LitE lit) = ann lit
  ann (VarE s _) = s
  ann (MemberAccessE s _ _) = s
  ann (MemberAccessMaybeE s _ _) = s
  ann (ClassAccessE s _ _) = s
  ann (DerefE s _) = s
  ann (BinOpE s _ _ _) = s
  ann (JustE s _) = s
  ann (ParenE s _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyUnit s) = s
  ann (SrcTyBool s) = s
  ann (SrcTyInt s) = s
  ann (SrcTyClass cn) = ann cn
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyPure s _) = s
  ann (SrcTyMaybe s _) = s
  ann (SrcTyMutable s _) = s
  ann (SrcTyRef s _) = s
  ann (SrcTyParen s _) = s

instance SrcAnnotated FunType where
  ann (FunType s _ _) = s

instance SrcAnnotated (Declaration v) where
  ann (Decl s _ _) = s

instance SrcAnnotated (Init v) where
  ann (Init s _ _) = s

instance SrcAnnotated VarBinder where
  ann (VarBinder s _ _) = s

