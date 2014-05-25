{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'SrcAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module OOLang.AST.SrcAnnotated
  ( module OOLang.SrcAnnotated
  ) where

import OOLang.AST
import OOLang.SrcAnnotated

instance SrcAnnotated (TopDef a) where
  ann (TopClassDef cd) = ann cd
  ann (TopFunDef fd)   = ann fd

instance SrcAnnotated (ClassDef a) where
  ann (ClassDef s _ _ _) = s

instance SrcAnnotated (FunDef a) where
  ann (FunDef s _ _ _) = s

instance SrcAnnotated (Stmt a) where
  ann (DeclS s _) = s
  ann (ExprS s _) = s
  ann (AssignS s _ _ _ _) = s

instance SrcAnnotated (Expr a) where
  ann (LitE lit) = ann lit
  ann (VarE s _ _ _) = s
  ann (MemberAccessE s _ _ _ _) = s
  ann (MemberAccessMaybeE s _ _ _ _) = s
  ann (ClassAccessE s _ _ _) = s
  ann (NewRefE s _ _) = s
  ann (DerefE s _ _) = s
  ann (BinOpE s _ _ _ _ _) = s
  ann (JustE s _ _) = s
  ann (ParenE s _) = s

instance SrcAnnotated (Literal a) where
  ann (UnitLit s _) = s
  ann (BoolLit s _ _) = s
  ann (IntLit s _ _) = s
  ann (FloatLit s _ _ _) = s
  ann (StringLit s _ _) = s
  ann (NothingLit s _ _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyUnit s) = s
  ann (SrcTyBool s) = s
  ann (SrcTyInt s) = s
  ann (SrcTyFloat s) = s
  ann (SrcTyClass cn) = ann cn
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyPure s _) = s
  ann (SrcTyMaybe s _) = s
  ann (SrcTyMutable s _) = s
  ann (SrcTyRef s _) = s
  ann (SrcTyParen s _) = s

instance SrcAnnotated FunType where
  ann (FunType s _ _) = s

instance SrcAnnotated (Declaration a) where
  ann (Decl s _ _ _) = s

instance SrcAnnotated (Init a) where
  ann (Init s _ _) = s

instance SrcAnnotated VarBinder where
  ann (VarBinder s _ _) = s

