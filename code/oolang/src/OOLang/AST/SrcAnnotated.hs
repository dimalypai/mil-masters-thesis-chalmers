{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'SrcAnnotated' and 'SrcAnnotated2' for
-- syntax tree nodes.
--
-- Note: Instances are orphaned.
module OOLang.AST.SrcAnnotated
  ( module OOLang.SrcAnnotated
  ) where

import OOLang.AST
import OOLang.SrcAnnotated

instance SrcAnnotated2 TopDef where
  ann2 (TopClassDef cd) = ann2 cd
  ann2 (TopFunDef fd)   = ann2 fd

instance SrcAnnotated2 ClassDef where
  ann2 (ClassDef s _ _ _) = s

instance SrcAnnotated2 FunDef where
  ann2 (FunDef s _ _ _ _) = s

instance SrcAnnotated2 Expr where
  ann2 (LitE lit) = ann2 lit
  ann2 (VarE s _) = s
  ann2 (BinOpE s _ _ _) = s
  ann2 (ParenE s _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyUnit s) = s
  ann (SrcTyBool s) = s
  ann (SrcTyInt s) = s
  ann (SrcTyClass cn) = ann2 cn
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyMaybe s _) = s
  ann (SrcTyMutable s _) = s
  ann (SrcTyRef s _) = s
  ann (SrcTyParen s _) = s

instance SrcAnnotated FunType where
  ann (FunType s _ _) = s

instance SrcAnnotated2 Declaration where
  ann2 (Decl s _ _) = s

instance SrcAnnotated2 Init where
  ann2 (Init s _ _) = s

instance SrcAnnotated VarBinder where
  ann (VarBinder s _ _) = s

