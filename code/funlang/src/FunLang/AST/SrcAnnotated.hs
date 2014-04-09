{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'SrcAnnotated' and 'SrcAnnotated2' for
-- syntax tree nodes.
--
-- Note: Instances are orphaned.
module FunLang.AST.SrcAnnotated
  ( module FunLang.SrcAnnotated
  ) where

import FunLang.AST
import FunLang.SrcAnnotated

instance SrcAnnotated2 TopDef where
  ann2 (TopTypeDef td) = ann td
  ann2 (TopFunDef fd)  = ann2 fd

instance SrcAnnotated TypeDef where
  ann (TypeDef s _ _ _) = s

instance SrcAnnotated ConDef where
  ann (ConDef s _ _) = s

instance SrcAnnotated2 FunDef where
  ann2 (FunDef s _ _ _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyApp s _ _) = s
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyForAll s _ _) = s

