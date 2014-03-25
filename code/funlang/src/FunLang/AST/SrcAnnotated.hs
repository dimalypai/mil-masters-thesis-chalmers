{-# OPTIONS_GHC -fno-warn-orphans #-}

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

