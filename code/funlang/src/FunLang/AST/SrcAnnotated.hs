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

instance SrcAnnotated2 Expr where
  ann2 (LitE lit) = ann2 lit
  ann2 (VarE s _) = s
  ann2 (LambdaE s _ _) = s
  ann2 (TypeLambdaE s _ _) = s
  ann2 (TypeAppE s _ _) = s
  ann2 (DoE s _) = s
  ann2 (BinOpE s _ _ _) = s
  ann2 (ParenE s _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyCon typeName) = ann2 typeName
  ann (SrcTyApp s _ _) = s
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyForAll s _ _) = s
  ann (SrcTyParen s _) = s

instance SrcAnnotated VarBinder where
  ann (VarBinder s _ _) = s

