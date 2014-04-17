{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'SrcAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module FunLang.AST.SrcAnnotated
  ( module FunLang.SrcAnnotated
  ) where

import FunLang.AST
import FunLang.SrcAnnotated

instance SrcAnnotated (TopDef v) where
  ann (TopTypeDef typeDef) = ann typeDef
  ann (TopFunDef funDef)   = ann funDef

instance SrcAnnotated TypeDef where
  ann (TypeDef s _ _ _) = s

instance SrcAnnotated ConDef where
  ann (ConDef s _ _) = s

instance SrcAnnotated (FunDef v) where
  ann (FunDef s _ _ _) = s

instance SrcAnnotated Pattern where
  ann (LitP lit) = ann lit
  ann (VarP varBinder) = ann varBinder
  ann (ConP s _ _) = s
  ann (DefaultP s) = s
  ann (ParenP s _) = s

instance SrcAnnotated (Expr v) where
  ann (LitE lit) = ann lit
  ann (VarE s _) = s
  ann (LambdaE s _ _) = s
  ann (TypeLambdaE s _ _) = s
  ann (TypeAppE s _ _) = s
  ann (ConNameE conName) = ann conName
  ann (CaseE s _ _) = s
  ann (DoE s _) = s
  ann (BinOpE s _ _ _) = s
  ann (ParenE s _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyCon typeName) = ann typeName
  ann (SrcTyApp s _ _) = s
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyForAll s _ _) = s
  ann (SrcTyParen s _) = s

instance SrcAnnotated VarBinder where
  ann (VarBinder s _ _) = s

