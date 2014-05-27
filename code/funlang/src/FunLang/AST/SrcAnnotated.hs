{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'SrcAnnotated' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module FunLang.AST.SrcAnnotated
  ( module FunLang.SrcAnnotated
  ) where

import FunLang.AST
import FunLang.SrcAnnotated

instance SrcAnnotated (TopDef a) where
  ann (TopTypeDef typeDef) = ann typeDef
  ann (TopFunDef funDef)   = ann funDef

instance SrcAnnotated TypeDef where
  ann (TypeDef s _ _ _) = s

instance SrcAnnotated ConDef where
  ann (ConDef s _ _) = s

instance SrcAnnotated (FunDef a) where
  ann (FunDef s _ _ _) = s

instance SrcAnnotated (Pattern a) where
  ann (LitP lit) = ann lit
  ann (VarP varBinder) = ann varBinder
  ann (ConP s _ _ _) = s
  ann (DefaultP s _) = s
  ann (ParenP s _) = s

instance SrcAnnotated (Expr a) where
  ann (LitE lit) = ann lit
  ann (VarE s _ _) = s
  ann (LambdaE s _ _ _) = s
  ann (TypeLambdaE s _ _ _) = s
  ann (TypeAppE s _ _ _) = s
  ann (ConNameE _ conName) = ann conName
  ann (CaseE s _ _ _) = s
  ann (DoE s _ _) = s
  ann (BinOpE s _ _ _ _) = s
  ann (ParenE s _) = s

instance SrcAnnotated (Literal a) where
  ann (UnitLit s _) = s
  ann (IntLit s _ _) = s
  ann (FloatLit s _ _ _) = s
  ann (StringLit s _ _) = s

instance SrcAnnotated (CaseAlt a) where
  ann (CaseAlt s _ _) = s

instance SrcAnnotated TypeS where
  ann (SrcTyCon typeName) = ann typeName
  ann (SrcTyApp s _ _) = s
  ann (SrcTyArrow s _ _) = s
  ann (SrcTyForAll s _ _) = s
  ann (SrcTyParen s _) = s

instance SrcAnnotated (VarBinder a) where
  ann (VarBinder s _ _ _) = s

