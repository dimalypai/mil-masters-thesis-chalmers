{-# OPTIONS_GHC -fno-warn-orphans #-}

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

instance SrcAnnotated SrcType where
  ann (SrcTyUnit s) = s
  ann (SrcTyBool s) = s
  ann (SrcTyInt s) = s

instance SrcAnnotated VarBinder where
  ann (VarBinder s _ _) = s

