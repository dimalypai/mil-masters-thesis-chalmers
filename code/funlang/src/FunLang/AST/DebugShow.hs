{-# OPTIONS_GHC -fno-warn-orphans #-}

module FunLang.AST.DebugShow
  ( module FunLang.DebugShow
  ) where

import FunLang.AST
import FunLang.DebugShow

instance DebugShow s => DebugShow (Program s v) where
  showDebug (Program s typeDefs funDefs) =
    text "Program" <+> showDebug s $$
      (nest indLvl (showDebug typeDefs)) $$
      (nest indLvl (showDebug funDefs))

instance DebugShow s => DebugShow (TypeDef s) where
  showDebug (TypeDef s typeName typeVars conDefs) =
    text "TypeDef" <+> showDebug s $$
      (nest indLvl (showDebug typeName)) $$
      (nest indLvl (showDebug typeVars)) $$
      (nest indLvl (showDebug conDefs))

instance DebugShow s => DebugShow (FunDef s v) where
  showDebug (FunDef s _ _ _) =
    text "FunDef" <+> showDebug s

instance DebugShow TypeName where
  showDebug typeName = text $ show typeName

instance DebugShow TypeVar where
  showDebug typeVar = text $ show typeVar

instance DebugShow s => DebugShow (ConDef s) where
  showDebug (ConDef s _ _) = text "ConDef" <+> showDebug s

