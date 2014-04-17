{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'Pretty' for syntax tree nodes.
--
-- Note: Instances are orphaned.
module FunLang.AST.PrettyPrinter (prPrint) where

import FunLang.AST
import FunLang.PrettyPrinter

instance Pretty Type where
  prPrn (TyVar typeVar) = prPrn typeVar
  prPrn (TyArrow t1 t2) = prPrn t1 <+> text "->" <+> prPrn t2  -- TODO: precedence
  prPrn (TyApp typeName args) = prPrn typeName <+> hsep (map prPrn args) -- TODO: precedence
  prPrn (TyForAll typeVar t) = text "forall" <+> prPrn typeVar <+> text "." <+> prPrn t

-- Since 'TypeS' is a source representation of types (how a user entered them),
-- we don't need to do extra precedence and associativity handling during the
-- pretty printing. We just print it as it is. Thanks to 'SrcTyParen'.
instance Pretty (TypeS s) where
  prPrn (SrcTyCon srcTypeName) = prPrn (getTypeName srcTypeName)
  prPrn (SrcTyApp _ st1 st2) = prPrn st1 <+> prPrn st2
  prPrn (SrcTyArrow _ st1 st2) = prPrn st1 <+> text "->" <+> prPrn st2
  prPrn (SrcTyForAll _ srcTypeVar st) = text "forall" <+> prPrn (getTypeVar srcTypeVar) <+> text "." <+> prPrn st
  prPrn (SrcTyParen _ st) = parens (prPrn st)

instance Pretty Kind where
  prPrn StarK = text "*"
  prPrn (k1 :=>: k2) = prPrn k1 <+> text "=>" <+> prPrn k2  -- TODO: parens

instance Pretty TypeVar where
  prPrn (TypeVar typeVarStr) = text typeVarStr

instance Pretty TypeName where
  prPrn (TypeName typeNameStr) = text typeNameStr

instance Pretty FunName where
  prPrn (FunName funNameStr) = text funNameStr

