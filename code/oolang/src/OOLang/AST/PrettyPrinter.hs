{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'Pretty' for syntax tree nodes.
--
-- Note: Instances are orphaned.
--
-- Note [Precedences and associativity]:
--
-- In order to get code with parentheses right, we use pairs of `hasPrec` and
-- `hasPrecAssoc` functions. The first one is reflexive and the second one is
-- *not*. The idea is that we use `hasPrecAssoc` function on the side on which
-- an operation associates and `hasPrec` - on another. See examples in
-- `instance Pretty Type`.
module OOLang.AST.PrettyPrinter (prPrint) where

import OOLang.AST
import OOLang.AST.Helpers
import OOLang.PrettyPrinter

-- See Note [Precedences and associativity]
instance Pretty Type where
  prPrn TyUnit   = text "Unit"
  prPrn TyBool   = text "Bool"
  prPrn TyInt    = text "Int"
  prPrn TyFloat  = text "Float"
  prPrn TyString = text "String"
  prPrn (TyClass className) = prPrn className
  prPrn t@(TyArrow t1 t2) =
    prPrnParens (t1 `typeHasLowerPrec` t) t1 <+>
    text "->" <+>
    prPrnParens (t2 `typeHasLowerPrecAssoc` t) t2
  prPrn (TyPure    t) = text "Pure"    <+> prPrnParens (not $ isAtomicType t) t
  prPrn (TyMaybe   t) = text "Maybe"   <+> prPrnParens (not $ isAtomicType t) t
  prPrn (TyMutable t) = text "Mutable" <+> prPrnParens (not $ isAtomicType t) t
  prPrn (TyRef     t) = text "Ref"     <+> prPrnParens (not $ isAtomicType t) t

-- Since 'TypeS' is a source representation of types (how a user entered them),
-- we don't need to do extra precedence and associativity handling during the
-- pretty printing. We just print it as it is. Thanks to 'SrcTyParen'.
instance Pretty (TypeS s) where
  prPrn (SrcTyUnit  _) = text "Unit"
  prPrn (SrcTyBool  _) = text "Bool"
  prPrn (SrcTyInt   _) = text "Int"
  prPrn (SrcTyFloat _) = text "Float"
  prPrn (SrcTyClass srcClassName) = prPrn (getClassName srcClassName)
  prPrn (SrcTyArrow _ st1 st2) = prPrn st1 <+> text "->" <+> prPrn st2
  prPrn (SrcTyPure    _ st) = text "Pure" <+> prPrn st
  prPrn (SrcTyMaybe   _ st) = text "Maybe" <+> prPrn st
  prPrn (SrcTyMutable _ st) = text "Mutable" <+> prPrn st
  prPrn (SrcTyRef     _ st) = text "Ref" <+> prPrn st
  prPrn (SrcTyParen   _ st) = parens (prPrn st)

instance Pretty Var where
  prPrn (Var varStr) = text varStr

instance Pretty ClassName where
  prPrn (ClassName classNameStr) = text classNameStr

instance Pretty FunName where
  prPrn (FunName funNameStr) = text funNameStr

instance Pretty MemberName where
  prPrn (MemberName memberNameStr) = text memberNameStr

