{-# OPTIONS_GHC -fno-warn-orphans #-}

module MIL.AST.PrettyPrinter
  ( prPrint
  ) where

import MIL.AST
import MIL.PrettyPrinter

instance Pretty Program where
  prPrn (Program (typeDefs, funDefs)) =
    vsepBig (map prPrn typeDefs) $+$
    vsepBig (map prPrn funDefs)

instance Pretty TypeDef where
  prPrn (TypeDef typeName typeVars conDefs) =
      text "type" <+> prPrn typeName <+> hsep (map prPrn typeVars) $+$
      nest indLvl prConDefs
    where prConDefs =
            if null consWithBars
              then empty
              else vsep ((equals <+> head consWithBars) : tail consWithBars)
          consWithBars = intersperse (text "| ") (map prPrn conDefs)

instance Pretty ConDef where
  prPrn (ConDef conName conFields) = prPrn conName <+> hsep (map prPrn conFields)

instance Pretty FunDef where
  prPrn (FunDef funName funType expr) =
    prPrn funName <+> colon <+> prPrn funType <+> equals <+> prPrn expr

instance Pretty Expr where
  prPrn (LitE lit) = prPrn lit

instance Pretty Literal where
  prPrn UnitLit    = text "unit"
  prPrn (IntLit i) = int i

instance Pretty Type where
  prPrn (TyVar typeVar) = prPrn typeVar
  prPrn (TyArrow t1 t2) = prPrn t1 <+> text "->" <+> prPrn t2

instance Pretty TypeVar where
  prPrn (TypeVar typeVarStr) = text typeVarStr

instance Pretty TypeName where
  prPrn (TypeName typeNameStr) = text typeNameStr

instance Pretty ConName where
  prPrn (ConName conNameStr) = text conNameStr

instance Pretty FunName where
  prPrn (FunName funNameStr) = text funNameStr

indLvl :: Int
indLvl = 2

