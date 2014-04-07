{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'Pretty' for syntax tree nodes.
--
-- Note: Instances are orphaned.
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
      nest indLvl prConDefs <> semi
    where prConDefs =
            if null consWithBars
              then empty
              -- This hacking is going on for nice indentation
              else vsep ((equals <+> head consWithBars) : tail consWithBars)
          consWithBars = intersperse (text "| ") (map prPrn conDefs)

instance Pretty ConDef where
  prPrn (ConDef conName conFields) =
    prPrn conName <+> hsep (map (\t -> prPrnParens (not $ isAtomicType t) t) conFields)

instance Pretty FunDef where
  prPrn (FunDef funName funType expr) =
    prPrn funName <+> colon <+> prPrn funType <+> equals <+> prPrn expr <> semi

instance Pretty Expr where
  prPrn (LitE lit) = prPrn lit
  prPrn (VarE (VarBinder (v, _))) = prPrn v
  prPrn (LambdaE varBind e) = text "\\" <> prPrn varBind <+> text "->" <+> prPrn e
  prPrn e@(AppE e1 e2) =
    -- This relies on exprHasLowerPrec using < and not <=.
    -- In this way left associativity of application works fine.
    prPrnParens (e1 `exprHasLowerPrec` e) e1 <+> prPrnParens (e2 `exprHasLowerPrec` e) e2

instance Pretty Literal where
  prPrn UnitLit    = text "unit"
  prPrn (IntLit i) = int i

instance Pretty Type where
  prPrn (TyTypeCon typeName _) = prPrn typeName
  prPrn (TyVar typeVar) = prPrn typeVar
  prPrn t@(TyArrow t1 t2) =
    prPrnParens (t1 `typeHasLowerPrec` t) t1 <+> text "->" <+> prPrn t2
  prPrn (TyForAll typeVar t) =
    text "forall" <+> prPrn typeVar <+> text "." <+> prPrn t
  prPrn t@(TyApp t1 t2) =
    -- We apply only type constructors at the moment, so this is fine (not having t1 in parens).
    -- Probably, it should be done as for expressions instead.
    prPrn t1 <+> prPrnParens (t2 `typeHasLowerPrec` t) t2

instance Pretty VarBinder where
  prPrn (VarBinder (v, t)) = parens $ prPrn v <+> colon <+> prPrn t

instance Pretty Var where
  prPrn (Var varStr) = text varStr

instance Pretty TypeVar where
  prPrn (TypeVar typeVarStr) = text typeVarStr

instance Pretty TypeName where
  prPrn (TypeName typeNameStr) = text typeNameStr

instance Pretty ConName where
  prPrn (ConName conNameStr) = text conNameStr

instance Pretty FunName where
  prPrn (FunName funNameStr) = text funNameStr

-- | Indentation level for code.
indLvl :: Int
indLvl = 2

