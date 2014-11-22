{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module containing instances of 'Pretty' for syntax tree nodes.
--
-- Note: Instances are orphaned.
--
-- Note [Precedences and associativity]:
-- In order to get code with parentheses right, we use pairs of `hasPrec` and
-- `hasPrecAssoc` functions. The first one is reflexive and the second one is
-- *not*. The idea is that we use `hasPrecAssoc` function on the side on which
-- an operation associates and `hasPrec` - on another. See examples in
-- `instance Pretty Expr` and `instance Pretty Type`.
module MIL.AST.PrettyPrinter
  ( prPrint
  ) where

import MIL.AST
import MIL.PrettyPrinter

instance (IsType t, Pretty mt, Pretty t) => Pretty (Program Var ct mt t) where
  prPrn (Program (typeDefs, funDefs)) =
    vsepBig (map prPrn typeDefs) $+$
    vsepBig (map prPrn funDefs)

instance (IsType t, Pretty t) => Pretty (TypeDef t) where
  prPrn (TypeDef typeName typeVars conDefs) =
      text "type" <+> prPrn typeName <+> hsep (map prPrn typeVars) $+$
      nest indLvl prConDefs <> semi
    where prConDefs =
            if null consWithBars
              then empty
              -- This hacking is going on for nice indentation
              else vsep ((equals <+> head consWithBars) : tail consWithBars)
          consWithBars = intersperse (text "| ") (map prPrn conDefs)

instance (IsType t, Pretty t) => Pretty (ConDef t) where
  prPrn (ConDef conName conFields) =
    prPrn conName <+> hsep (map (\t -> prPrnParens (not $ isAtomicType t) t) conFields)

instance (Pretty mt, Pretty t) => Pretty (FunDef Var ct mt t) where
  prPrn (FunDef funName funType expr) =
    prPrn funName <+> colon <+> prPrn funType <+> equals $+$
    nest indLvl (prPrn expr <> semi)

-- See Note [Precedences and associativity]
instance (Pretty mt, Pretty t) => Pretty (Expr Var ct mt t) where
  prPrn (LitE lit) = prPrn lit
  prPrn (VarE v) = prPrn v
  prPrn (LambdaE varBind e) = text "\\" <> prPrn varBind <+> text "->" <+> prPrn e
  prPrn e@(AppE e1 e2) =
    prPrnParens (e1 `exprHasLowerPrecAssoc` e) e1 <+>
    prPrnParens (e2 `exprHasLowerPrec` e) e2
  prPrn (TypeLambdaE typeVar e) = text "/\\" <> prPrn typeVar <+> text "." <+> prPrn e
  prPrn e@(TypeAppE e1 t) =
    prPrnParens (e1 `exprHasLowerPrecAssoc` e) e1 <+>
    brackets (prPrn t)
  prPrn (ConNameE conName _) = prPrn conName
  prPrn (LetE varBind e1 e2) =
    text "let" <+> prPrn varBind <+> text "<-" <+> prPrn e1 <+> text "in" $+$
    nest indLvl (prPrn e2)
  prPrn e@(ReturnE m e1) =
    text "return" <+> brackets (prPrn m) <+>
    prPrnParens (e1 `exprHasLowerPrec` e) e1
  prPrn e@(LiftE e1 tm1 tm2) =
    text "lift" <+> brackets (prPrn tm1 <+> text "->" <+> prPrn tm2) <+>
    prPrnParens (e1 `exprHasLowerPrec` e) e1
  prPrn (LetRecE varBinds e) =
    text "let rec" <+>
      (hsep $ punctuate semi $
         map (\(varBind, bindE) -> prPrn varBind <+> text "<-" <+> prPrn bindE) varBinds) <+>
      text "in" $+$
      nest indLvl (prPrn e)
  prPrn (CaseE e caseAlts) =
    text "case" <+> prPrn e <+> text "of" $+$
    nest indLvl (vsep $ map prPrn caseAlts) $+$
    text "end"
  prPrn (TupleE tElems) = braces (hsep $ punctuate comma $ map prPrn tElems)

instance Pretty Literal where
  prPrn UnitLit       = text "unit"
  prPrn (IntLit i)    = int i
  prPrn (FloatLit f)  = double f
  prPrn (CharLit c)   = quotes (char c)

instance (Pretty mt, Pretty t) => Pretty (CaseAlt Var ct mt t) where
  prPrn (CaseAlt (pat, e)) = text "|" <+> prPrn pat <+> text "=>" <+> prPrn e

instance Pretty t => Pretty (Pattern t) where
  prPrn (LitP lit) = prPrn lit
  prPrn (VarP varBinder) = prPrn varBinder
  prPrn (ConP conName varBinders) = prPrn conName <+> hsep (map prPrn varBinders)
  prPrn (TupleP varBinders) = braces (hsep $ punctuate comma $ map prPrn varBinders)
  prPrn DefaultP = text "_"

instance Pretty SrcType where
  prPrn (SrcTyTypeCon typeName) = prPrn typeName
  prPrn st@(SrcTyArrow st1 st2) =
    prPrnParens (st1 `typeHasLowerPrec` st) st1 <+>
    text "->" <+>
    prPrnParens (st2 `typeHasLowerPrecAssoc` st) st2
  prPrn (SrcTyForAll typeVar st) =
    text "forall" <+> prPrn typeVar <+> text "." <+> prPrn st
  prPrn st@(SrcTyApp st1 st2) =
    prPrnParens (st1 `typeHasLowerPrecAssoc` st) st1 <+>
    prPrnParens (st2 `typeHasLowerPrec` st) st2
  prPrn (SrcTyTuple elemSrcTypes) = braces (hsep $ punctuate comma $ map prPrn elemSrcTypes)
  prPrn (SrcTyMonadCons st1 st2) = prPrn st1 <+> text ":::" <+> prPrn st2  -- TODO: associativity?

-- See Note [Precedences and associativity]
instance Pretty Type where
  prPrn (TyTypeCon typeName) = prPrn typeName
  prPrn (TyVar typeVar) = prPrn typeVar
  prPrn t@(TyArrow t1 t2) =
    prPrnParens (t1 `typeHasLowerPrec` t) t1 <+>
    text "->" <+>
    prPrnParens (t2 `typeHasLowerPrecAssoc` t) t2
  prPrn (TyForAll typeVar t) =
    text "forall" <+> prPrn typeVar <+> text "." <+> prPrn t
  prPrn t@(TyApp t1 t2) =
    prPrnParens (t1 `typeHasLowerPrecAssoc` t) t1 <+>
    prPrnParens (t2 `typeHasLowerPrec` t) t2
  prPrn (TyTuple elemTypes) = braces (hsep $ punctuate comma $ map prPrn elemTypes)
  prPrn (TyMonad tm) = prPrn tm

instance Pretty MonadType where
  prPrn (MTyMonad m) = prPrn m
  prPrn (MTyMonadCons m mt) = prPrn m <+> text ":::" <+> prPrn mt

instance Pretty SingleMonad where
  prPrn (SinMonad m) = prPrn m
  prPrn (SinMonadApp m t) = prPrn m <+> prPrn t  -- TODO

instance Pretty Kind where
  prPrn StarK = text "*"
  prPrn (k1 :=>: k2) = prPrn k1 <+> text "=>" <+> prPrn k2  -- TODO: parens

instance Pretty t => Pretty (VarBinder t) where
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

instance Pretty MilMonad where
  prPrn Id      = text "Id"
  prPrn State   = text "State"
  prPrn Error   = text "Error"
  prPrn NonTerm = text "NonTerm"
  prPrn IO      = text "IO"

-- | Indentation level for code.
indLvl :: Int
indLvl = 2

