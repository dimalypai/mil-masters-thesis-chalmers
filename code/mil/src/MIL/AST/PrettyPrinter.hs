{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- See Note [Precedences and associativity]
instance Pretty Expr where
  prPrn (LitE lit) = prPrn lit
  prPrn (VarE (VarBinder (v, _))) = prPrn v
  prPrn (LambdaE varBind e) = text "\\" <> prPrn varBind <+> text "->" <+> prPrn e
  prPrn e@(AppE e1 e2) =
    prPrnParens (e1 `exprHasLowerPrecAssoc` e) e1 <+>
    prPrnParens (e2 `exprHasLowerPrec` e) e2
  prPrn (TypeLambdaE typeVar e) = text "/\\" <> prPrn typeVar <+> text "." <+> prPrn e
  prPrn (TypeAppE e t) = prPrn e <+> brackets (prPrn t)
  prPrn (ConNameE conName) = prPrn conName
  prPrn e@(NewRefE e1) = text "new" <+> prPrnParens (e1 `exprHasLowerPrec` e) e1
  prPrn e@(DerefE e1) = text "!" <+> prPrnParens (e1 `exprHasLowerPrec` e) e1
  prPrn e@(AssignRefE e1 e2) =
    prPrnParens (e1 `exprHasLowerPrec` e) e1 <+>
    text ":=" <+>
    prPrnParens (e2 `exprHasLowerPrecAssoc` e) e2
  prPrn (LetE varBind e1 e2) =
    text "let" <+> prPrn varBind <+> text "<-" <+> prPrn e1 <+> text "in" <+> prPrn e2
  prPrn (ReturnE e m) = text "return" <+> prPrn e

instance Pretty Literal where
  prPrn UnitLit    = text "unit"
  prPrn (IntLit i) = int i

-- See Note [Precedences and associativity]
instance Pretty Type where
  prPrn (TyTypeCon typeName _) = prPrn typeName
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

