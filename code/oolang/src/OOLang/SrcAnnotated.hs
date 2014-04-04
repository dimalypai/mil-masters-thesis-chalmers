-- | Module that defines an interface for working with source tree annotations.
module OOLang.SrcAnnotated where

import OOLang.SrcSpan

-- | AST nodes with one type parameter (s) should be an instance of the
-- 'SrcAnnotated' type class.
class SrcAnnotated ast where
  ann :: ast s -> s

-- | AST nodes with two type parameters (s and v) should be an instance of the
-- 'SrcAnnotated2' type class.
class SrcAnnotated2 ast where
  ann2 :: ast s v -> s

-- | Pair is an instance of 'SrcAnnotated2'. Used for data types which are
-- annotated as a pair.
instance SrcAnnotated2 (,) where
  ann2 = fst

-- | 'SrcSpan' specific synonym for 'ann'.
getSrcSpan :: SrcAnnotated ast => ast SrcSpan -> SrcSpan
getSrcSpan = ann

-- | 'SrcSpan' specific synonym for 'ann2'.
getSrcSpan2 :: SrcAnnotated2 ast => ast SrcSpan v -> SrcSpan
getSrcSpan2 = ann2

