-- | Module that defines an interface for working with source tree annotations.
module FunLang.SrcAnnotated where

import FunLang.SrcSpan

-- | AST nodes with one type parameter (s) should be an instance of the
-- 'SrcAnnotated' type class.
class SrcAnnotated ast where
  ann :: ast s -> s

-- | Pair is an instance of 'SrcAnnotated'. Used for data types which are
-- annotated as a pair.
instance SrcAnnotated ((,) v) where
  ann = snd

-- | 'SrcSpan' specific synonym for 'ann'.
getSrcSpan :: SrcAnnotated ast => ast SrcSpan -> SrcSpan
getSrcSpan = ann

