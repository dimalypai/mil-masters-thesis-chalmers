-- | Module that defines an interface for working with source tree annotations.
module OOLang.SrcAnnotated where

import OOLang.SrcSpan

class SrcAnnotated ast where
  ann :: ast s -> s

-- | Pair is an instance of 'SrcAnnotated'. Used for data types which are
-- annotated as a pair.
instance SrcAnnotated ((,) a) where
  ann = snd

-- | 'SrcSpan' specific synonym for 'ann'.
getSrcSpan :: SrcAnnotated ast => ast SrcSpan -> SrcSpan
getSrcSpan = ann

