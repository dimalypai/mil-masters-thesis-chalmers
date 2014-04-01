module OOLang.SrcAnnotated where

import OOLang.SrcSpan

class SrcAnnotated ast where
  ann :: ast s -> s

class SrcAnnotated2 ast where
  ann2 :: ast s v -> s

instance SrcAnnotated2 (,) where
  ann2 = fst

getSrcSpan :: SrcAnnotated ast => ast SrcSpan -> SrcSpan
getSrcSpan = ann

getSrcSpan2 :: SrcAnnotated2 ast => ast SrcSpan v -> SrcSpan
getSrcSpan2 = ann2

