module FunLang.SrcSpan where

import Text.Printf

-- | Span in the source file:
-- * file name
-- * start line
-- * start column
-- * end line
-- * end column
data SrcSpan = SrcSpan String Int Int Int Int
  deriving Eq

instance Show SrcSpan where
  show (SrcSpan fileName startLine startCol endLine endCol) =
    printf "%s:%d,%d-%d,%d" fileName startLine startCol endLine endCol

data SrcPos = SrcPos String Int Int

instance Show SrcPos where
  show (SrcPos fileName line col) = printf "%s:%d,%d" fileName line col

srcSpanToPos :: SrcSpan -> SrcPos
srcSpanToPos (SrcSpan fileName line col _ _) = SrcPos fileName line col

combineSrcSpan :: [SrcSpan] -> SrcSpan
combineSrcSpan [sp]  = sp
combineSrcSpan spans =
  let (SrcSpan fileName startLine startCol _ _) = head spans
      (SrcSpan _ _ _ endLine endCol) = last spans
  in SrcSpan fileName startLine startCol endLine endCol

