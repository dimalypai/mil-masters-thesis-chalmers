module OOLang.SrcSpan
  ( SrcSpan
  , mkSrcSpan
  , SrcPos
  , mkSrcPos
  , srcSpanToPos
  , setSrcPosFileName
  ) where

import OOLang.PrettyPrinter

-- | Span in the source file:
-- * file name
-- * start line
-- * start column
-- * end line
-- * end column
data SrcSpan = SrcSpan String Int Int Int Int
  deriving (Show, Eq)

mkSrcSpan :: String -> Int -> Int -> Int -> Int -> SrcSpan
mkSrcSpan = SrcSpan

instance Pretty SrcSpan where
  prPrn (SrcSpan fileName startLine startCol endLine endCol) =
    text fileName <> colon <> int startLine <> comma <> int startCol <> char '-'
      <> int endLine <> comma <> int endCol

-- | Position in the source file:
-- * file name
-- * line
-- * column
data SrcPos = SrcPos String Int Int

mkSrcPos :: String -> Int -> Int -> SrcPos
mkSrcPos = SrcPos

instance Pretty SrcPos where
  prPrn (SrcPos fileName line col) = text fileName <> colon <> int line <> comma <> int col

srcSpanToPos :: SrcSpan -> SrcPos
srcSpanToPos (SrcSpan fileName line col _ _) = mkSrcPos fileName line col

setSrcSpanFileName :: SrcSpan -> String -> SrcSpan
setSrcSpanFileName ss fileName =
  let (SrcSpan _ startLine startCol endLine endCol) = ss
  in mkSrcSpan fileName startLine startCol endLine endCol

setSrcPosFileName :: SrcPos -> String -> SrcPos
setSrcPosFileName sp fileName =
  let (SrcPos _ line col) = sp
  in mkSrcPos fileName line col

