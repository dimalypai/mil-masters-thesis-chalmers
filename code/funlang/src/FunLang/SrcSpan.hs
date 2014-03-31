module FunLang.SrcSpan
  ( SrcSpan
  , mkSrcSpan
  , SrcPos
  , mkSrcPos
  , srcSpanToPos
  , combineSrcSpans
  , setSrcSpanFileName
  , setSrcPosFileName
  ) where

import FunLang.PrettyPrinter

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

combineSrcSpans :: [SrcSpan] -> String -> SrcSpan
combineSrcSpans [ss]  fileName = setSrcSpanFileName ss fileName
combineSrcSpans spans fileName =
  let (SrcSpan _ startLine startCol _ _) = head spans
      (SrcSpan _ _ _ endLine endCol)     = last spans
  in mkSrcSpan fileName startLine startCol endLine endCol

setSrcSpanFileName :: SrcSpan -> String -> SrcSpan
setSrcSpanFileName ss fileName =
  let (SrcSpan _ startLine startCol endLine endCol) = ss
  in mkSrcSpan fileName startLine startCol endLine endCol

setSrcPosFileName :: SrcPos -> String -> SrcPos
setSrcPosFileName sp fileName =
  let (SrcPos _ line col) = sp
  in mkSrcPos fileName line col

