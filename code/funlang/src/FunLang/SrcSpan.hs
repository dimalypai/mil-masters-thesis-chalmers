-- | Source spans and source positions of everything in the program.
module FunLang.SrcSpan
  ( SrcSpan
  , mkSrcSpan
  , SrcPos
  , mkSrcPos
  , srcSpanToPos
  , combineSrcSpans
  , setSrcSpanFileName
  , setSrcPosFileName
  , prPrint
  ) where

import FunLang.PrettyPrinter

-- | Span in the source file:
--
-- * file name
--
-- * start line
--
-- * start column
--
-- * end line
--
-- * end column
data SrcSpan = SrcSpan String Int Int Int Int
  deriving (Show, Eq)

-- | Smart constructor for 'SrcSpan'.
mkSrcSpan :: String -> Int -> Int -> Int -> Int -> SrcSpan
mkSrcSpan = SrcSpan

instance Pretty SrcSpan where
  prPrn (SrcSpan fileName startLine startCol endLine endCol) =
    text fileName <> colon <> int startLine <> comma <> int startCol <> char '-'
      <> int endLine <> comma <> int endCol

-- | Position in the source file:
--
-- * file name
--
-- * line
--
-- * column
data SrcPos = SrcPos String Int Int

-- | Smart constructor for 'SrcPos'.
mkSrcPos :: String -> Int -> Int -> SrcPos
mkSrcPos = SrcPos

instance Pretty SrcPos where
  prPrn (SrcPos fileName line col) = text fileName <> colon <> int line <> comma <> int col

-- | Conversion function. Takes the start position.
srcSpanToPos :: SrcSpan -> SrcPos
srcSpanToPos (SrcSpan fileName line col _ _) = mkSrcPos fileName line col

-- | Combines a non-empty list of source spans by taking the start position of
-- the first span and the end position of the last one.
-- Fixes a file name as well.
combineSrcSpans :: [SrcSpan] -> String -> SrcSpan
combineSrcSpans [ss]  fileName = setSrcSpanFileName ss fileName
combineSrcSpans spans fileName =
  let (SrcSpan _ startLine startCol _ _) = head spans
      (SrcSpan _ _ _ endLine endCol)     = last spans
  in mkSrcSpan fileName startLine startCol endLine endCol

-- | Changes a file name in a given source span.
setSrcSpanFileName :: SrcSpan -> String -> SrcSpan
setSrcSpanFileName ss fileName =
  let (SrcSpan _ startLine startCol endLine endCol) = ss
  in mkSrcSpan fileName startLine startCol endLine endCol

-- | Changes a file name in a given source position.
setSrcPosFileName :: SrcPos -> String -> SrcPos
setSrcPosFileName sp fileName =
  let (SrcPos _ line col) = sp
  in mkSrcPos fileName line col

