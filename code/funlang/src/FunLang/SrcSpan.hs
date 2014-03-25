module FunLang.SrcSpan
  ( SrcSpan
  , mkSrcSpan
  , SrcPos
  , mkSrcPos
  , srcSpanToPos
  , combineSrcSpans
  , setSrcSpanFileName
  ) where

import Text.Printf

-- | Span in the source file:
-- * file name
-- * start line
-- * start column
-- * end line
-- * end column
data SrcSpan = SrcSpan
  { ssFileName  :: String
  , ssStartLine :: Int
  , ssStartCol  :: Int
  , ssEndLine   :: Int
  , ssEndCol    :: Int
  } deriving Eq

mkSrcSpan :: String -> Int -> Int -> Int -> Int -> SrcSpan
mkSrcSpan = SrcSpan

instance Show SrcSpan where
  show (SrcSpan fileName startLine startCol endLine endCol) =
    printf "%s:%d,%d-%d,%d" fileName startLine startCol endLine endCol

data SrcPos = SrcPos
  { spFileName :: String
  , spLine     :: Int
  , spCol      :: Int
  }

mkSrcPos :: String -> Int -> Int -> SrcPos
mkSrcPos = SrcPos

instance Show SrcPos where
  show (SrcPos fileName line col) = printf "%s:%d,%d" fileName line col

srcSpanToPos :: SrcSpan -> SrcPos
srcSpanToPos (SrcSpan fileName line col _ _) = mkSrcPos fileName line col

combineSrcSpans :: [SrcSpan] -> String -> SrcSpan
combineSrcSpans [ss]  fileName = ss { ssFileName = fileName }
combineSrcSpans spans fileName =
  let (SrcSpan _ startLine startCol _ _) = head spans
      (SrcSpan _ _ _ endLine endCol)     = last spans
  in mkSrcSpan fileName startLine startCol endLine endCol

setSrcSpanFileName :: SrcSpan -> String -> SrcSpan
setSrcSpanFileName ss fileName = ss { ssFileName = fileName }

