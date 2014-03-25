module FunLang.SrcSpan
  ( SrcSpan
  , mkSrcSpan
  , SrcPos
  , mkSrcPos
  , srcSpanToPos
  , combineSrcSpans
  , setSrcSpanFileName
  ) where

import FunLang.DebugShow
import FunLang.PrettyPrinter

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

instance Pretty SrcSpan where
  prPrn (SrcSpan fileName startLine startCol endLine endCol) =
    text fileName <> colon <> int startLine <> comma <> int startCol <> char '-'
      <> int endLine <> comma <> int endCol

instance DebugShow SrcSpan where
  showDebug = prPrn

data SrcPos = SrcPos
  { spFileName :: String
  , spLine     :: Int
  , spCol      :: Int
  }

mkSrcPos :: String -> Int -> Int -> SrcPos
mkSrcPos = SrcPos

instance Pretty SrcPos where
  prPrn (SrcPos fileName line col) = text fileName <> colon <> int line <> comma <> int col

instance DebugShow SrcPos where
  showDebug = prPrn

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

