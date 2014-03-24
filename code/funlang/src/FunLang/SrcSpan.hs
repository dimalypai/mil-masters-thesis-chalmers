module FunLang.SrcSpan where

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

instance Show SrcSpan where
  show (SrcSpan fileName startLine startCol endLine endCol) =
    printf "%s:%d,%d-%d,%d" fileName startLine startCol endLine endCol

data SrcPos = SrcPos
  { spFileName :: String
  , spLine     :: Int
  , spCol      :: Int
  }

instance Show SrcPos where
  show (SrcPos fileName line col) = printf "%s:%d,%d" fileName line col

srcSpanToPos :: SrcSpan -> SrcPos
srcSpanToPos (SrcSpan fileName line col _ _) = SrcPos fileName line col

combineSrcSpans :: [SrcSpan] -> String -> SrcSpan
combineSrcSpans [ss]  fileName = ss { ssFileName = fileName }
combineSrcSpans spans fileName =
  let (SrcSpan _ startLine startCol _ _) = head spans
      (SrcSpan _ _ _ endLine endCol) = last spans
  in SrcSpan fileName startLine startCol endLine endCol

