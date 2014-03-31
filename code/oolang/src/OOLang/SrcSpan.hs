module OOLang.SrcSpan
  ( SrcSpan
  , mkSrcSpan
  ) where

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

