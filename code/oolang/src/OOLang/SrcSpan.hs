module OOLang.SrcSpan where

data SrcSpan = SrcSpan String Int Int Int Int
  deriving (Show, Eq)

