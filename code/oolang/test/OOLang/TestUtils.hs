-- | Utility functions for testing.
module OOLang.TestUtils where

import System.FilePath ((<.>))

-- | Turns base name into a file name (by appending extension).
mkFileName :: String -> String
mkFileName baseName = baseName <.> "ool"

-- | Removes all new line characters at the end of the string.
dropNewLine :: String -> String
dropNewLine "" = ""
dropNewLine str = let l = last str
                  in if l == '\n' || l == '\r'
                       then dropNewLine (init str)
                       else str

