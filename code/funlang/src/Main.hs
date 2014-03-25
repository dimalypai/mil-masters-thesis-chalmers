module Main where

import FunLang.Lexer
import FunLang.Parser

main :: IO ()
main = print $
  lexer "type Bool = True | False \n type String A = S" |>
  parseFunLang "filename"

(|>) :: a -> (a -> b) -> b
a |> f = f a

