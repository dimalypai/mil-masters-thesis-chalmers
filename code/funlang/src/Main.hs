module Main where

import FunLang.Lexer
import FunLang.Parser
import FunLang.AST.DebugShow

main :: IO ()
main = putStrLn $ renderDebug p
  where Right p = lexer "type Bool = True | False \n type String A = S" |>
                  parseFunLang "filename"

(|>) :: a -> (a -> b) -> b
a |> f = f a

