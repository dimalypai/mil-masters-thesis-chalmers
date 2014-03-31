module Main where

import System.IO

import FunLang.Lexer
import FunLang.Parser
import FunLang.AST.DebugShow

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "fli> "
    input <- getLine
    if input == ":q"
      then return ()
      else do let p = lexer input |>
                      parse "fli"
              case p of
                Left err -> putStrLn $ prPrint err
                Right pr -> putStrLn $ renderDebug pr
              main

(|>) :: a -> (a -> b) -> b
a |> f = f a

