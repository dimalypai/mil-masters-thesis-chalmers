module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Exit
import Text.Show.Pretty
import Control.Monad (when)

import OOLang.Lexer
import OOLang.Parser

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    (flags, nonOpts) <- getArgs >>= compilerOpts
    when (Help `elem` flags) $ do
      printHelp
      exitSuccess
    if Interactive `elem` flags
      then do putStr "ooli> "
              input <- getLine
              if input == ":q"
                then exitSuccess
                else do let p = lexer input |>
                                parse "ooli"
                        case p of
                          Left err -> putStrLn (prPrint err)
                          Right pr -> putStrLn (ppShow pr)
              main
      else do let filePath = head nonOpts
              src <- readFile filePath
              let p = lexer src |>
                      parse (takeFileName filePath)
              case p of
                Left err -> putStrLn (prPrint err) >> exitFailure
                Right pr -> putStrLn (ppShow pr) >> exitSuccess

data Flag = Interactive | DumpAst | Help
  deriving Eq

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["interactive"] (NoArg Interactive) "Interactive mode (REPL)"
  , Option []    ["dump-ast"]    (NoArg DumpAst)     "Write AST to a file"  -- TODO
  , Option ['h'] ["help"]        (NoArg Help)        "Prints this help information"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (opts, nonOpts, []) -> return (opts, nonOpts)
    (_, _, errs) -> ioError (userError (concat errs ++ "Try the -h (--help) option"))

printHelp :: IO ()
printHelp = putStr $ usageInfo header options
  where header = "Usage: oolc [OPTIONS] file"

(|>) :: a -> (a -> b) -> b
a |> f = f a

