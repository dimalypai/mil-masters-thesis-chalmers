module Main where

import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt
import System.FilePath
import System.Exit
import Text.Show.Pretty
import Control.Monad (when)
import Data.List (intercalate)

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
    then interactive
    else compiler flags nonOpts

interactive :: IO ()
interactive = do
  putStr "ooli> "
  input <- getLine
  case input of
    ':':'q':_ -> exitSuccess
    ":parse" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left err  -> putStrLn (prPrint err)
        Right ast -> putStrLn (ppShow ast)
    ":tc" -> putStrLn "Not implemented yet"
    _ -> putStrLn "Wrong command"
  interactive

readProgram :: IO String
readProgram = readProgram' 1 []
  where readProgram' :: Int -> [String] -> IO String
        readProgram' line revProgram = do
          putStr $ "ooli|" ++ show line ++ " "
          input <- getLine
          if input == ":ok"
            then return $ intercalate "\n" $ reverse revProgram
            else readProgram' (line + 1) (input : revProgram)

compiler :: [Flag] -> [String] -> IO ()
compiler flags args = do
  when (null args) $ do
    printHelp
    exitFailure
  let filePath = head args
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

