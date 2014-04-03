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
import OOLang.TypeChecker

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (flags, nonOpts) <- getArgs >>= compilerOpts
  when (Help `elem` flags) $ do
    printHelp
    exitSuccess
  if Interactive `elem` flags
    then interactive emptyTypeEnv
    else compiler flags nonOpts

interactive :: TypeEnv -> IO ()
interactive typeEnv = do
  putStr "ooli> "
  input <- getLine
  case input of
    ':':'q':_ -> exitSuccess
    ":parse" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram -> putStrLn (ppShow srcProgram)
    ":tc" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram ->
          case typeCheck srcProgram of
            Left tcErr -> putStrLn (prPrint tcErr)
            Right (tyProgram, _) -> putStrLn (ppShow tyProgram) >> interactive typeEnv
    ":define" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram ->
          case typeCheckStage srcProgram typeEnv of
            Left tcErr -> putStrLn (prPrint tcErr)
            Right (tyProgram, typeEnv') -> putStrLn (ppShow tyProgram) >> interactive typeEnv'
    --':':'t' : exprStr ->
    --":compile" -> readProgram
    --exprStr ->
    _ -> putStrLn "Wrong command"
  interactive typeEnv

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
    Left parseErr -> putStrLn (prPrint parseErr) >> exitFailure
    Right srcProgram -> putStrLn (ppShow srcProgram) >> exitSuccess

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

