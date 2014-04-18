-- | Main module of the OOLang compiler.
module Main where

import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt
import System.FilePath
import System.Exit
import Text.Show.Pretty
import Control.Monad (when)
import Data.List (intercalate, stripPrefix)

import OOLang.Lexer
import OOLang.Parser
import OOLang.TypeChecker

-- | Main entry point.
-- Gets compiler arguments.
-- Dispatches on Help and Interactive.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (flags, nonOpts) <- getArgs >>= compilerOpts
  when (Help `elem` flags) $ do
    printHelp
    exitSuccess
  if Interactive `elem` flags
    then interactive flags initTypeEnv []
    else compiler flags nonOpts

-- | Main interactive loop. Invoked with `oolc -i`.
-- Gets compiler flags as a first argument.
-- Two other arguments are for maintaining and accumulating type environment
-- (used and updated with `:define` command) and a reversed list of source
-- programs (every new defined program becomes a head of the list, used with
-- `:save` command).
interactive :: [Flag] -> TypeEnv -> [String] -> IO ()
interactive flags typeEnv revProgramStrs = do
  putStr "ooli> "
  input <- getLine
  case input of
    -- anything that begins with ":q" exits the program
    ':':'q':_ -> exitSuccess
    ":parse" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram -> do
          when (DumpAst `elem` flags) $
            putStrLn (ppShow srcProgram)
    -- `:tc` is purely local type checking, touches neither the type
    -- environment nor the list of programs
    ":tc" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram ->
          case typeCheck srcProgram of
            Left tcErr -> putStrLn (prPrint tcErr)
            Right (tyProgram, _) -> do
              when (DumpAst `elem` flags) $
                putStrLn (ppShow tyProgram)
    -- `:define` interacts with the global state.
    -- It uses 'typeCheckStage' to pass the current type environment and to get
    -- a new one. It also saves a program to the program list.
    ":define" -> do
      src <- readProgram
      case lexer src |> parse "ooli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram ->
          case typeCheckStage srcProgram typeEnv of
            Left tcErr -> putStrLn (prPrint tcErr)
            Right (tyProgram, typeEnv') -> do
              when (DumpAst `elem` flags) $
                putStrLn (ppShow tyProgram)
              interactive flags typeEnv' (src : revProgramStrs)
    --":compile" ->
    -- All commands with arguments go here
    command -> do
      let processCommand cmd | Just fileName <- stripPrefix ":save " cmd =
          -- `:save` takes a file name and saves a current list of programs to the file
            writeFile fileName ((intercalate "\n\n" $ reverse revProgramStrs) ++ "\n")
                             | Just strExpr <- stripPrefix ":t " cmd =
          -- `:t` takes an expression and returns its type (things defined
          -- with `:define` are in scope)
            do case lexer strExpr |> parseExpr "ooli" of
                 Left parseErr -> putStrLn (prPrint parseErr)
                 Right srcExpr ->
                   case typeOf srcExpr typeEnv of
                     Left tcErr -> putStrLn (prPrint tcErr)
                     Right exprType -> putStrLn ({-prPrint srcExpr ++ " : " ++ -}prPrint exprType)
          processCommand _ = putStrLn "Wrong command"
      processCommand command
  interactive flags typeEnv revProgramStrs

-- | Enters the program entering mode and returns a program text as a String.
readProgram :: IO String
readProgram = readProgram' 1 []
  -- Helper function that counts line numbers (for display) and accumulates
  -- program lines in reverse order.
  where readProgram' :: Int -> [String] -> IO String
        readProgram' line revProgram = do
          putStr $ "ooli|" ++ show line ++ " "
          input <- getLine
          if input == ":ok"
            then return $ intercalate "\n" $ reverse revProgram
            else readProgram' (line + 1) (input : revProgram)

-- | Main compilation function.
-- Takes compiler flags and a list of non-options. We currently use it just for
-- one file name.
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

-- | Compiler flags.
data Flag = Interactive | DumpAst | Help
  deriving Eq

-- | Description and mapping of compiler flags.
options :: [OptDescr Flag]
options =
  [ Option ['i'] ["interactive"] (NoArg Interactive) "Interactive mode (REPL)"
  , Option []    ["dump-ast"]    (NoArg DumpAst)     "Write AST to a file"
  , Option ['h'] ["help"]        (NoArg Help)        "Prints this help information"
  ]

-- | Takes a list of arguments and parses them to a list of compiler flags and
-- non-options. In the case of errors, prints help information.
compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (opts, nonOpts, []) -> return (opts, nonOpts)
    (_, _, errs) -> ioError (userError (concat errs ++ "Try the -h (--help) option"))

-- | Prints help information.
printHelp :: IO ()
printHelp = putStr $ usageInfo header options
  where header = "Usage: oolc [OPTIONS] file"

-- | Postfix application. Inspired by OCaml/F#.
-- Used with lexer and parser calls.
(|>) :: a -> (a -> b) -> b
a |> f = f a

