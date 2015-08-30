-- | Main module of the FunLang compiler.
module Main where

import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt
import System.FilePath
import System.Exit
import Text.Show.Pretty
import Control.Monad (when)
import Data.List (intercalate, stripPrefix)

import FunLang.Lexer
import FunLang.Parser
import FunLang.TypeChecker
import FunLang.CodeGenMil
import FunLang.Optimiser
import FunLang.Utils

import qualified MIL.AST as MIL
import qualified MIL.AST.PrettyPrinter as MIL
import qualified MIL.TypeChecker as MIL
import qualified MIL.LintChecker as MIL

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

-- | Main interactive loop. Invoked with `flc -i`.
-- Gets compiler flags as a first argument.
-- Two other arguments are for maintaining and accumulating type environment
-- (used and updated with `:define` command) and a reversed list of source
-- programs (every new defined program becomes a head of the list, used with
-- `:save` command).
interactive :: [Flag] -> TypeEnv -> [String] -> IO ()
interactive flags typeEnv revProgramStrs = do
  putStr "fli> "
  input <- getLine
  case input of
    -- anything that begins with ":q" exits the program
    ':':'q':_ -> exitSuccess
    ":parse" -> do
      src <- readProgram
      case lexer src |> parse "fli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram -> do
          when (DumpAst `elem` flags) $
            putStrLn (ppShow srcProgram)
    -- `:tc` is purely local type checking, touches neither the type
    -- environment nor the list of programs
    ":tc" -> do
      src <- readProgram
      case lexer src |> parse "fli" of
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
      case lexer src |> parse "fli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram ->
          case typeCheckStage srcProgram typeEnv of
            Left tcErr -> putStrLn (prPrint tcErr)
            Right (tyProgram, typeEnv') -> do
              when (DumpAst `elem` flags) $
                putStrLn (ppShow tyProgram)
              interactive flags typeEnv' (src : revProgramStrs)
    -- `:compile` generates and displays MIL code for programs defined with
    -- `:define` command.
    ":compile" -> do
      let src = intercalate "\n\n" (reverse revProgramStrs)
      case lexer src |> parse "fli" of
        Left parseErr -> putStrLn (prPrint parseErr)
        Right srcProgram ->
          case typeCheck srcProgram of
            Left tcErr -> putStrLn (prPrint tcErr)
            Right (tyProgram, programTypeEnv) -> do
              when (DumpAst `elem` flags) $
                putStrLn (ppShow tyProgram)
              let milSrcProgram = codeGen tyProgram programTypeEnv
              mTyMilProgram <- typeCheckMil milSrcProgram "Before Optimiser"
              putStrLn (MIL.prPrint milSrcProgram)
              case mTyMilProgram of
                Just tyMilProgram ->
                  when (Opt `elem` flags) $ do
                    let optMilProgram = optimiseMil tyMilProgram
                    when (CheckMil `elem` flags) $
                      lintCheckMil optMilProgram "After Optimiser"
                    putStrLn (MIL.prPrint optMilProgram)
                Nothing -> return ()
    -- `:print` displays all the programs defined with `:define` command.
    ":print" -> putStr ((intercalate "\n\n" $ reverse revProgramStrs) ++ "\n")
    -- All commands with arguments go here
    command -> do
      let processCommand cmd | Just fileName <- stripPrefix ":save " cmd =
          -- `:save` takes a file name and saves a current list of programs to the file
            writeFile fileName ((intercalate "\n\n" $ reverse revProgramStrs) ++ "\n")

                             | Just fileName <- stripPrefix ":load " cmd =
          -- `:load` takes a file name and processes contents of a file through `:define`
            do src <- readFile fileName
               case lexer src |> parse "ooli" of
                 Left parseErr -> putStrLn (prPrint parseErr)
                 Right srcProgram ->
                   case typeCheckStage srcProgram typeEnv of
                     Left tcErr -> putStrLn (prPrint tcErr)
                     Right (tyProgram, typeEnv') -> do
                       when (DumpAst `elem` flags) $
                         putStrLn (ppShow tyProgram)
                       interactive flags typeEnv' (src : revProgramStrs)

                             | Just strExpr <- stripPrefix ":t " cmd =
          -- `:t` takes an expression and returns its type (things defined
          -- with `:define` are in scope)
            do case lexer strExpr |> parseExpr "fli" of
                 Left parseErr -> putStrLn (prPrint parseErr)
                 Right srcExpr ->
                   case typeOf srcExpr typeEnv of
                     Left tcErr -> putStrLn (prPrint tcErr)
                     Right exprType -> putStrLn ({-prPrint srcExpr ++ " : " ++ -}prPrint exprType)
          processCommand _ = putStrLn "Incorrect command"
      processCommand command
  interactive flags typeEnv revProgramStrs

-- | Enters the program entering mode and returns a program text as a String.
readProgram :: IO String
readProgram = readProgram' 1 []
  -- Helper function that counts line numbers (for display) and accumulates
  -- program lines in reverse order.
  where readProgram' :: Int -> [String] -> IO String
        readProgram' line revProgram = do
          putStr $ "fli|" ++ show line ++ " "
          input <- getLine
          if input == ":ok"
            then return $ intercalate "\n" $ reverse revProgram
            else readProgram' (line + 1) (input : revProgram)

-- | Performs MIL type checking and prints an error message if it fails.
-- Takes a string for a phase to improve error message.
-- Returns a typed MIL program in the case of success.
typeCheckMil :: MIL.SrcProgram -> String -> IO (Maybe MIL.TyProgram)
typeCheckMil srcMilProgram phase =
  case MIL.typeCheck srcMilProgram monadError of
    Left milTcErr -> do
      putStrLn (phase ++ ":")
      putStrLn (MIL.prPrint milTcErr)
      return Nothing
    Right (tyProgram, _) -> return $ Just tyProgram

-- | Performs MIL lint checking and prints an error message if it fails.
-- Takes a string for a phase to improve error message.
lintCheckMil :: MIL.TyProgram -> String -> IO ()
lintCheckMil tyMilProgram phase =
  case MIL.lintCheck tyMilProgram monadError of
    Left milTcErr -> do
      putStrLn (phase ++ ":")
      putStrLn (MIL.prPrint milTcErr)
    Right _ -> return ()

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
  case lexer src |> parse (takeFileName filePath) of
    Left parseErr -> putStrLn (prPrint parseErr) >> exitFailure
    Right srcProgram ->
      case typeCheck srcProgram of
        Left tcErr -> putStrLn (prPrint tcErr) >> exitFailure
        Right (tyProgram, programTypeEnv) -> do
          when (DumpAst `elem` flags) $
            putStrLn (ppShow tyProgram)
          let srcMilProgram = codeGen tyProgram programTypeEnv
          mTyMilProgram <- typeCheckMil srcMilProgram "Before Optimiser"
          putStrLn (MIL.prPrint srcMilProgram)
          case mTyMilProgram of
            Just tyMilProgram -> do
              when (Opt `elem` flags) $ do
                let outMilProgram = optimiseMil tyMilProgram
                when (CheckMil `elem` flags) $
                  lintCheckMil outMilProgram "After Optimiser"
                putStrLn (MIL.prPrint outMilProgram)
              exitSuccess
            Nothing -> exitFailure

-- | Compiler flags.
data Flag = Interactive | DumpAst | Opt | CheckMil | Help
  deriving Eq

-- | Description and mapping of compiler flags.
options :: [OptDescr Flag]
options =
  [ Option ['i'] ["interactive"] (NoArg Interactive) "Interactive mode (REPL)"
  , Option []    ["dump-ast"]    (NoArg DumpAst)     "Write AST to the screen"
  , Option ['O'] []              (NoArg Opt)         "Perform optimisations"
  , Option ['c'] ["check-mil"]   (NoArg CheckMil)    "Perform MIL type checking"
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
  where header = "Usage: flc [OPTIONS] file"

