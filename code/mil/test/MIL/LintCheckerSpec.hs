-- | Lint checker tests are basically the same as type checker tests.
-- LintChecker takes an output of the TypeChecker (typed program) and then
-- compares its own result (type environment) with the result of the
-- TypeChecker.
module MIL.LintCheckerSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import MIL.Parser
import MIL.TypeChecker
import MIL.LintChecker
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "typecheckertests"

successDir :: FilePath
successDir = testDir </> "success"

failureDir :: FilePath
failureDir = testDir </> "failure"

-- | Main specification function.
spec :: Spec
spec =
  describe "lintCheck" $ do
    -- Success
    it "accepts type correct program" $
      let baseName = "Program"
      in successCase baseName

-- Infrastructure

-- | Takes a file base name and performs a test.
-- Only check that type checking succeeded (for now).
successCase :: String -> IO ()
successCase baseName = do
  input <- successRead baseName
  let srcProgram = parseMil input
  case typeCheck srcProgram of
    Right (tyProgram, typeEnv) ->
      case lintCheck tyProgram of
        Right typeEnv' -> typeEnv `shouldBe` typeEnv'
        Left err -> error $ prPrint err
    Left err -> error $ prPrint err

-- | Takes a file base name and reads a source program.
successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

-- | Takes a file base name and performs a test (by comparing pretty printed
-- error message).
failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let srcProgram = parseMil input
  let Right (tyProgram, _) = typeCheck srcProgram
  let Left err = lintCheck tyProgram
  prPrint err `shouldBe` errMsg

-- | Takes a file base name and reads a source program and expected error
-- message (from .err file).
failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))

