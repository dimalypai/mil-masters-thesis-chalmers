-- | MIL code generator tests.
module OOLang.CodeGenMilSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import OOLang.Parser
import OOLang.TypeChecker
import OOLang.CodeGenMil
import OOLang.TestUtils

import qualified MIL.AST.PrettyPrinter as MIL
import qualified MIL.TypeChecker as MIL

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "codegenmiltests"

-- | Main specification function.
spec :: Spec
spec =
  describe "codeGen" $ do
    it "generates MIL code" $
      testCase "Program"

-- Infrastructure

-- | Takes a file base name and performs a test (by comparing pretty printed
-- code).
testCase :: String -> IO ()
testCase baseName = do
  (input, output) <- testRead baseName
  let Right srcProgram = parseOOLang (mkFileName baseName) input
  let Right (tyProgram, _) = typeCheck srcProgram
  let milProgram = codeGen tyProgram
  case MIL.typeCheck milProgram of
    Right _ -> (dropNewLine $ MIL.prPrint milProgram) `shouldBe` output
    Left milTcErr -> MIL.prPrint milTcErr `shouldBe` ""

-- | Takes a file base name and reads a source program and expected output
-- (from .mil file).
testRead :: String -> IO (String, String)
testRead baseName =
  liftM2 (,) (readFile (testDir </> mkFileName baseName))
             (dropNewLine <$> readFile (testDir </> baseName <.> "mil"))

