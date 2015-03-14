-- | MIL code generator tests.
module FunLang.CodeGenMilSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import FunLang.Parser
import FunLang.TypeChecker
import FunLang.CodeGenMil
import FunLang.TestUtils

import qualified MIL.AST.PrettyPrinter as MIL
import qualified MIL.Parser as MIL
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
    it "generates MIL code for data types" $
      testCase "DataTypes"

    it "generates MIL code for lambdas and applications" $
      testCase "LambdasApps"

    it "generates MIL code for polymorphic functions" $
      testCase "Polymorphism"

    it "generates MIL code for monadic do-blocks" $
      testCase "DoBlocks"

-- Infrastructure

-- | Takes a file base name and performs a test (by comparing pretty printed
-- code).
testCase :: String -> IO ()
testCase baseName = do
  (input, output) <- testRead baseName
  let Right srcProgram = parseFunLang (mkFileName baseName) input
  case typeCheck srcProgram of
    Right (tyProgram, typeEnv) -> do
      let srcMilProgram = MIL.parseMil output
      case MIL.typeCheck srcMilProgram of
        Right (tyMilProgram, _) -> do
          let srcGenMilProgram = codeGen tyProgram typeEnv
          case MIL.typeCheck srcGenMilProgram of
            Right (tyGenMilProgram, _) -> MIL.prPrint tyGenMilProgram `shouldBe` MIL.prPrint tyMilProgram
            Left tcErr -> MIL.prPrint tcErr `shouldBe` ""
        Left tcErr -> MIL.prPrint tcErr `shouldBe` ""
    Left tcErr -> prPrint tcErr `shouldBe` ""

-- | Takes a file base name and reads a source program and expected output
-- (from .mil file).
testRead :: String -> IO (String, String)
testRead baseName =
  liftM2 (,) (readFile (testDir </> mkFileName baseName))
             (dropNewLine <$> readFile (testDir </> baseName <.> "mil"))

