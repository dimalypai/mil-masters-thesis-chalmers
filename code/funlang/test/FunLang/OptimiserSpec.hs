-- | MIL optimiser tests.
module FunLang.OptimiserSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)

import FunLang.Parser
import FunLang.TypeChecker
import FunLang.CodeGenMil
import FunLang.Optimiser
import FunLang.TestUtils

import qualified MIL.AST.PrettyPrinter as MIL
import qualified MIL.Parser as MIL
import qualified MIL.TypeChecker as MIL

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "optimisertests"

-- | Main specification function.
spec :: Spec
spec =
  describe "optimiseMil" $ do
    it "optimises the code (simple case)" $
      testCase "Simple"

    it "optimises the code (simple data constructor and function applications)" $
      testCase "DataConFunctionApps"

    it "optimises the code (monads)" $
      testCase "Monads"

    it "folds constants" $
      testCase "ConstantFolding"

-- | Takes a file base name and performs a test (by comparing pretty printed
-- code).
testCase :: String -> IO ()
testCase baseName = do
  (input, output) <- testRead baseName
  let Right srcProgram = parseFunLang (mkFileName baseName) input
  case typeCheck srcProgram of
    Right (tyProgram, typeEnv) -> do
      let srcGenMilProgram = codeGen tyProgram typeEnv
      let srcMilProgram = MIL.parseMil output
      case MIL.typeCheck srcMilProgram monadErrorTypeCons of
        Right (tyMilProgram, _) -> do
          case MIL.typeCheck srcGenMilProgram monadErrorTypeCons of
            Right (tyGenMilProgram, _) ->
              MIL.prPrint (optimiseMil tyGenMilProgram) `shouldBe` MIL.prPrint tyMilProgram
            Left tcErr -> MIL.prPrint tcErr `shouldBe` ""
        Left tcErr -> MIL.prPrint tcErr `shouldBe` ""
    Left tcErr -> prPrint tcErr `shouldBe` ""

-- | Takes a file base name and reads a source program and expected output
-- (from .mil file).
testRead :: String -> IO (String, String)
testRead baseName =
  liftM2 (,) (readFile (testDir </> mkFileName baseName))
             (readFile (testDir </> (baseName ++ "_opt") <.> "mil"))

