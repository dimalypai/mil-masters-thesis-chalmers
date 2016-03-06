-- | MIL optimiser tests.
module OOLang.OptimiserSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)

import OOLang.Parser
import OOLang.TypeChecker
import OOLang.CodeGenMil
import OOLang.Optimiser
import OOLang.TestUtils

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

    it "optimises the code (immutable variables and function calls)" $
      testCase "ImmutableVarsFunctionCalls"

    it "optimises the code for classes" $
      testCase "Classes"

    it "folds constants" $
      testCase "ConstantFolding"

    it "eliminates redundant when statements" $
      testCase "WhenElimination"

-- | Takes a file base name and performs a test (by comparing pretty printed
-- code).
testCase :: String -> IO ()
testCase baseName = do
  (input, output) <- testRead baseName
  let Right srcProgram = parseOOLang (mkFileName baseName) input
  case typeCheck srcProgram of
    Right (tyProgram, typeEnv) -> do
      let srcGenMilProgram = codeGen tyProgram typeEnv
      let srcMilProgram = MIL.parseMil output
      case MIL.typeCheck srcMilProgram monadErrorTypeCons of
        Right (tyMilProgram, _) -> do
          case MIL.typeCheck srcGenMilProgram monadErrorTypeCons of
            Right (tyGenMilProgram, _) ->
              MIL.prPrint (optimiseMil tyGenMilProgram) `shouldBe` MIL.prPrint tyMilProgram
            Left tcErr -> ("GENERATED:" ++ MIL.prPrint tcErr) `shouldBe` "GENERATED:"
        Left tcErr -> ("EXPECTED:" ++ MIL.prPrint tcErr) `shouldBe` "EXPECTED:"
    Left tcErr -> prPrint tcErr `shouldBe` ""

-- | Takes a file base name and reads a source program and expected output
-- (from .mil file).
testRead :: String -> IO (String, String)
testRead baseName =
  liftM2 (,) (readFile (testDir </> mkFileName baseName))
             (readFile (testDir </> (baseName ++ "_opt") <.> "mil"))

