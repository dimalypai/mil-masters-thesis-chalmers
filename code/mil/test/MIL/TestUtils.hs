-- | Utility functions for testing.
module MIL.TestUtils where

import Test.Hspec
import Control.Monad (liftM2)
import System.FilePath ((</>), (<.>))

import MIL.AST
import MIL.AST.PrettyPrinter
import MIL.BuiltIn
import MIL.Parser
import MIL.TypeChecker

-- | Turns base name into a file name (by appending extension).
mkFileName :: String -> String
mkFileName baseName = baseName <.> "mil"

-- | Removes all new line characters at the end of the string.
dropNewLine :: String -> String
dropNewLine "" = ""
dropNewLine str = let l = last str
                  in if l == '\n' || l == '\r'
                       then dropNewLine (init str)
                       else str

-- | Configuration

transformationTestDir :: FilePath
transformationTestDir = "test" </> "transformationstests"

transformationTestCase :: String -> (TyProgram -> TyProgram) -> IO ()
transformationTestCase baseName trans = do
  (input, output) <- transformationTestRead baseName
  let srcInputProgram = parseMil input
      srcOutputProgram = parseMil output
  case ( typeCheck srcInputProgram (repeat defaultMonadError)
       , typeCheck srcOutputProgram (repeat defaultMonadError)) of
    (Right (tyInputProgram, _), Right (tyOutputProgram, _)) ->
      prPrint (trans tyInputProgram) `shouldBe` prPrint tyOutputProgram
    (Left inputTcError, Left outputTcError) ->
      (prPrint inputTcError, prPrint outputTcError) `shouldBe` ("", "")
    (Left inputTcError, _) -> prPrint inputTcError `shouldBe` ""
    (_, Left outputTcError) -> prPrint outputTcError `shouldBe` ""

transformationTestRead :: String -> IO (String, String)
transformationTestRead baseName =
  liftM2 (,) (readFile (transformationTestDir </> mkFileName baseName))
             (readFile (transformationTestDir </> mkFileName (baseName ++ "_out")))

