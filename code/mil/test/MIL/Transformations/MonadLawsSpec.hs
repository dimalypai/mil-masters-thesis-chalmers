-- | MIL monad laws transformations tests.
module MIL.Transformations.MonadLawsSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>))
import Control.Monad (liftM2)

import MIL.AST
import MIL.AST.PrettyPrinter
import MIL.BuiltIn
import MIL.Parser
import MIL.TypeChecker
import MIL.Transformations.MonadLaws
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Configuration

testDir :: FilePath
testDir = "test" </> "transformationstests"

-- | Main specification function.
spec :: Spec
spec = do
  describe "leftIdentity" $ do
    it "applies left identity monad law (simplest case)" $
      testCase "LeftIdentity" leftIdentity

    it "applies left identity monad law (all syntactic forms)" $
      testCase "LeftIdentityFullSyntax" leftIdentity

  describe "rightIdentity" $ do
    it "applies right identity monad law (simplest case)" $
      testCase "RightIdentity" rightIdentity

    it "applies right identity monad law (all syntactic forms)" $
      testCase "RightIdentityFullSyntax" rightIdentity

    it "does not apply when different variable than in the bind is returned" $
      testCase "RightIdentityDifferentVar" rightIdentity

  describe "associativity" $ do
    it "applies associativity monad law (simplest case)" $
      testCase "Associativity" associativity

    it "applies associativity monad law (all syntactic forms)" $
      testCase "AssociativityFullSyntax" associativity

testCase :: String -> (TyProgram -> TyProgram) -> IO ()
testCase baseName trans = do
  (input, output) <- testRead baseName
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

testRead :: String -> IO (String, String)
testRead baseName =
  liftM2 (,) (readFile (testDir </> mkFileName baseName))
             (readFile (testDir </> mkFileName (baseName ++ "_out")))

