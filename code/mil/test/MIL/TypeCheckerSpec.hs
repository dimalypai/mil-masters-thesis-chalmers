-- | Type checker tests.
module MIL.TypeCheckerSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import MIL.Parser
import MIL.TypeChecker
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
  describe "typeCheck" $ do
    -- Success
    it "accepts type correct program" $
      let baseName = "Program"
      in successCase baseName

    -- Failure
    describe "gives an error message" $ do
      it "given a type redefinition" $
        failureCase "TypeRedefinition"

      it "given a function redefinition" $
        failureCase "FunRedefinition"

      it "given a type definition which has diplicated names of type parameters" $
        failureCase "TypeParamsDuplication"

      it "given type definitions with the same data constructors" $
        failureCase "DataConAlreadyDefined"

      it "given a program without main function" $
        failureCase "MainNotDefined"

      it "given a type definition with a type parameter that shadows another type" $
        failureCase "TypeParamShadowsType"

      it "given a function body of incorrect type" $
        failureCase "FunBodyIncorrectType"

      it "given an unbound variable name" $
        failureCase "VarNotBound"

      it "given a lambda expression which shadows an existing function" $
        failureCase "VarShadowsFun"

      it "given nested lambda expressions with the same parameter names" $
        failureCase "NestedLambdasVarShadowing"

      it "given a function body with lambda of incorrect type" $
        failureCase "LambdaIncorrectType"

      it "given an application with left-hand side which is not a function" $
        failureCase "AppNotFunction"

      it "given an application with incorrect argument type" $
        failureCase "AppIncorrectArgType"

      it "given an application of lambda to the argument of incorrect type" $
        failureCase "LambdaAppIncorrectType"

      it "given a function body with type lambda of incorrect type" $
        failureCase "TypeLambdaIncorrectType"

      it "given a type lambda which shadows an existing type" $
        failureCase "TypeLambdaShadowsType"

      it "given nested type lambdas with the same parameter names" $
        failureCase "NestedTypeLambdasVarShadowing"

      it "given a type application with non-forall left-hand side" $
        failureCase "TypeAppNotForall"

      it "given an expression referencing data constructor that is not defined" $
        failureCase "DataConNotDefined"

      it "given a function body with a tuple of incorrect type" $
        failureCase "TupleIncorrectType"

      it "given a case alternative of incorrect type" $
        failureCase "CaseAltIncorrectType"

      it "given a literal pattern of incorrect type" $
        failureCase "LitPatternIncorrectType"

      it "given a variable pattern which shadows an existing function" $
        failureCase "VarPatternShadowsFun"

      it "given a variable pattern of incorrect type" $
        failureCase "VarPatternIncorrectType"

      it "given a constructor pattern with undefined constructor" $
        failureCase "ConPatternUndefined"

      it "given a constructor pattern of incorrect type" $
        failureCase "ConPatternIncorrectType"

      it "given a constructor pattern with incorrect number of field patterns" $
        failureCase "ConPatternIncorrectNumberOfFields"

      it "given a nested variable pattern (in constructor) which shadows another variable" $
        failureCase "VarConPatternShadowsNested"

      it "given a nested variable pattern (in constructor) of incorrect type" $
        failureCase "VarConPatternNestedIncorrectType"

      it "given a tuple pattern for non-tuple scrutinee" $
        failureCase "TuplePatternNonTupleScrut"

      it "given a tuple pattern with incorrect number of element patterns" $
        failureCase "TuplePatternIncorrectNumberOfElements"

      it "given a nested variable pattern (in tuple) which shadows another variable" $
        failureCase "VarTuplePatternShadowsNested"

      it "given a nested variable pattern (in tuple) of incorrect type" $
        failureCase "VarTuplePatternNestedIncorrectType"

      it "given a non-monadic type in return" $
        failureCase "ReturnNonMonadic"

      it "given a type of incorrect kind in return" $
        failureCase "ReturnIncorrectKind"

-- * Infrastructure

-- | Takes a file base name and performs a test.
-- Only check that type checking succeeded (for now).
successCase :: String -> IO ()
successCase baseName = do
  input <- successRead baseName
  let srcProgram = parseMil input
  case typeCheck srcProgram of
    Right (tyProgram, _) -> True `shouldBe` True
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
  let Left err = typeCheck srcProgram
  prPrint err `shouldBe` errMsg

-- | Takes a file base name and reads a source program and expected error
-- message (from .err file).
failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))

