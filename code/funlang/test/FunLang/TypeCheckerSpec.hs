-- | Type checker tests.
module FunLang.TypeCheckerSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import FunLang.Parser
import FunLang.TypeChecker
import FunLang.TestUtils

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
      it "given a program without main function" $
        failureCase "MainNotDefined"

      it "given a program with incorrect type of main" $
        failureCase "MainIncorrectType"

      it "given a program with ill-formed type of main" $
        failureCase "MainIllFormedType"

      it "given a type redefinition" $
        failureCase "TypeRedefinition"

      it "given a function redefinition" $
        failureCase "FunRedefinition"

      it "given a type definition which has diplicated names of type parameters" $
        failureCase "TypeParamsDuplication"

      it "given type definitions with the same data constructors" $
        failureCase "DataConAlreadyDefined"

      it "given an expression referencing data constructor that is not defined" $
        failureCase "DataConNotDefined"

      it "given a type where type variable has the same name as already defined type" $
        failureCase "TypeVarShadowsType"

      it "given a type where type variable is redefined" $
        failureCase "TypeVarShadowsTypeVar"

      it "given a type which contains type variable application" $
        failureCase "TypeVarApp"

      it "given a type that uses undefined type" $
        failureCase "TypeNotDefined"

      it "given a type constructor which is applied to too few arguments" $
        failureCase "TypeIncorrectKindLess"

      it "given a type constructor which is applied to too many arguments" $
        failureCase "TypeIncorrectKindMore"

      it "given an ill-kinded type nested in another type" $
        failureCase "TypeIncorrectKindNested"

      it "given a function equation with different function name" $
        failureCase "FunEqIncorrectName"

      it "given a function equation with incorrect type" $
        failureCase "FunEqBodyIncorrectType"

      it "given a return statement with ill-kinded type" $
        failureCase "ReturnIllKinded"

      it "given a return statement with non-monadic type" $
        failureCase "ReturnNotMonad"

      it "given a return statement with incorrect expression type" $
        failureCase "ReturnIncorrectExprType"

      it "given an application with left-hand side which is not a function" $
        failureCase "AppNotFunction"

      it "given an application with incorrect argument type" $
        failureCase "AppIncorrectArgType"

      it "given an unbound variable name" $
        failureCase "VarNotBound"

      it "given a function body with lambda of incorrect type" $
        failureCase "LambdaIncorrectType"

      it "given an application of lambda to the argument of incorrect type" $
        failureCase "LambdaAppIncorrectType"

      it "given a lambda expression with duplicated parameter names" $
        failureCase "LambdaParamsDup"

      it "given a lambda expression which shadows an existing function" $
        failureCase "VarShadowsFun"

      it "given nested lambda expressions with the same parameter names" $
        failureCase "NestedLambdasVarShadowing"

      it "given a lambda expression with a binding that references type which is not in scope" $
        failureCase "LambdaTypeNotDefined"

      it "given a lambda expression and a free variable with the same name as lambda parameter" $
        failureCase "LambdaParamFreeVarName"

      it "given a function body with type lambda of incorrect type" $
        failureCase "TypeLambdaIncorrectType"

      it "given a type lambda with duplicated parameter names" $
        failureCase "TypeLambdaParamsDup"

      it "given a type lambda which shadows an existing type" $
        failureCase "TypeLambdaShadowsType"

      it "given nested type lambdas with the same parameter names" $
        failureCase "NestedTypeLambdasVarShadowing"

      it "given a type application with non-forall left-hand side" $
        failureCase "TypeAppNotForall"

-- Infrastructure

-- | Takes a file base name and performs a test.
-- Only check that type checking succeeded (for now).
successCase :: String -> IO ()
successCase baseName = do
  input <- successRead baseName
  let Right srcProgram = parseFunLang (mkFileName baseName) input
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
  let Right srcProgram = parseFunLang (mkFileName baseName) input
  let Left err = typeCheck srcProgram
  prPrint err `shouldBe` errMsg

-- | Takes a file base name and reads a source program and expected error
-- message (from .err file).
failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))

