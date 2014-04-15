-- | Type checker tests.
module OOLang.TypeCheckerSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import OOLang.AST
import OOLang.Parser
import OOLang.TypeChecker
import OOLang.SrcSpan
import OOLang.TestUtils

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
    it "type checks a simple hierarchy of empty classes" $
      let baseName = "SimpleEmptyClassHierarchy"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 18)
                  [ ClassDef (srcSp 3 1 3 14)
                      (srcSp 3 7 3 7, ClassName "A")
                      Nothing
                      []
                  , ClassDef (srcSp 5 1 5 18)
                      (srcSp 5 7 5 7, ClassName "B")
                      (Just (srcSp 5 11 5 11, ClassName "A"))
                      []]
                  [FunDef (srcSp 1 1 1 22)
                     (srcSp 1 5 1 8, FunName "main")
                     (FunType (srcSp 1 12 1 15)
                        []
                        (SrcTyUnit $ srcSp 1 12 1 15))
                     []
                     False]
      in successCase baseName ast

    -- Failure
    describe "gives an error message" $ do
      it "given a program without main function" $
        failureCase "MainNotDefined"

      it "given a program with incorrect type of main" $
        failureCase "MainIncorrectType"

      it "given a program with pure main" $
        failureCase "MainPure"

      it "given a class redefinition" $
        failureCase "ClassRedefinition"

      it "given a function redefinition" $
        failureCase "FunRedefinition"

      it "given a class inheriting from a non-existent class" $
        failureCase "InheritingNonExistentClass"

      it "given a cyclic class hierarchy" $
        failureCase "CyclicClassHierarchy"

-- Infrastructure

-- | Takes a file base name and an expected AST and performs a test (by
-- comparing showed ASTs).
successCase :: String -> TyProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  let Right srcProgram = parseOOLang (mkFileName baseName) input
  case typeCheck srcProgram of
    Right (tyProgram, _) -> show tyProgram `shouldBe` show result
    Left err -> error $ prPrint err

-- | Takes a file base name and reads a source program.
successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

-- | Takes a file base name and performs a test (by comparing pretty printed
-- error message).
failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Right srcProgram = parseOOLang (mkFileName baseName) input
  let Left err = typeCheck srcProgram
  prPrint err `shouldBe` errMsg

-- | Takes a file base name and reads a source program and expected error
-- message (from .err file).
failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))

