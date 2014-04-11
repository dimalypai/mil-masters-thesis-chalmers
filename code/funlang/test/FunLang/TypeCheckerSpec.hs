-- | Type checker tests.
module FunLang.TypeCheckerSpec (main, spec) where

import Test.Hspec
import System.FilePath ((</>), (<.>))
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import FunLang.AST
import FunLang.Parser
import FunLang.TypeChecker
import FunLang.SrcSpan
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
    it "type checks simple data type and function" $
      -- TODO: Fix IO and unit.
      -- For now, the type checker erases function bodies.
      let baseName = "SimpleDataTypeAndFunction"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 7 12)
                  [TypeDef (srcSp 4 1 4 12)
                     (srcSp 4 6 4 6, TypeName "T")
                     []
                     [ConDef (srcSp 4 10 4 12)
                        (srcSp 4 10 4 12, ConName "MkT")
                        []]]
                  [ FunDef (srcSp 1 1 2 13)
                      (srcSp 1 1 1 4, FunName "main")
                      (SrcTyApp (srcSp 1 8 1 14)
                         (SrcTyCon (srcSp 1 8 1 9, TypeName "IO"))
                         (SrcTyCon (srcSp 1 11 1 14, TypeName "Unit")))
                      []
                  , FunDef (srcSp 6 1 7 12)
                      (srcSp 6 1 6 3, FunName "fun")
                      (SrcTyCon (srcSp 6 7 6 10, TypeName "Unit"))
                      []]
      in successCase baseName ast

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

-- Infrastructure

-- | Takes a file base name and an expected AST and performs a test (by
-- comparing showed ASTs).
successCase :: String -> TyProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  let Right srcProgram = parseFunLang (mkFileName baseName) input
  let Right (tyProgram, _) = typeCheck srcProgram
  show tyProgram `shouldBe` show result

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

