-- | Parser tests.
module FunLang.ParserSpec (main, spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import FunLang.AST
import FunLang.Parser
import FunLang.SrcSpan
import FunLang.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "parsertests"

successDir :: FilePath
successDir = testDir </> "success"

failureDir :: FilePath
failureDir = testDir </> "failure"

-- | Main specification function.
spec :: Spec
spec =
  describe "parseFunLang" $ do
    -- Success
    it "parses data types" $
      let baseName = "DataTypes"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 47)
                  [ TypeDef (srcSp 1 1 1 12)
                      (srcSp 1 6 1 6, TypeName "T")
                      []
                      [ConDef (srcSp 1 10 1 12)
                         (srcSp 1 10 1 12, ConName "MkT")
                         []]
                  , TypeDef (srcSp 3 1 3 24)
                      (srcSp 3 6 3 9, TypeName "Bool")
                      []
                      [ ConDef (srcSp 3 13 3 16)
                          (srcSp 3 13 3 16, ConName "True")
                          []
                      , ConDef (srcSp 3 20 3 24)
                          (srcSp 3 20 3 24, ConName "False")
                          []]
                  , TypeDef (srcSp 5 1 6 47)
                      (srcSp 5 6 5 10, TypeName "Tree2")
                      [ (srcSp 5 12 5 12, TypeVar "A")
                      , (srcSp 5 14 5 14, TypeVar "B")]
                      [ ConDef (srcSp 5 18 5 24)
                          (srcSp 5 18 5 22, ConName "Empty")
                          [SrcTyApp (srcSp 5 24 5 24)
                             (srcSp 5 24 5 24, TypeName "B")
                             []]
                      , ConDef (srcSp 6 18 6 47)
                          (srcSp 6 18 6 21, ConName "Node")
                          [ SrcTyApp (srcSp 6 23 6 23)
                              (srcSp 6 23 6 23, TypeName "A")
                              []
                          , SrcTyApp (srcSp 6 25 6 35)
                              (srcSp 6 26 6 30, TypeName "Tree2")
                              [ SrcTyApp (srcSp 6 32 6 32)
                                  (srcSp 6 32 6 32, TypeName "A")
                                  []
                              , SrcTyApp (srcSp 6 34 6 34)
                                  (srcSp 6 34 6 34, TypeName "B")
                                  []]
                          , SrcTyApp (srcSp 6 37 6 47)
                              (srcSp 6 38 6 42, TypeName "Tree2")
                              [ SrcTyApp (srcSp 6 44 6 44)
                                  (srcSp 6 44 6 44, TypeName "A")
                                  []
                              , SrcTyApp (srcSp 6 46 6 46)
                                  (srcSp 6 46 6 46, TypeName "B")
                                  []]]]]
                  []
      in successCase baseName ast

    -- Failure
    describe "gives an error message when" $ do
      it "given an empty program" $
        failureCase "Empty"

      it "given a program with type definition with no constructors" $
        failureCase "TypeZeroConstructors"

      it "given a program with type definition with missing equal sign" $
        failureCase "TypeMissingEqualSign"

      it "given a program with type definition with lower case type name" $
        failureCase "TypeLowerId"

-- Infrastructure

-- | Takes a file base name and an expected AST and performs a test (by
-- comparing showed ASTs).
successCase :: String -> SrcProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  let Right pr = parseFunLang (mkFileName baseName) input
  show pr `shouldBe` show result

-- | Takes a file base name and reads a source program.
successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

-- | Takes a file base name and performs a test (by comparing pretty printed
-- error message).
failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Left err = parseFunLang (mkFileName baseName) input
  prPrint err `shouldBe` errMsg

-- | Takes a file base name and reads a source program and expected error
-- message (from .err file).
failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))

