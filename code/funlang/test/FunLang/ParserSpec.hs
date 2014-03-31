module FunLang.ParserSpec (main, spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec
import Control.Monad (liftM2)
import Control.Applicative ((<$>))
import Text.Show.Pretty

import FunLang.AST
import FunLang.Parser
import FunLang.SrcSpan

main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "parsertests"

successDir :: FilePath
successDir = testDir </> "success"

failureDir :: FilePath
failureDir = testDir </> "failure"

spec :: Spec
spec =
  describe "parseFunLang" $ do
    -- Success
    it "parses a program with single simplest data type" $
      let baseName = "SingleSimpleDataType"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 1 10)
                  [TypeDef (srcSp 1 1 1 10)
                     (srcSp 1 6 1 6, TypeName "T")
                     []
                     [ConDef (srcSp 1 10 1 10)
                        (srcSp 1 10 1 10, ConName "T")
                        []]]
                  []
      in successCase baseName ast

    it "parses a program with single simple data type with type variables" $
      let baseName = "SingleDataTypeWithTypeVars"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 1 14)
                  [TypeDef (srcSp 1 1 1 14)
                     (srcSp 1 6 1 6, TypeName "T")
                     [(srcSp 1 8 1 8, TypeVar "A"),
                      (srcSp 1 10 1 10, TypeVar "B")]
                     [ConDef (srcSp 1 14 1 14)
                        (srcSp 1 14 1 14, ConName "C")
                        []]]
                  []
      in successCase baseName ast

    it "parses a program with single simple data type with several simple constructors" $
      let baseName = "SingleDataTypeWithSeveralSimpleConstructors"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 1 16)
                  [TypeDef (srcSp 1 1 1 16)
                     (srcSp 1 6 1 6, TypeName "T")
                     []
                     [ConDef (srcSp 1 10 1 11)
                        (srcSp 1 10 1 11, ConName "C1")
                        [],
                      ConDef (srcSp 1 15 1 16)
                        (srcSp 1 15 1 16, ConName "C2")
                        []]]
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

successCase :: String -> SrcProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  let Right pr = parseFunLang (mkFileName baseName) input
  ppShow pr `shouldBe` ppShow result

successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Left err = parseFunLang (mkFileName baseName) input
  prPrint err `shouldBe` errMsg

failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))
  where dropNewLine "" = ""
        dropNewLine str = let l = last str
                          in if l == '\n' || l == '\r'
                             then dropNewLine (init str)
                             else str

mkFileName :: String -> String
mkFileName baseName = baseName <.> "fl"

