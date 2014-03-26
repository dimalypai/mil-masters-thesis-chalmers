module FunLang.ParserSpec (main, spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import FunLang.AST
import FunLang.AST.DebugShow
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
          fileName = baseName <.> "fl"
          ast = Program (mkSrcSpan fileName 1 1 1 10)
                  [TypeDef (mkSrcSpan fileName 1 1 1 10)
                     (mkSrcSpan fileName 1 6 1 6, TypeName "T")
                     []
                     [ConDef (mkSrcSpan fileName 1 10 1 10)
                        (mkSrcSpan fileName 1 10 1 10, ConName "T")
                        []]]
                  []
      in successCase baseName ast

    -- Failure
    describe "gives an error message when" $ do
      it "given an empty program" $
        failureCase "Empty"

-- Infrastructure

successCase :: String -> SrcProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  let Right pr = parseFunLang (baseName <.> "fl") input
  renderDebug pr `shouldBe` renderDebug result

successRead :: String -> IO String
successRead baseName = readFile (successDir </> baseName <.> "fl")

failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Left err = parseFunLang (baseName <.> "fl") input
  err `shouldBe` errMsg

failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> baseName <.> "fl"))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))
  where dropNewLine "" = ""
        dropNewLine str = let l = last str
                          in if l == '\n' || l == '\r'
                             then dropNewLine (init str)
                             else str

