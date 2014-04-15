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
                          [SrcTyCon (srcSp 5 24 5 24, TypeName "B")]
                      , ConDef (srcSp 6 18 6 47)
                          (srcSp 6 18 6 21, ConName "Node")
                          [ SrcTyCon (srcSp 6 23 6 23, TypeName "A")
                          , SrcTyParen (srcSp 6 25 6 35)
                              (SrcTyApp (srcSp 6 26 6 34)
                                 (SrcTyApp (srcSp 6 26 6 32)
                                   (SrcTyCon (srcSp 6 26 6 30, TypeName "Tree2"))
                                   (SrcTyCon (srcSp 6 32 6 32, TypeName "A")))
                                 (SrcTyCon (srcSp 6 34 6 34, TypeName "B")))
                          , SrcTyParen (srcSp 6 37 6 47)
                              (SrcTyApp (srcSp 6 38 6 46)
                                 (SrcTyApp (srcSp 6 38 6 44)
                                    (SrcTyCon (srcSp 6 38 6 42, TypeName "Tree2"))
                                    (SrcTyCon (srcSp 6 44 6 44, TypeName "A")))
                                 (SrcTyCon (srcSp 6 46 6 46, TypeName "B")))]]]
                  []
      in successCase baseName ast

    it "parses functions" $
      let baseName = "Functions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 7 10)
                  []
                  [ FunDef (srcSp 1 1 3 10)
                      (srcSp 1 1 1 1, FunName "f")
                      (SrcTyCon (srcSp 1 5 1 8, TypeName "Unit"))
                      [ FunEq (srcSp 2 1 2 9)
                          (srcSp 2 1 2 1, FunName "f")
                          []
                          (LitE (srcSp 2 5 2 8, UnitLit))
                      , FunEq (srcSp 3 1 3 9)
                          (srcSp 3 1 3 1, FunName "f")
                          []
                          (LitE (srcSp 3 5 3 8, UnitLit))]
                  , FunDef (srcSp 5 1 7 10)
                      (srcSp 5 1 5 1, FunName "g")
                      (SrcTyArrow (srcSp 5 5 5 15)
                         (SrcTyCon (srcSp 5 5 5 8, TypeName "Unit"))
                         (SrcTyCon (srcSp 5 13 5 15, TypeName "Int")))
                      [ FunEq (srcSp 6 1 6 6)
                          (srcSp 6 1 6 1, FunName "g")
                          []
                          (VarE (srcSp 6 5 6 5) (Var "x"))
                      , FunEq (srcSp 7 1 7 9)
                          (srcSp 7 1 7 1, FunName "g")
                          []
                          (LitE (srcSp 7 5 7 8, UnitLit))]]
      in successCase baseName ast

    it "parses data types and functions" $
      let baseName = "DataTypesAndFunctions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 16)
                  [ TypeDef (srcSp 1 1 1 12)
                      (srcSp 1 6 1 7, TypeName "Id")
                      []
                      [ConDef (srcSp 1 11 1 12)
                         (srcSp 1 11 1 12, ConName "Id")
                         []]
                  , TypeDef (srcSp 6 1 6 16)
                      (srcSp 6 6 6 6, TypeName "A")
                      [(srcSp 6 8 6 8, TypeVar "T")]
                      [ConDef (srcSp 6 12 6 16)
                         (srcSp 6 12 6 14, ConName "MkA")
                         [SrcTyCon (srcSp 6 16 6 16, TypeName "T")]]]
                  [ FunDef (srcSp 3 1 4 10)
                      (srcSp 3 1 3 1, FunName "f")
                      (SrcTyCon (srcSp 3 5 3 7, TypeName "Int"))
                      [FunEq (srcSp 4 1 4 9)
                         (srcSp 4 1 4 1, FunName "f")
                         []
                         (LitE (srcSp 4 5 4 8, UnitLit))]]
      in successCase baseName ast

    it "parses types" $
      let baseName = "Types"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 4 10)
                  []
                  [FunDef (srcSp 1 1 4 10)
                     (srcSp 1 1 1 1, FunName "f")
                     (SrcTyForAll (srcSp 1 5 3 47)
                        (srcSp 1 12 1 12, TypeVar "A")
                        (SrcTyArrow (srcSp 1 16 3 47)
                           (SrcTyParen (srcSp 1 16 1 23)
                              (SrcTyArrow (srcSp 1 17 1 22)
                                 (SrcTyCon (srcSp 1 17 1 17, TypeName "A"))
                                 (SrcTyCon (srcSp 1 22 1 22, TypeName "A"))))
                           (SrcTyArrow (srcSp 2 16 3 47)
                              (SrcTyCon (srcSp 2 16 2 18, TypeName "Int"))
                              (SrcTyParen (srcSp 3 16 3 47)
                                 (SrcTyArrow (srcSp 3 17 3 46)
                                    (SrcTyApp (srcSp 3 17 3 30)
                                       (SrcTyApp (srcSp 3 17 3 25)
                                          (SrcTyCon (srcSp 3 17 3 21, TypeName "State"))
                                          (SrcTyCon (srcSp 3 23 3 25, TypeName "Int")))
                                       (SrcTyCon (srcSp 3 27 3 30, TypeName "Unit")))
                                    (SrcTyApp (srcSp 3 35 3 46)
                                       (SrcTyParen (srcSp 3 35 3 42)
                                          (SrcTyApp (srcSp 3 36 3 41)
                                             (SrcTyCon (srcSp 3 36 3 39, TypeName "Pair"))
                                             (SrcTyCon (srcSp 3 41 3 41, TypeName "A"))))
                                       (SrcTyCon (srcSp 3 44 3 46, TypeName "Int"))))))))
                     [FunEq (srcSp 4 1 4 9)
                        (srcSp 4 1 4 1, FunName "f")
                        []
                        (LitE (srcSp 4 5 4 8, UnitLit))]]
      in successCase baseName ast

    it "parses expressions" $
      let baseName = "Expressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 2 28)
                  []
                  [ FunDef (srcSp 1 1 2 28)
                      (srcSp 1 1 1 3, FunName "fun")
                      (SrcTyCon (srcSp 1 7 1 10, TypeName "Unit"))
                      [FunEq (srcSp 2 1 2 27)
                         (srcSp 2 1 2 3, FunName "fun")
                         []
                         (BinOpE (srcSp 2 7 2 26)
                            (srcSp 2 16 2 16, App)
                            (BinOpE (srcSp 2 7 2 15)
                               (srcSp 2 8 2 8, App)
                               (VarE (srcSp 2 7 2 7) (Var "f"))
                               (LitE (srcSp 2 9 2 15, StringLit "Hello")))
                            (ParenE (srcSp 2 17 2 26)
                               (BinOpE (srcSp 2 18 2 25)
                                  (srcSp 2 21 2 21, App)
                                  (BinOpE (srcSp 2 18 2 20)
                                     (srcSp 2 19 2 19, App)
                                     (VarE (srcSp 2 18 2 18) (Var "h"))
                                     (LitE (srcSp 2 20 2 20, IntLit 1)))
                                  (LitE (srcSp 2 22 2 25, FloatLit 0.01 "0.01")))))]]
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
  case parseFunLang (mkFileName baseName) input of
    Right pr -> show pr `shouldBe` show result
    Left err -> error $ prPrint err

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

