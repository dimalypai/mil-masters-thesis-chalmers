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
                  [ FunDef (srcSp 1 1 3 28)
                      (srcSp 1 1 1 1, FunName "f")
                      (SrcTyCon (srcSp 1 5 1 8, TypeName "Unit"))
                      [ FunEq (srcSp 2 1 2 27)
                          (srcSp 2 1 2 1, FunName "f")
                          [ ConP (srcSp 2 3 2 6)
                              (srcSp 2 3 2 6, ConName "True")
                              []
                          , LitP (srcSp 2 11 2 11, IntLit 0)]
                          (LitE (srcSp 2 23 2 26, UnitLit))
                      , FunEq (srcSp 3 1 3 27)
                          (srcSp 3 1 3 1, FunName "f")
                          [ ParenP (srcSp 3 3 3 9)
                              (ConP (srcSp 3 4 3 8)
                                 (srcSp 3 4 3 6, ConName "Con")
                                 [DefaultP $ srcSp 3 8 3 8])
                          , ParenP (srcSp 3 11 3 19)
                              (VarP (VarBinder (srcSp 3 12 3 18)
                                 (srcSp 3 12 3 12, Var "n")
                                 (SrcTyCon (srcSp 3 16 3 18, TypeName "Int"))))]
                          (LitE (srcSp 3 23 3 26, UnitLit))]
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

    it "parses lambda, application and literal expressions" $
      let baseName = "LamAppLitExpressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 35)
                  []
                  [ FunDef (srcSp 1 1 2 56)
                      (srcSp 1 1 1 3, FunName "fun")
                      (SrcTyCon (srcSp 1 7 1 10, TypeName "Unit"))
                      [FunEq (srcSp 2 1 2 55)
                         (srcSp 2 1 2 3, FunName "fun")
                         []
                         (BinOpE (srcSp 2 7 2 54)
                            (srcSp 2 44 2 44, App)
                            (BinOpE (srcSp 2 7 2 43)
                               (srcSp 2 36 2 36, App)
                               (ParenE (srcSp 2 7 2 35)
                                  (LambdaE (srcSp 2 8 2 34)
                                     [ VarBinder (srcSp 2 9 2 18)
                                         (srcSp 2 9 2 9, Var "x")
                                         (SrcTyCon (srcSp 2 13 2 18, TypeName "String"))
                                     , VarBinder (srcSp 2 20 2 27)
                                         (srcSp 2 20 2 20, Var "y")
                                         (SrcTyCon (srcSp 2 24 2 27, TypeName "Unit"))]
                                     (LitE (srcSp 2 31 2 34, UnitLit))))
                               (LitE (srcSp 2 37 2 43, StringLit "Hello")))
                            (ParenE (srcSp 2 45 2 54)
                               (BinOpE (srcSp 2 46 2 53)
                                  (srcSp 2 49 2 49, App)
                                  (BinOpE (srcSp 2 46 2 48)
                                     (srcSp 2 47 2 47, App)
                                     (VarE (srcSp 2 46 2 46) (Var "h"))
                                     (LitE (srcSp 2 48 2 48, IntLit 1)))
                                  (LitE (srcSp 2 50 2 53, FloatLit 0.01 "0.01")))))]
                  , FunDef (srcSp 4 1 5 35)
                      (srcSp 4 1 4 4, FunName "fun2")
                      (SrcTyCon (srcSp 4 8 4 11, TypeName "Unit"))
                      [FunEq (srcSp 5 1 5 34)
                         (srcSp 5 1 5 4, FunName "fun2")
                         []
                         (BinOpE (srcSp 5 8 5 33)
                            (srcSp 5 32 5 32, App)
                            (TypeAppE (srcSp 5 8 5 31)
                               (ParenE (srcSp 5 8 5 25)
                                  (TypeLambdaE (srcSp 5 9 5 24)
                                     [(srcSp 5 11 5 11, TypeVar "T")]
                                     (LambdaE (srcSp 5 15 5 24)
                                        [VarBinder (srcSp 5 16 5 20)
                                           (srcSp 5 16 5 16, Var "x")
                                           (SrcTyCon (srcSp 5 20 5 20, TypeName "T"))]
                                        (VarE (srcSp 5 24 5 24) (Var "x")))))
                                  (SrcTyCon (srcSp 5 28 5 30, TypeName "Int")))
                            (LitE (srcSp 5 33 5 33, IntLit 1)))]]
      in successCase baseName ast

    it "parses case expressions and constructor names" $
      let baseName = "CaseConExpressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 5)
                  []
                  [FunDef (srcSp 1 1 6 5)
                     (srcSp 1 1 1 3, FunName "fun")
                     (SrcTyCon (srcSp 1 7 1 10, TypeName "Unit"))
                     [FunEq (srcSp 2 1 6 4)
                        (srcSp 2 1 2 3, FunName "fun")
                        []
                        (CaseE (srcSp 2 7 6 3)
                           (LitE (srcSp 2 12 2 12, IntLit 0))
                           [ CaseAlt (srcSp 3 3 3 13)
                               (LitP (srcSp 3 5 3 5, IntLit 0))
                               (ConNameE (srcSp 3 10 3 13, ConName "True"))
                           , CaseAlt (srcSp 4 3 4 41)
                               (ConP (srcSp 4 5 4 33)
                                  (srcSp 4 5 4 7, ConName "Con")
                                  [ ParenP (srcSp 4 9 4 17)
                                      (VarP (VarBinder (srcSp 4 10 4 16)
                                         (srcSp 4 10 4 10, Var "x")
                                         (SrcTyCon (srcSp 4 14 4 16, TypeName "Int"))))
                                  , DefaultP (srcSp 4 19 4 19)
                                  , ConP (srcSp 4 21 4 24)
                                      (srcSp 4 21 4 24, ConName "True")
                                      []
                                  , ParenP (srcSp 4 26 4 33)
                                      (ConP (srcSp 4 27 4 32)
                                         (srcSp 4 27 4 30, ConName "Con2")
                                         [LitP (srcSp 4 32 4 32, IntLit 0)])])
                               (ConNameE (srcSp 4 38 4 41, ConName "True"))
                           , CaseAlt (srcSp 5 3 5 14)
                               (DefaultP $ srcSp 5 5 5 5)
                               (ConNameE (srcSp 5 10 5 14, ConName "False"))])]]
      in successCase baseName ast

    it "parses statements (do-expressions)" $
      let baseName = "Statements"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 5)
                  []
                  [FunDef (srcSp 1 1 6 5)
                     (srcSp 1 1 1 4, FunName "main")
                     (SrcTyApp (srcSp 1 8 1 14)
                        (SrcTyCon (srcSp 1 8 1 9, TypeName "IO"))
                        (SrcTyCon (srcSp 1 11 1 14, TypeName "Unit")))
                     [FunEq (srcSp 2 1 6 4)
                        (srcSp 2 1 2 4, FunName "main")
                        []
                        (DoE (srcSp 2 8 6 3)
                           [ ExprS (srcSp 3 3 3 13)
                               (BinOpE (srcSp 3 3 3 12)
                                  (srcSp 3 11 3 11, App)
                                  (VarE (srcSp 3 3 3 10) (Var "printInt"))
                                  (LitE (srcSp 3 12 3 12, IntLit 1)))
                           , BindS (srcSp 4 3 4 25)
                               (VarBinder (srcSp 4 3 4 10)
                                  (srcSp 4 3 4 3, Var "x")
                                  (SrcTyCon (srcSp 4 7 4 10, TypeName "Unit")))
                               (BinOpE (srcSp 4 15 4 24)
                                  (srcSp 4 23 4 23, App)
                                  (VarE (srcSp 4 15 4 22) (Var "printInt"))
                                  (LitE (srcSp 4 24 4 24, IntLit 2)))
                           , ReturnS (srcSp 5 3 5 11)
                               (VarE (srcSp 5 10 5 10) (Var "x"))
                           ])]]
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

