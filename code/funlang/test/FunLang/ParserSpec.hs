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
                      (TypeName "T", srcSp 1 6 1 6)
                      []
                      [ConDef (srcSp 1 10 1 12)
                         (ConName "MkT", srcSp 1 10 1 12)
                         []]
                  , TypeDef (srcSp 3 1 3 24)
                      (TypeName "Bool", srcSp 3 6 3 9)
                      []
                      [ ConDef (srcSp 3 13 3 16)
                          (ConName "True", srcSp 3 13 3 16)
                          []
                      , ConDef (srcSp 3 20 3 24)
                          (ConName "False", srcSp 3 20 3 24)
                          []]
                  , TypeDef (srcSp 5 1 6 47)
                      (TypeName "Tree2", srcSp 5 6 5 10)
                      [ (TypeVar "A", srcSp 5 12 5 12)
                      , (TypeVar "B", srcSp 5 14 5 14)]
                      [ ConDef (srcSp 5 18 5 24)
                          (ConName "Empty", srcSp 5 18 5 22)
                          [SrcTyCon (TypeName "B", srcSp 5 24 5 24)]
                      , ConDef (srcSp 6 18 6 47)
                          (ConName "Node", srcSp 6 18 6 21)
                          [ SrcTyCon (TypeName "A", srcSp 6 23 6 23)
                          , SrcTyParen (srcSp 6 25 6 35)
                              (SrcTyApp (srcSp 6 26 6 34)
                                 (SrcTyApp (srcSp 6 26 6 32)
                                   (SrcTyCon (TypeName "Tree2", srcSp 6 26 6 30))
                                   (SrcTyCon (TypeName "A", srcSp 6 32 6 32)))
                                 (SrcTyCon (TypeName "B", srcSp 6 34 6 34)))
                          , SrcTyParen (srcSp 6 37 6 47)
                              (SrcTyApp (srcSp 6 38 6 46)
                                 (SrcTyApp (srcSp 6 38 6 44)
                                    (SrcTyCon (TypeName "Tree2", srcSp 6 38 6 42))
                                    (SrcTyCon (TypeName "A", srcSp 6 44 6 44)))
                                 (SrcTyCon (TypeName "B", srcSp 6 46 6 46)))]]]
                  []
      in successCase baseName ast

    it "parses functions" $
      let baseName = "Functions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 7 10)
                  []
                  [ FunDef (srcSp 1 1 3 28)
                      (FunName "f", srcSp 1 1 1 1)
                      (SrcTyCon (TypeName "Unit", srcSp 1 5 1 8))
                      [ FunEq (srcSp 2 1 2 27)
                          (FunName "f", srcSp 2 1 2 1)
                          [ ConP (srcSp 2 3 2 6)
                              (ConName "True", srcSp 2 3 2 6)
                              []
                          , LitP (IntLit 0, srcSp 2 11 2 11)]
                          (LitE (UnitLit, srcSp 2 23 2 26))
                      , FunEq (srcSp 3 1 3 27)
                          (FunName "f", srcSp 3 1 3 1)
                          [ ParenP (srcSp 3 3 3 9)
                              (ConP (srcSp 3 4 3 8)
                                 (ConName "Con", srcSp 3 4 3 6)
                                 [DefaultP $ srcSp 3 8 3 8])
                          , ParenP (srcSp 3 11 3 19)
                              (VarP (VarBinder (srcSp 3 12 3 18)
                                 (Var "n", srcSp 3 12 3 12)
                                 (SrcTyCon (TypeName "Int", srcSp 3 16 3 18))))]
                          (LitE (UnitLit, srcSp 3 23 3 26))]
                  , FunDef (srcSp 5 1 7 10)
                      (FunName "g", srcSp 5 1 5 1)
                      (SrcTyArrow (srcSp 5 5 5 15)
                         (SrcTyCon (TypeName "Unit", srcSp 5 5 5 8))
                         (SrcTyCon (TypeName "Int", srcSp 5 13 5 15)))
                      [ FunEq (srcSp 6 1 6 6)
                          (FunName "g", srcSp 6 1 6 1)
                          []
                          (VarE (srcSp 6 5 6 5) (Var "x"))
                      , FunEq (srcSp 7 1 7 9)
                          (FunName "g", srcSp 7 1 7 1)
                          []
                          (LitE (UnitLit, srcSp 7 5 7 8))]]
      in successCase baseName ast

    it "parses data types and functions" $
      let baseName = "DataTypesAndFunctions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 16)
                  [ TypeDef (srcSp 1 1 1 12)
                      (TypeName "Id", srcSp 1 6 1 7)
                      []
                      [ConDef (srcSp 1 11 1 12)
                         (ConName "Id", srcSp 1 11 1 12)
                         []]
                  , TypeDef (srcSp 6 1 6 16)
                      (TypeName "A", srcSp 6 6 6 6)
                      [(TypeVar "T", srcSp 6 8 6 8)]
                      [ConDef (srcSp 6 12 6 16)
                         (ConName "MkA", srcSp 6 12 6 14)
                         [SrcTyCon (TypeName "T", srcSp 6 16 6 16)]]]
                  [ FunDef (srcSp 3 1 4 10)
                      (FunName "f", srcSp 3 1 3 1)
                      (SrcTyCon (TypeName "Int", srcSp 3 5 3 7))
                      [FunEq (srcSp 4 1 4 9)
                         (FunName "f", srcSp 4 1 4 1)
                         []
                         (LitE (UnitLit, srcSp 4 5 4 8))]]
      in successCase baseName ast

    it "parses types" $
      let baseName = "Types"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 4 10)
                  []
                  [FunDef (srcSp 1 1 4 10)
                     (FunName "f", srcSp 1 1 1 1)
                     (SrcTyForAll (srcSp 1 5 3 47)
                        (TypeVar "A", srcSp 1 12 1 12)
                        (SrcTyArrow (srcSp 1 16 3 47)
                           (SrcTyParen (srcSp 1 16 1 23)
                              (SrcTyArrow (srcSp 1 17 1 22)
                                 (SrcTyCon (TypeName "A", srcSp 1 17 1 17))
                                 (SrcTyCon (TypeName "A", srcSp 1 22 1 22))))
                           (SrcTyArrow (srcSp 2 16 3 47)
                              (SrcTyCon (TypeName "Int", srcSp 2 16 2 18))
                              (SrcTyParen (srcSp 3 16 3 47)
                                 (SrcTyArrow (srcSp 3 17 3 46)
                                    (SrcTyApp (srcSp 3 17 3 30)
                                       (SrcTyApp (srcSp 3 17 3 25)
                                          (SrcTyCon (TypeName "State", srcSp 3 17 3 21))
                                          (SrcTyCon (TypeName "Int", srcSp 3 23 3 25)))
                                       (SrcTyCon (TypeName "Unit", srcSp 3 27 3 30)))
                                    (SrcTyApp (srcSp 3 35 3 46)
                                       (SrcTyParen (srcSp 3 35 3 42)
                                          (SrcTyApp (srcSp 3 36 3 41)
                                             (SrcTyCon (TypeName "Pair", srcSp 3 36 3 39))
                                             (SrcTyCon (TypeName "A", srcSp 3 41 3 41))))
                                       (SrcTyCon (TypeName "Int", srcSp 3 44 3 46))))))))
                     [FunEq (srcSp 4 1 4 9)
                        (FunName "f", srcSp 4 1 4 1)
                        []
                        (LitE (UnitLit, srcSp 4 5 4 8))]]
      in successCase baseName ast

    it "parses lambda, application and literal expressions" $
      let baseName = "LamAppLitExpressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 38)
                  []
                  [ FunDef (srcSp 1 1 2 40)
                      (FunName "fun", srcSp 1 1 1 3)
                      (SrcTyCon (TypeName "Unit", srcSp 1 7 1 10))
                      [FunEq (srcSp 2 1 2 39)
                         (FunName "fun", srcSp 2 1 2 3)
                         []
                         (BinOpE (srcSp 2 7 2 38)
                            (App, srcSp 2 28 2 28)
                            (ParenE (srcSp 2 7 2 27)
                               (LambdaE (srcSp 2 8 2 26)
                                  [VarBinder (srcSp 2 10 2 17)
                                     (Var "y", srcSp 2 10 2 10)
                                     (SrcTyCon (TypeName "Unit", srcSp 2 14 2 17))]
                                  (LitE (UnitLit, srcSp 2 23 2 26))))
                            (ParenE (srcSp 2 29 2 38)
                               (BinOpE (srcSp 2 30 2 37)
                                  (App, srcSp 2 33 2 33)
                                  (BinOpE (srcSp 2 30 2 32)
                                     (App, srcSp 2 31 2 31)
                                     (VarE (srcSp 2 30 2 30) (Var "h"))
                                     (LitE (IntLit 1, srcSp 2 32 2 32)))
                                  (LitE (FloatLit 0.01 "0.01", srcSp 2 34 2 37)))))]
                  , FunDef (srcSp 4 1 5 38)
                      (FunName "fun2", srcSp 4 1 4 4)
                      (SrcTyCon (TypeName "Unit", srcSp 4 8 4 11))
                      [FunEq (srcSp 5 1 5 37)
                         (FunName "fun2", srcSp 5 1 5 4)
                         []
                         (BinOpE (srcSp 5 8 5 36)
                            (App, srcSp 5 35 5 35)
                            (TypeAppE (srcSp 5 8 5 34)
                               (ParenE (srcSp 5 8 5 28)
                                  (TypeLambdaE (srcSp 5 9 5 27)
                                     [(TypeVar "T", srcSp 5 11 5 11)]
                                     (LambdaE (srcSp 5 15 5 27)
                                        [VarBinder (srcSp 5 17 5 21)
                                           (Var "x", srcSp 5 17 5 17)
                                           (SrcTyCon (TypeName "T", srcSp 5 21 5 21))]
                                        (VarE (srcSp 5 27 5 27) (Var "x")))))
                                  (SrcTyCon (TypeName "Int", srcSp 5 31 5 33)))
                            (LitE (IntLit 1, srcSp 5 36 5 36)))]]
      in successCase baseName ast

    it "parses case expressions and constructor names" $
      let baseName = "CaseConExpressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 5)
                  []
                  [FunDef (srcSp 1 1 6 5)
                     (FunName "fun", srcSp 1 1 1 3)
                     (SrcTyCon (TypeName "Unit", srcSp 1 7 1 10))
                     [FunEq (srcSp 2 1 6 4)
                        (FunName "fun", srcSp 2 1 2 3)
                        []
                        (CaseE (srcSp 2 7 6 3)
                           (LitE (IntLit 0, srcSp 2 12 2 12))
                           [ CaseAlt (srcSp 3 3 3 13)
                               (LitP (IntLit 0, srcSp 3 5 3 5))
                               (ConNameE (ConName "True", srcSp 3 10 3 13))
                           , CaseAlt (srcSp 4 3 4 41)
                               (ConP (srcSp 4 5 4 33)
                                  (ConName "Con", srcSp 4 5 4 7)
                                  [ ParenP (srcSp 4 9 4 17)
                                      (VarP (VarBinder (srcSp 4 10 4 16)
                                         (Var "x", srcSp 4 10 4 10)
                                         (SrcTyCon (TypeName "Int", srcSp 4 14 4 16))))
                                  , DefaultP (srcSp 4 19 4 19)
                                  , ConP (srcSp 4 21 4 24)
                                      (ConName "True", srcSp 4 21 4 24)
                                      []
                                  , ParenP (srcSp 4 26 4 33)
                                      (ConP (srcSp 4 27 4 32)
                                         (ConName "Con2", srcSp 4 27 4 30)
                                         [LitP (IntLit 0, srcSp 4 32 4 32)])])
                               (ConNameE (ConName "True", srcSp 4 38 4 41))
                           , CaseAlt (srcSp 5 3 5 14)
                               (DefaultP $ srcSp 5 5 5 5)
                               (ConNameE (ConName "False", srcSp 5 10 5 14))])]]
      in successCase baseName ast

    it "parses binary operations" $
      let baseName = "BinOpExpressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 2 69)
                  []
                  [FunDef (srcSp 1 1 2 69)
                     (FunName "fun", srcSp 1 1 1 3)
                     (SrcTyCon (TypeName "Int", srcSp 1 7 1 9))
                     [FunEq (srcSp 2 1 2 68)
                        (FunName "fun", srcSp 2 1 2 3)
                        []
                        (TypeLambdaE (srcSp 2 7 2 67)
                           [(TypeVar "T", srcSp 2 9 2 9)]
                           (LambdaE (srcSp 2 13 2 67)
                              [VarBinder (srcSp 2 15 2 19)
                                 (Var "x", srcSp 2 15 2 15)
                                 (SrcTyCon (TypeName "T", srcSp 2 19 2 19))]
                              (LambdaE (srcSp 2 25 2 67)
                                 [VarBinder (srcSp 2 27 2 31)
                                    (Var "y", srcSp 2 27 2 27)
                                    (SrcTyCon (TypeName "T", srcSp 2 31 2 31))]
                                 (BinOpE (srcSp 2 37 2 67)
                                    (Equal, srcSp 2 57 2 57)
                                    (BinOpE (srcSp 2 37 2 55)
                                       (Add, srcSp 2 43 2 43)
                                       (BinOpE (srcSp 2 37 2 41)
                                          (Add, srcSp 2 39 2 39)
                                          (LitE (IntLit 1, srcSp 2 37 2 37))
                                          (LitE (IntLit 2, srcSp 2 41 2 41)))
                                       (BinOpE (srcSp 2 45 2 55)
                                          (Mul, srcSp 2 53 2 53)
                                          (ParenE (srcSp 2 45 2 51)
                                             (BinOpE (srcSp 2 46 2 50)
                                                (Add, srcSp 2 48 2 48)
                                                (LitE (IntLit 3, srcSp 2 46 2 46))
                                                (LitE (IntLit 4, srcSp 2 50 2 50))))
                                          (LitE (IntLit 5, srcSp 2 55 2 55))))
                                    (BinOpE (srcSp 2 59 2 67)
                                       (Mul, srcSp 2 65 2 65)
                                       (BinOpE (srcSp 2 59 2 63)
                                          (Mul, srcSp 2 61 2 61)
                                          (LitE (IntLit 2, srcSp 2 59 2 59))
                                          (LitE (IntLit 3, srcSp 2 63 2 63)))
                                       (LitE (IntLit 4, srcSp 2 67 2 67)))))))]]
      in successCase baseName ast

    it "parses statements (do-expressions)" $
      let baseName = "Statements"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 5)
                  []
                  [FunDef (srcSp 1 1 6 5)
                     (FunName "main", srcSp 1 1 1 4)
                     (SrcTyApp (srcSp 1 8 1 14)
                        (SrcTyCon (TypeName "IO", srcSp 1 8 1 9))
                        (SrcTyCon (TypeName "Unit", srcSp 1 11 1 14)))
                     [FunEq (srcSp 2 1 6 4)
                        (FunName "main", srcSp 2 1 2 4)
                        []
                        (DoE (srcSp 2 8 6 3)
                           [ ExprS (srcSp 3 3 3 13)
                               (BinOpE (srcSp 3 3 3 12)
                                  (App, srcSp 3 11 3 11)
                                  (VarE (srcSp 3 3 3 10) (Var "printInt"))
                                  (LitE (IntLit 1, srcSp 3 12 3 12)))
                           , BindS (srcSp 4 3 4 25)
                               (VarBinder (srcSp 4 3 4 10)
                                  (Var "x", srcSp 4 3 4 3)
                                  (SrcTyCon (TypeName "Unit", srcSp 4 7 4 10)))
                               (BinOpE (srcSp 4 15 4 24)
                                  (App, srcSp 4 23 4 23)
                                  (VarE (srcSp 4 15 4 22) (Var "printInt"))
                                  (LitE (IntLit 2, srcSp 4 24 4 24)))
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

