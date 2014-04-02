module OOLang.ParserSpec (main, spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import OOLang.AST
import OOLang.Parser
import OOLang.SrcSpan

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
  describe "parseOOLang" $ do
    -- Success
    it "parses class definitions" $
      let baseName = "Classes"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 3)
                  [ ClassDef (srcSp 1 1 2 3)
                      (srcSp 1 7 1 11, ClassName "Shape")
                      Nothing
                      []
                  , ClassDef (srcSp 4 1 5 3)
                      (srcSp 4 7 4 12, ClassName "Circle")
                      (Just (srcSp 4 16 4 20, ClassName "Shape"))
                      []]
                  []
      in successCase baseName ast

    it "parses function definitions" $
      let baseName = "Functions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 3)
                  []
                  [ FunDef (srcSp 1 1 2 3)
                      (srcSp 1 5 1 5, FunName "f")
                      (SrcFunType (srcSp 1 9 1 56)
                         [ VarBinder (srcSp 1 9 1 17)
                             (srcSp 1 10 1 10, Var "x")
                             (SrcTyInt (srcSp 1 14 1 16))
                         , VarBinder (srcSp 1 22 1 31)
                             (srcSp 1 23 1 23, Var "y")
                             (SrcTyBool (srcSp 1 27 1 30))]
                         (SrcTyArrow (srcSp 1 36 1 56)
                            (SrcTyParen (srcSp 1 36 1 48)
                               (SrcTyArrow (srcSp 1 37 1 47)
                                 (SrcTyInt (srcSp 1 37 1 39))
                                 (SrcTyBool (srcSp 1 44 1 47))))
                            (SrcTyUnit (srcSp 1 53 1 56))))
                      []
                      False
                  , FunDef (srcSp 4 1 5 3)
                      (srcSp 4 10 4 10, FunName "g")
                      (SrcFunType (srcSp 4 14 4 16)
                         []
                         (SrcTyInt (srcSp 4 14 4 16)))
                      []
                      True]
      in successCase baseName ast

    it "parses class and function definitions together" $
      let baseName = "ClassesAndFunctions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 26)
                  [ ClassDef (srcSp 1 1 1 18)
                      (srcSp 1 7 1 11, ClassName "Super")
                      Nothing
                      []
                  , ClassDef (srcSp 6 1 6 26)
                      (srcSp 6 7 6 11, ClassName "Child")
                      (Just (srcSp 6 15 6 19, ClassName "Super"))
                      []]
                  [FunDef (srcSp 3 1 4 3)
                     (srcSp 3 5 3 7, FunName "fun")
                     (SrcFunType (srcSp 3 11 3 34)
                        [VarBinder (srcSp 3 11 3 26)
                           (srcSp 3 12 3 12, Var "f")
                           (SrcTyArrow (srcSp 3 16 3 25)
                              (SrcTyInt (srcSp 3 16 3 18))
                              (SrcTyInt (srcSp 3 23 3 25)))]
                        (SrcTyUnit (srcSp 3 31 3 34)))
                     []
                     False]
      in successCase baseName ast

    it "parses types" $
      let baseName = "Types"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 3)
                  []
                  [FunDef (srcSp 1 1 5 3)
                     (srcSp 1 5 1 7, FunName "fun")
                     (SrcFunType (srcSp 1 11 4 39)
                        []
                        (SrcTyArrow (srcSp 1 11 4 39)
                           (SrcTyParen (srcSp 1 11 1 22)
                             (SrcTyArrow (srcSp 1 12 1 21)
                                (SrcTyInt (srcSp 1 12 1 14))
                                (SrcTyInt (srcSp 1 19 1 21))))
                           (SrcTyArrow (srcSp 2 11 4 39)
                              (SrcTyMutable (srcSp 2 11 2 40)
                                 (SrcTyMaybe (srcSp 2 20 2 39)
                                    (SrcTyArrow (srcSp 2 27 2 38)
                                       (SrcTyBool (srcSp 2 27 2 30))
                                       (SrcTyBool (srcSp 2 35 2 38)))))
                              (SrcTyArrow (srcSp 3 11 4 39)
                                 (SrcTyRef (srcSp 3 11 3 18)
                                    (SrcTyBool (srcSp 3 15 3 18)))
                                 (SrcTyRef (srcSp 4 11 4 39)
                                    (SrcTyArrow (srcSp 4 16 4 38)
                                       (SrcTyMaybe (srcSp 4 16 4 25)
                                          (SrcTyBool (srcSp 4 22 4 25)))
                                       (SrcTyMaybe (srcSp 4 30 4 38)
                                          (SrcTyInt (srcSp 4 36 4 38)))))))))
                     []
                     False]
      in successCase baseName ast

    -- Failure
    describe "gives an error message" $ do
      it "given an empty program" $
        failureCase "Empty"

      it "given a class definition with missing class name" $
        failureCase "MissingClassName"

      it "given a class definition with lower case name" $
        failureCase "ClassNameLower"

      it "given a class definition with missing fat arrow" $
        failureCase "ClassMissingFatArrow"

      it "given a class definition with missing end" $
        failureCase "ClassMissingEnd"

      it "given a class definition with missing super class name" $
        failureCase "MissingSuperClassName"

      it "given a function definition with missing def" $
        failureCase "FunMissingDef"

      it "given a function definition with missing name" $
        failureCase "MissingFunName"

      it "given a function definition with upper case name" $
        failureCase "FunNameUpper"

      it "given a function definition with missing type" $
        failureCase "MissingFunType"

      it "given a function definition with missing fat arrow" $
        failureCase "FunMissingFatArrow"

      it "given a function definition with missing end" $
        failureCase "FunMissingEnd"

      it "given a variable binder with missing variable name" $
        failureCase "VarBinderMissingName"

      it "given a variable binder with missing type" $
        failureCase "VarBinderMissingType"

      it "given a variable binder with unbalanced curlies" $
        failureCase "VarBinderUnbalancedCurlies"

      it "given a nested Ref type" $
        failureCase "NestedRefType"

      it "given a wrong nesting of Maybe and Mutable types" $
        failureCase "WrongMaybeMutableNesting"

      it "given an unparenthesised Mutable nesting" $
        failureCase "UnparenthesisedMutable"

      it "given an unparenthesised Maybe nesting" $
        failureCase "UnparenthesisedMaybe"

      it "given an ill-formed arrow type" $
        failureCase "IllFormedArrowType"

      it "given a lower case type name" $
        failureCase "TypeLowerCase"

-- Infrastructure

successCase :: String -> SrcProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  let Right pr = parseOOLang (mkFileName baseName) input
  show pr `shouldBe` show result

successRead :: String -> IO String
successRead baseName = readFile (successDir </> mkFileName baseName)

failureCase :: String -> IO ()
failureCase baseName = do
  (input, errMsg) <- failureRead baseName
  let Left err = parseOOLang (mkFileName baseName) input
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
mkFileName baseName = baseName <.> "ool"

