-- | Parser tests.
module OOLang.ParserSpec (main, spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec
import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import OOLang.AST
import OOLang.Parser
import OOLang.SrcSpan
import OOLang.TestUtils

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
  describe "parseOOLang" $ do
    -- Success
    it "parses class definitions" $
      let baseName = "Classes"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 9 3)
                  [ ClassDef (srcSp 1 1 3 3)
                      (srcSp 1 7 1 11, ClassName "Shape")
                      Nothing
                      [FieldDecl (srcSp 2 3 2 18)
                         (Decl (srcSp 2 3 2 17)
                            (VarBinder (srcSp 2 3 2 17)
                               (srcSp 2 3 2 3, Var "x")
                               (SrcTyMutable (srcSp 2 7 2 17)
                                  (SrcTyInt $ srcSp 2 15 2 17)))
                            Nothing)
                         []]
                  , ClassDef (srcSp 5 1 9 3)
                      (srcSp 5 7 5 12, ClassName "Circle")
                      (Just (srcSp 5 16 5 20, ClassName "Shape"))
                      [MethodDecl (srcSp 6 3 8 5)
                         (FunDef (srcSp 6 3 8 5)
                            (srcSp 6 12 6 15, FunName "area")
                            (FunType (srcSp 6 19 6 21)
                               []
                               (SrcTyInt $ srcSp 6 19 6 21))
                            [ExprS (srcSp 7 5 7 6)
                               (LitE (srcSp 7 5 7 5, IntLit 1))]
                            True)
                         []]]
                  []
      in successCase baseName ast

    it "parses function definitions" $
      let baseName = "Functions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 7 3)
                  []
                  [ FunDef (srcSp 1 1 3 3)
                      (srcSp 1 5 1 5, FunName "f")
                      (FunType (srcSp 1 9 1 56)
                         [ VarBinder (srcSp 1 9 1 17)
                             (srcSp 1 10 1 10, Var "x")
                             (SrcTyInt $ srcSp 1 14 1 16)
                         , VarBinder (srcSp 1 22 1 31)
                             (srcSp 1 23 1 23, Var "y")
                             (SrcTyBool $ srcSp 1 27 1 30)]
                         (SrcTyArrow (srcSp 1 36 1 56)
                            (SrcTyParen (srcSp 1 36 1 48)
                               (SrcTyArrow (srcSp 1 37 1 47)
                                 (SrcTyInt $ srcSp 1 37 1 39)
                                 (SrcTyBool $ srcSp 1 44 1 47)))
                            (SrcTyUnit $ srcSp 1 53 1 56)))
                      [ExprS (srcSp 2 3 2 7)
                         (LitE (srcSp 2 3 2 6, UnitLit))]
                      False
                  , FunDef (srcSp 5 1 7 3)
                      (srcSp 5 10 5 10, FunName "g")
                      (FunType (srcSp 5 14 5 16)
                         []
                         (SrcTyInt $ srcSp 5 14 5 16))
                      [ExprS (srcSp 6 3 6 4)
                         (LitE (srcSp 6 3 6 3, IntLit 1))]
                      True]
      in successCase baseName ast

    it "parses class and function definitions together" $
      let baseName = "ClassesAndFunctions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 7 26)
                  [ ClassDef (srcSp 1 1 1 18)
                      (srcSp 1 7 1 11, ClassName "Super")
                      Nothing
                      []
                  , ClassDef (srcSp 7 1 7 26)
                      (srcSp 7 7 7 11, ClassName "Child")
                      (Just (srcSp 7 15 7 19, ClassName "Super"))
                      []]
                  [FunDef (srcSp 3 1 5 3)
                     (srcSp 3 5 3 7, FunName "fun")
                     (FunType (srcSp 3 11 3 34)
                        [VarBinder (srcSp 3 11 3 26)
                           (srcSp 3 12 3 12, Var "f")
                           (SrcTyArrow (srcSp 3 16 3 25)
                              (SrcTyInt $ srcSp 3 16 3 18)
                              (SrcTyInt $ srcSp 3 23 3 25))]
                        (SrcTyUnit $ srcSp 3 31 3 34))
                     [ExprS (srcSp 4 3 4 7)
                        (LitE (srcSp 4 3 4 6, UnitLit))]
                     False]
      in successCase baseName ast

    it "parses types" $
      let baseName = "Types"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 3)
                  []
                  [FunDef (srcSp 1 1 6 3)
                     (srcSp 1 5 1 7, FunName "fun")
                     (FunType (srcSp 1 11 4 39)
                        []
                        (SrcTyArrow (srcSp 1 11 4 39)
                           (SrcTyParen (srcSp 1 11 1 22)
                             (SrcTyArrow (srcSp 1 12 1 21)
                                (SrcTyInt $ srcSp 1 12 1 14)
                                (SrcTyInt $ srcSp 1 19 1 21)))
                           (SrcTyArrow (srcSp 2 11 4 39)
                              (SrcTyMutable (srcSp 2 11 2 40)
                                 (SrcTyMaybe (srcSp 2 20 2 39)
                                    (SrcTyArrow (srcSp 2 27 2 38)
                                       (SrcTyBool $ srcSp 2 27 2 30)
                                       (SrcTyBool $ srcSp 2 35 2 38))))
                              (SrcTyArrow (srcSp 3 11 4 39)
                                 (SrcTyRef (srcSp 3 11 3 18)
                                    (SrcTyBool $ srcSp 3 15 3 18))
                                 (SrcTyRef (srcSp 4 11 4 39)
                                    (SrcTyArrow (srcSp 4 16 4 38)
                                       (SrcTyMaybe (srcSp 4 16 4 25)
                                          (SrcTyBool $ srcSp 4 22 4 25))
                                       (SrcTyMaybe (srcSp 4 30 4 38)
                                          (SrcTyInt $ srcSp 4 36 4 38))))))))
                     [ExprS (srcSp 5 3 5 7)
                        (LitE (srcSp 5 3 5 6, UnitLit))]
                     False]
      in successCase baseName ast

    it "parses declaration and assignment statements" $
      let baseName = "DeclAssignStatements"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 6 3)
                  []
                  [FunDef (srcSp 1 1 6 3)
                     (srcSp 1 5 1 7, FunName "fun")
                     (FunType (srcSp 1 11 1 14)
                        []
                        (SrcTyUnit $ srcSp 1 11 1 14))
                     [ DeclS (srcSp 2 3 2 18)
                         (Decl (srcSp 2 3 2 17)
                            (VarBinder (srcSp 2 3 2 17)
                               (srcSp 2 3 2 3, Var "x")
                               (SrcTyMutable (srcSp 2 7 2 17)
                                  (SrcTyInt $ srcSp 2 15 2 17)))
                            Nothing)
                     , DeclS (srcSp 3 3 3 14)
                         (Decl (srcSp 3 3 3 13)
                            (VarBinder (srcSp 3 3 3 9)
                               (srcSp 3 3 3 3, Var "y")
                               (SrcTyInt $ srcSp 3 7 3 9))
                            (Just $ Init (srcSp 3 11 3 13)
                               (srcSp 3 11 3 11, InitEqual)
                               (LitE (srcSp 3 13 3 13, IntLit 1))))
                     , DeclS (srcSp 4 3 4 23)
                         (Decl (srcSp 4 3 4 22)
                            (VarBinder (srcSp 4 3 4 17)
                               (srcSp 4 3 4 3, Var "z")
                               (SrcTyMutable (srcSp 4 7 4 17)
                                  (SrcTyInt $ srcSp 4 15 4 17)))
                            (Just $ Init (srcSp 4 19 4 22)
                               (srcSp 4 19 4 20, InitMut)
                               (LitE (srcSp 4 22 4 22, IntLit 1))))
                     , AssignS (srcSp 5 3 5 9)
                         (srcSp 5 5 5 6, AssignMut)
                         (srcSp 5 3 5 3, Var "x")
                         (LitE (srcSp 5 8 5 8, IntLit 2))]
                     False]
      in successCase baseName ast

    it "parses control flow statements" $
      let baseName = "ControlStatements"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 8 3)
                  []
                  [FunDef (srcSp 1 1 8 3)
                     (srcSp 1 5 1 7, FunName "fun")
                     (FunType (srcSp 1 11 1 14)
                        []
                        (SrcTyUnit $ srcSp 1 11 1 14))
                     [ WhenS (srcSp 2 3 5 6)
                         (VarE (srcSp 2 8 2 8) (Var "b"))
                         [ExprS (srcSp 3 5 3 9)
                            (LitE (srcSp 3 5 3 8, UnitLit))]
                         []
                     , WhileS (srcSp 6 3 7 6)
                         (LitE (srcSp 6 9 6 12, BoolLit True))
                         []]
                     False]
      in successCase baseName ast

    it "parses expressions" $
      let baseName = "Expressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 4 3)
                  []
                  [FunDef (srcSp 1 1 4 3)
                     (srcSp 1 5 1 7, FunName "fun")
                     (FunType (srcSp 1 11 1 14)
                        []
                        (SrcTyUnit $ srcSp 1 11 1 14))
                     [ ExprS (srcSp 2 3 2 33)
                         (BinOpE (srcSp 2 3 2 32)
                            (srcSp 2 14 2 14, App)
                            (BinOpE (srcSp 2 3 2 13)
                               (srcSp 2 9 2 9, App)
                               (BinOpE (srcSp 2 3 2 8)
                                  (srcSp 2 4 2 4, App)
                                  (VarE (srcSp 2 3 2 3) (Var "f"))
                                  (LitE (srcSp 2 5 2 8, UnitLit)))
                               (LitE (srcSp 2 10 2 13, BoolLit True)))
                            (ParenE (srcSp 2 15 2 32)
                               (BinOpE (srcSp 2 16 2 31)
                                  (srcSp 2 27 2 27, App)
                                  (BinOpE (srcSp 2 16 2 26)
                                     (srcSp 2 25 2 25, App)
                                     (BinOpE (srcSp 2 16 2 24)
                                        (srcSp 2 17 2 17, App)
                                        (VarE (srcSp 2 16 2 16) (Var "h"))
                                        (LitE (srcSp 2 18 2 24, StringLit "Hello")))
                                     (LitE (srcSp 2 26 2 26, IntLit 1)))
                                  (LitE (srcSp 2 28 2 31, FloatLit 0.01 "0.01")))))
                     , ExprS (srcSp 3 3 3 15)
                         (JustE (srcSp 3 3 3 14)
                            (LitE (srcSp 3 8 3 14, NothingLit)))]
                     False]
      in successCase baseName ast

    it "parses class expressions" $
      let baseName = "ClassExpressions"
          fileName = mkFileName baseName
          srcSp = mkSrcSpan fileName
          ast = Program (srcSp 1 1 5 3)
                  []
                  [FunDef (srcSp 1 1 5 3)
                     (srcSp 1 5 1 7, FunName "fun")
                     (FunType (srcSp 1 11 1 14)
                        []
                        (SrcTyUnit $ srcSp 1 11 1 14))
                     [ ExprS (srcSp 2 3 2 20)
                         (BinOpE (srcSp 2 3 2 19)
                            (srcSp 2 18 2 18, App)
                            (MemberAccessE (srcSp 2 3 2 17)
                               (ParenE (srcSp 2 3 2 12)
                                  (BinOpE (srcSp 2 4 2 11)
                                     (srcSp 2 10 2 10, App)
                                     (MemberAccessE (srcSp 2 4 2 9)
                                        (VarE (srcSp 2 4 2 4) (Var "x"))
                                        (srcSp 2 6 2 9, FunName "fun1"))
                                     (LitE (srcSp 2 11 2 11, IntLit 1))))
                               (srcSp 2 14 2 17, FunName "fun2"))
                            (LitE (srcSp 2 19 2 19, IntLit 2)))
                     , ExprS (srcSp 3 3 3 17)
                         (BinOpE (srcSp 3 3 3 16)
                            (srcSp 3 15 3 15, App)
                            (MemberAccessMaybeE (srcSp 3 3 3 14)
                               (VarE (srcSp 3 3 3 3) (Var "x"))
                               (srcSp 3 7 3 14, FunName "maybeFun"))
                            (LitE (srcSp 3 16 3 16, IntLit 1)))
                     , ExprS (srcSp 4 3 4 15)
                         (BinOpE (srcSp 4 3 4 14)
                            (srcSp 4 13 4 13, App)
                            (ClassAccessE (srcSp 4 3 4 12)
                               (srcSp 4 3 4 8, ClassName "Object")
                               (srcSp 4 10 4 12, FunName "new"))
                            (LitE (srcSp 4 14 4 14, IntLit 1)))]
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

      it "given an empty function definition" $
        failureCase "EmptyFunDef"

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

-- | Takes a file base name and an expected AST and performs a test (by
-- comparing showed ASTs).
successCase :: String -> SrcProgram -> IO ()
successCase baseName result = do
  input <- successRead baseName
  case parseOOLang (mkFileName baseName) input of
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
  let Left err = parseOOLang (mkFileName baseName) input
  prPrint err `shouldBe` errMsg

-- | Takes a file base name and reads a source program and expected error
-- message (from .err file).
failureRead :: String -> IO (String, String)
failureRead baseName =
  liftM2 (,) (readFile (failureDir </> mkFileName baseName))
             (dropNewLine <$> readFile (failureDir </> baseName <.> "err"))

