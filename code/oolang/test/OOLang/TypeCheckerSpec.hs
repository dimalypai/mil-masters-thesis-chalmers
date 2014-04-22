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
    it "accepts type correct program" $
      let baseName = "Program"
      in successCase baseName

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

      it "given a function type which uses undefined type" $
        failureCase "FunTypeNotDefined"

      it "given a function with parameter names duplication" $
        failureCase "FunParamsDup"

      it "given a function with parameter name which shadows an existing name" $
        failureCase "FunParamShadows"

      it "given a function with incorrect type of the last expression (statement)" $
        failureCase "FunIncorrectReturnType"

      it "given a function with usage of unbound variable" $
        failureCase "VarNotBound"

      it "given a function with Pure return type and impure statement" $
        failureCase "PureFunImpureStmt"

      it "given a function with Pure return type and impure statement (local function)" $
        failureCase "PureFunImpureStmtLocal"

      it "given an application with left-hand side which is not a function" $
        failureCase "AppNotFunction"

      it "given an application with left-hand side which has not a function type" $
        failureCase "AppNotFunctionType"

      it "given an application with incorrect argument type" $
        failureCase "AppIncorrectArgType"

      it "given an application with impure argument" $
        failureCase "AppImpureArg"

      it "given a function definition with Pure parameter" $
        failureCase "FunDefPureParam"

      it "given a function definition with Mutable parameter" $
        failureCase "FunDefMutableParam"

      it "given a function definition with Mutable return type" $
        failureCase "FunDefMutableReturnType"

      it "given an arrow type with Pure parameter" $
        failureCase "TyArrowPureParam"

      it "given an arrow type with Mutable parameter" $
        failureCase "TyArrowMutableParam"

      it "given an arrow type with Mutable return type" $
        failureCase "TyArrowMutableReturnType"

      it "given a Pure type inside another type" $
        failureCase "PureNestedInType"

      it "given a Maybe type with nested Ref type" $
        failureCase "MaybeRefNestedType"

      it "given a Maybe type with nested Mutable type" $
        failureCase "MaybeMutableNestedType"

      it "given a Mutable type with nested Ref type" $
        failureCase "MutableRefNestedType"

      it "given a Mutable type with nested Mutable type" $
        failureCase "MutableMutableNestedType"

      it "given a Ref type with nested Ref type" $
        failureCase "RefRefNestedType"

      it "given a Ref type with nested Mutable type" $
        failureCase "RefMutableNestedType"

      it "given a declaration that shadows a global function" $
        failureCase "DeclShadowsFun"

      it "given a declaration that shadows a function parameter" $
        failureCase "DeclShadowsFunParam"

      it "given a declaration that shadows a local variable" $
        failureCase "DeclShadowsLocalVar"

      it "given a declaration that uses undefined type" $
        failureCase "DeclUndefinedType"

      it "given a declaration with incorrect init type" $
        failureCase "DeclInitIncorrectType"

      it "given a declaration of immutable variable with mutable init operator" $
        failureCase "ImmutableDeclMutInitOp"

      it "given a declaration of mutable variable with immutable init operator" $
        failureCase "MutableDeclEqualInitOp"

      it "given a declaration as a last statement of integer returning function" $
        failureCase "DeclIntFunReturn"

      it "given an impure declaration inside a Pure function" $
        failureCase "PureFunImpureDecl"

      it "given a mutable variable assignment to an immutable variable" $
        failureCase "AssignMutImmutableVar"

      it "given a mutable variable assignment with incorrect type" $
        failureCase "AssignMutIncorrectType"

      it "given an impure mutable variable assignment inside a Pure function" $
        failureCase "PureFunImpureAssignMut"

      it "given a mutable variable assignment as a last statement of integer returning function" $
        failureCase "AssignIntFunReturn"

      -- TODO: add tests for references

-- Infrastructure

-- | Takes a file base name and performs a test.
successCase :: String -> IO ()
successCase baseName = do
  input <- successRead baseName
  let Right srcProgram = parseOOLang (mkFileName baseName) input
  case typeCheck srcProgram of
    Right (tyProgram, _) -> True `shouldBe` True
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

