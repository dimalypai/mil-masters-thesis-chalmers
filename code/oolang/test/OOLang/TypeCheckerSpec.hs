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
    it "accepts type correct program with functions (mostly)" $
      let baseName = "Functions"
      in successCase baseName

    it "accepts type correct program with classes (mostly)" $
      let baseName = "Classes"
      in successCase baseName

    it "accepts type correct program (regression test for inheritance cycle detection)" $
      let baseName = "NoInheritanceCycleRegression"
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

      it "given a function with Pure return type and impure statement (dereferencing)" $
        failureCase "PureFunImpureStmtDeref"

      it "given a function with Pure return type and impure statement (impure return type and application)" $
        failureCase "PureFunImpureStmtRetTypeFullApp"

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

      it "given a declaration with incorrect init type (Ref)" $
        failureCase "DeclInitIncorrectTypeRef"

      it "given a declaration of immutable variable with mutable init operator" $
        failureCase "ImmutableDeclMutInitOp"

      it "given a declaration of mutable variable with immutable init operator" $
        failureCase "MutableDeclEqualInitOp"

      it "given a declaration of reference variable with mutable init operator" $
        failureCase "RefDeclMutInitOp"

      it "given a declaration as a last statement of integer returning function" $
        failureCase "DeclIntFunReturn"

      it "given an impure declaration inside a Pure function" $
        failureCase "PureFunImpureDecl"

      it "given an impure declaration inside a Pure function (Ref)" $
        failureCase "PureFunImpureDeclRef"

      it "given a mutable variable assignment to an immutable variable" $
        failureCase "AssignMutImmutableVar"

      it "given a reference variable assignment to a mutable variable" $
        failureCase "AssignRefMutableVar"

      it "given a mutable variable assignment with incorrect type" $
        failureCase "AssignMutIncorrectType"

      it "given an impure mutable variable assignment inside a Pure function" $
        failureCase "PureFunImpureAssignMut"

      it "given an impure reference variable assignment inside a Pure function" $
        failureCase "PureFunImpureAssignRef"

      it "given a mutable variable assignment as a last statement of integer returning function" $
        failureCase "AssignIntFunReturn"

      it "given an uninitialised variable of non-Maybe type" $
        failureCase "NonMaybeVarNotInit"

      it "given a nothing literal annotated with non-Maybe type" $
        failureCase "NothingNotMaybe"

      it "given a class field with the name `self`" $
        failureCase "ClassFieldSelf"

      it "given a class method with the name `self`" $
        failureCase "ClassMethodSelf"

      it "given a class field with the name `super`" $
        failureCase "ClassFieldSuper"

      it "given a class method with the name `super`" $
        failureCase "ClassMethodSuper"

      it "given a class field with the name `new`" $
        failureCase "ClassFieldNew"

      it "given a class method with the name `new`" $
        failureCase "ClassMethodNew"

      it "given a class field names duplication in the same class" $
        failureCase "ClassFieldNamesDup"

      it "given a class field names duplication with the super class" $
        failureCase "ClassFieldNamesDupSuper"

      it "given a class method names duplication (overloading) in the same class" $
        failureCase "ClassMethodNamesDup"

      it "given a class method names duplication (overloading) with the super class" $
        failureCase "ClassMethodNamesDupSuper"

      it "given a class field and a method with the same name (field is first)" $
        failureCase "ClassFieldMethodSameNameFieldFirst"

      it "given a class field and a method with the same name (method is first)" $
        failureCase "ClassFieldMethodSameNameMethodFirst"

      it "given a class field and a method with the same name (field is in the super class)" $
        failureCase "ClassFieldMethodSameNameFieldSuper"

      it "given a class field and a method with the same name (method is in the super class)" $
        failureCase "ClassFieldMethodSameNameMethodSuper"

      it "given a class field that uses undefined type" $
        failureCase "ClassFieldUndefinedType"

      it "given an impure class field initialiser" $
        failureCase "ClassFieldImpureInit"

      it "given a class field initialiser which forward references another field (directly)" $
        failureCase "ClassFieldForwardDirectRef"

      it "given a class field with incorrect initialiser type" $
        failureCase "ClassFieldInitIncorrectType"

      it "given a class field with incorrect initialiser type (`super`)" $
        failureCase "ClassFieldInitIncorrectTypeSuper"

      it "given an uninitialised class field of non-Maybe type" $
        failureCase "ClassFieldNonMaybeNotInit"

      it "given an immutable class field with mutable init operator" $
        failureCase "ImmutableClassFieldMutInitOp"

      it "given a mutable class field with immutable init operator" $
        failureCase "MutableClassFieldEqualInitOp"

      it "given a reference class field with mutable init operator" $
        failureCase "RefClassFieldMutInitOp"

      it "given an assignment to `self`" $
        failureCase "ClassSelfAssign"

      it "given an assignment to `super`" $
        failureCase "ClassSuperAssign"

      it "given a `super` reference from a base class" $
        failureCase "BaseSuperRef"

      it "given a member access to an undefined class member" $
        failureCase "MemberAccessUndefined"

      it "given a member access to an undefined super class member" $
        failureCase "SuperMemberAccessUndefined"

      it "given a forward class field access from field initialiser" $
        failureCase "FieldInitForwardFieldAccess"

      it "given a class method access from field initialiser" $
        failureCase "FieldInitMethodAccess"

      it "given a member access with Maybe type" $
        failureCase "MemberAccessMaybeType"

      it "given a member access with non-class type" $
        failureCase "MemberAccessNonClass"

      it "given a class field reference without `self`" $
        failureCase "FieldReferenceNoSelf"

      it "given a class method reference without `self`" $
        failureCase "MethodReferenceNoSelf"

      it "given an impure method call from pure method" $
        failureCase "PureMethodImpureMethodCall"

      it "given a member access through `self` in a function after a class definition" $
        failureCase "SelfMemberAccessFunAfterClass"

      it "given a member access through `super` in a function after a class definition" $
        failureCase "SuperMemberAccessFunAfterClass"

      it "given a class access not to `new`" $
        failureCase "ClassAccessNotNew"

      it "given a member access to `new`" $
        failureCase "MemberAccessNew"

      it "given an assignment statement with incorrect left-hand side (literal)" $
        failureCase "AssignLiteralLeftHandSide"

      it "given an assignment to global function" $
        failureCase "AssignGlobalFun"

      it "given an assignment to a class method" $
        failureCase "AssignClassMethod"

      it "given a pure method with assignment to a class field" $
        failureCase "AssignFieldPure"

      it "given a field access outside of the class" $
        failureCase "FieldAccessOutside"

      it "given a field access inside of the class, but not via `self` or `super`" $
        failureCase "FieldAccessInsideNotSelfSuper"

      it "given a declaration with a class not from hierarchy (subtyping)" $
        failureCase "DeclClassNotSubClass"

      it "given a declaration with a class not from hierarchy (subtyping, Ref)" $
        failureCase "DeclClassNotSubClassRef"

      it "given a nested reference creation" $
        failureCase "NestedNewRef"

      it "given a dereference operation with non-Ref type" $
        failureCase "DerefNonRef"

      it "given a reference assignment with incorrect type" $
        failureCase "AssignRefIncorrectType"

      it "given a reference assignment with incorrect type (nested ref)" $
        failureCase "AssignRefIncorrectTypeRef"

      it "given a usage of not-dereferenced variable (member access)" $
        failureCase "NotDerefVarUsage"

      it "given a Maybe member access with non-Maybe type" $
        failureCase "MemberAccessMaybeWithNonMaybeType"

      it "given a Maybe member access with non-class type" $
        failureCase "MemberAccessMaybeNonClass"

      it "given a Maybe member access to an undefined class method" $
        failureCase "MemberAccessMaybeUndefined"

      it "given a Maybe member access to a class field" $
        failureCase "MemberAccessMaybeField"

      it "given a variable redeclaration inside try block" $
        failureCase "TryBlockVarRedeclaration"

      it "given an empty catch block with non-Unit try block" $
        failureCase "EmptyCatchNonUnitTry"

      it "given an incorrect return type in catch block" $
        failureCase "CatchIncorrectType"

      it "given a catch block usage of a variable defined inside try block" $
        failureCase "TryVarUsageCatch"

      it "given an outside usage of a variable defined inside try block" $
        failureCase "TryVarUsageOutside"

      it "given an arithmetic operation with the left operand of non-arithmetic type" $
        failureCase "ArithOpLeftNonArithType"

      it "given an arithmetic operation with the right operand of non-arithmetic type" $
        failureCase "ArithOpRightNonArithType"

      it "given an arithmetic operation with different types of operands" $
        failureCase "ArithOpDiffTypes"

      it "given a comparison operation with the left operand of non-comparable type" $
        failureCase "CmpOpLeftNonCmpType"

      it "given a comparison operation with the right operand of non-comparable type" $
        failureCase "CmpOpRightNonCmpType"

      it "given a comparison operation with different types of operands" $
        failureCase "CmpOpDiffTypes"

      it "given a nothing coalesce operator with incompatible types" $
        failureCase "NothingCoalesceIncompatibleTypes"

      it "given a nothing coalesce operator with non-Maybe type" $
        failureCase "NothingCoalesceNonMaybe"

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

