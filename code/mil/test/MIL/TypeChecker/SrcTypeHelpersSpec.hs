-- | Type checker source type helpers tests.
module MIL.TypeChecker.SrcTypeHelpersSpec (main, spec) where

import Test.Hspec
import Control.Applicative
import qualified Data.Set as Set

import MIL.AST
import MIL.BuiltIn
import MIL.TypeChecker.SrcTypeHelpers
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TypeEnv
import MIL.TypeChecker.TcError

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "srcTypeToType" $ do
    it "transforms a built-in type correctly" $
      let srcType = SrcTyTypeCon (TypeName "Unit")
          expT = unitType
      in successCase srcType expT

    it "transforms a type constructor correctly" $
      let srcType = SrcTyTypeCon (TypeName "T")
          expT = mkSimpleType "T"
      in successCaseWithSetup (addTypeM (TypeName "T") StarK) srcType expT

    it "can not transform an undefined type constructor" $
      let srcType = SrcTyTypeCon (TypeName "T")
          tcError = TypeNotDefined (TypeName "T")
      in failureCase srcType tcError

    it "transforms a function type correctly (with built-in types)" $
      let srcType = SrcTyArrow (SrcTyTypeCon $ TypeName "Bool") (SrcTyTypeCon $ TypeName "Int")
          expT = TyArrow boolType intType
      in successCase srcType expT

    it "transforms a forall type correctly (simple type var)" $
      let srcType = SrcTyForAll (TypeVar "A") (SrcTyTypeCon $ TypeName "A")
          expT = TyForAll (TypeVar "A") (mkTypeVar "A")
      in successCase srcType expT

    it "transforms a nested forall type correctly" $
      let srcType = SrcTyForAll (TypeVar "A")
                      (SrcTyForAll (TypeVar "B") (SrcTyArrow
                                                    (SrcTyTypeCon $ TypeName "A")
                                                    (SrcTyTypeCon $ TypeName "B")))
          expT = TyForAll (TypeVar "A")
                   (TyForAll (TypeVar "B") (TyArrow
                                              (mkTypeVar "A")
                                              (mkTypeVar "B")))
      in successCase srcType expT

    it "doesn't allow a type variable in forall to shadow a type" $
      let srcType = SrcTyForAll (TypeVar "T") (SrcTyTypeCon $ TypeName "T")
          tcError = TypeVarShadowsType (TypeVar "T")
      in failureCaseWithSetup (addTypeM (TypeName "T") StarK) srcType tcError

    it "doesn't allow a type variable in forall to shadow another type variable" $
      let srcType = SrcTyForAll (TypeVar "A") (SrcTyForAll (TypeVar "A") (SrcTyTypeCon $ TypeName "Unit"))
          tcError = TypeVarShadowsTypeVar (TypeVar "A")
      in failureCase srcType tcError

    it "transforms forall types with the same type variable name on different sides of function arrow" $
      let srcType = SrcTyArrow (SrcTyForAll (TypeVar "A") (SrcTyTypeCon $ TypeName "A"))
                               (SrcTyForAll (TypeVar "A") (SrcTyTypeCon $ TypeName "A"))
          expT = TyArrow (TyForAll (TypeVar "A") (mkTypeVar "A"))
                         (TyForAll (TypeVar "A") (mkTypeVar "A"))
      in successCase srcType expT

    it "transforms type application correctly" $
      let srcType = SrcTyApp (SrcTyApp (SrcTyTypeCon $ TypeName "Pair") (SrcTyTypeCon $ TypeName "Unit"))
                             (SrcTyTypeCon $ TypeName "Bool")
          expT = TyApp (TyApp (mkSimpleType "Pair") unitType) boolType
      in successCaseWithSetup (addTypeM (TypeName "Pair") (mkKind 2)) srcType expT

    it "doesn't allow function type to be the left component of type application" $
      let srcType = SrcTyApp (SrcTyArrow (SrcTyTypeCon $ TypeName "Unit") (SrcTyTypeCon $ TypeName "Unit"))
                             (SrcTyTypeCon $ TypeName "Bool")
          tcError = IllFormedSrcType srcType
      in failureCase srcType tcError

    it "doesn't allow tuple type to be the left component of type application" $
      let srcType = SrcTyApp (SrcTyTuple [SrcTyTypeCon (TypeName "Unit"), SrcTyTypeCon (TypeName "Unit")])
                             (SrcTyTypeCon $ TypeName "Bool")
          tcError = IllFormedSrcType srcType
      in failureCase srcType tcError

    it "performs a kind checking (too many arguments)" $
      let srcType = SrcTyApp (SrcTyApp (SrcTyApp (SrcTyTypeCon $ TypeName "Pair")
                                                 (SrcTyTypeCon $ TypeName "Unit"))
                                       (SrcTyTypeCon $ TypeName "Bool"))
                             (SrcTyTypeCon $ TypeName "Int")
          tcError = TypeConIncorrectApp (TypeName "Pair") (mkKind 2) (mkKind 3)
      in failureCaseWithSetup (addTypeM (TypeName "Pair") (mkKind 2)) srcType tcError

    it "performs a kind checking (too few arguments)" $
      let srcType = SrcTyApp (SrcTyTypeCon $ TypeName "Pair") (SrcTyTypeCon $ TypeName "Unit")
          tcError = TypeConIncorrectApp (TypeName "Pair") (mkKind 2) (mkKind 1)
      in failureCaseWithSetup (addTypeM (TypeName "Pair") (mkKind 2)) srcType tcError

    it "only allows types of kind '*' as components of function arrow" $
      let srcType = SrcTyArrow (SrcTyTypeCon $ TypeName "T") (SrcTyTypeCon $ TypeName "Unit")
          tcError = TypeConIncorrectApp (TypeName "T") (mkKind 1) StarK
      in failureCaseWithSetup (addTypeM (TypeName "T") (mkKind 1)) srcType tcError

    it "only allows type variables of kind '*'" $
      let srcType = SrcTyForAll (TypeVar "A")
                      (SrcTyApp (SrcTyTypeCon $ TypeName "A") (SrcTyTypeCon $ TypeName "Unit"))
          tcError = TypeVarApp (TypeVar "A")
      in failureCase srcType tcError

    it "transforms a tuple type correctly (with built-in types)" $
      let srcType = SrcTyTuple [SrcTyTypeCon (TypeName "Float"), SrcTyTypeCon (TypeName "Char")]
          expT = TyTuple [floatType, charType]
      in successCase srcType expT

    it "only allows types of kind '*' as tuple elements" $
      let srcType = SrcTyTuple [SrcTyTypeCon (TypeName "Unit"), SrcTyTypeCon (TypeName "T")]
          tcError = TypeConIncorrectApp (TypeName "T") (mkKind 1) StarK
      in failureCaseWithSetup (addTypeM (TypeName "T") (mkKind 1)) srcType tcError

    it "transforms a type application with single built-in monad correctly" $
      let srcType = SrcTyApp (SrcTyTypeCon $ TypeName "IO") (SrcTyTypeCon $ TypeName "Unit")
          expT = TyApp (TyMonad (MTyMonad $ SinMonad IO)) unitType
      in successCase srcType expT

    it "transforms a type application with parameterised built-in monad correctly" $
      let srcType = SrcTyApp (SrcTyApp (SrcTyTypeCon $ TypeName "Error") (SrcTyTypeCon $ TypeName "Unit"))
                             (SrcTyTypeCon $ TypeName "Int")
          expT = TyApp (TyMonad (MTyMonad $ SinMonadApp (SinMonad Error) unitType)) intType
      in successCase srcType expT

    it "transforms a type application with monad cons correctly" $
      let srcType = SrcTyApp (SrcTyMonadCons (SrcTyTypeCon $ TypeName "State") (SrcTyTypeCon $ TypeName "IO"))
                             (SrcTyTypeCon $ TypeName "Int")
          expT = TyApp (TyMonad $ MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad IO)) intType
      in successCase srcType expT

    it "transforms a type application with monad cons correctly (with parameterised monad)" $
      let srcType = SrcTyApp (SrcTyMonadCons (SrcTyApp (SrcTyTypeCon $ TypeName "Error") (SrcTyTypeCon $ TypeName "Unit"))
                                             (SrcTyTypeCon $ TypeName "State"))
                             (SrcTyTypeCon $ TypeName "Unit")
          expT = TyApp (TyMonad (MTyMonadCons (SinMonadApp (SinMonad Error) unitType) (MTyMonad $ SinMonad State)))
                       unitType
      in successCase srcType expT

    it "only allows monadic types in monad cons" $
      let srcType = SrcTyApp (SrcTyMonadCons (SrcTyTypeCon $ TypeName "T") (SrcTyTypeCon $ TypeName "State"))
                             (SrcTyTypeCon $ TypeName "Int")
          tcError = NotMonadicType (mkSimpleType "T")
      in failureCaseWithSetup (addTypeM (TypeName "T") StarK) srcType tcError

    it "performs a kind checking on built-in monads" $
      let srcType = SrcTyTypeCon $ TypeName "Id"
          tcError = TypeConIncorrectApp (TypeName "Id") (mkKind 1) StarK
      in failureCase srcType tcError

    it "performs a kind checking on monad cons" $
      let srcType = SrcTyMonadCons (SrcTyTypeCon $ TypeName "State") (SrcTyTypeCon $ TypeName "Id")
          tcError = SrcTypeIncorrectKind srcType (mkKind 1) StarK
      in failureCase srcType tcError

    it "performs a kind checking on built-in monads (parameterised monad)" $
      let srcType = SrcTyApp (SrcTyTypeCon $ TypeName "Error") (SrcTyTypeCon $ TypeName "Unit")
          tcError = TypeConIncorrectApp (TypeName "Error") (mkKind 2) (mkKind 1)
      in failureCase srcType tcError

    it "performs a kind checking on built-in monads (in monad cons)" $
      let srcType = SrcTyApp (SrcTyMonadCons (SrcTyTypeCon $ TypeName "Error") (SrcTyTypeCon $ TypeName "IO"))
                             (SrcTyTypeCon $ TypeName "Bool")
          tcError = TypeConIncorrectApp (TypeName "Error") (mkKind 2) (mkKind 1)
      in failureCase srcType tcError

    it "transforms a nested monad cons correctly" $
      let srcType = SrcTyApp (SrcTyMonadCons (SrcTyTypeCon $ TypeName "NonTerm")
                                             (SrcTyMonadCons  (SrcTyTypeCon $ TypeName "State") (SrcTyTypeCon $ TypeName "Id")))
                             (SrcTyTypeCon $ TypeName "Unit")
          expT = TyApp (TyMonad $ MTyMonadCons (SinMonad NonTerm)
                                               (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad Id)))
                       unitType
      in successCase srcType expT

    it "does not allow non-monadic type in monad cons (on the left)" $
      let srcType = SrcTyApp (SrcTyMonadCons (SrcTyTuple []) (SrcTyTypeCon $ TypeName "Id"))
                             (SrcTyTypeCon $ TypeName "Int")
          tcError = NotMonadicSrcType (SrcTyTuple [])
      in failureCase srcType tcError

    it "does not allow monad cons to contain another monad cons on the left" $
      let srcType = SrcTyApp mt (SrcTyTypeCon $ TypeName "Unit")
          mt = SrcTyMonadCons (SrcTyMonadCons (SrcTyTypeCon $ TypeName "NonTerm") (SrcTyTypeCon $ TypeName "State"))
                              (SrcTyTypeCon $ TypeName "Id")
          tcError = MonadConsOnTheLeft mt
      in failureCase srcType tcError

  describe "srcTypeToTypeWithTypeVars" $ do
    it "transforms a type variable correctly" $
      let srcType = SrcTyTypeCon (TypeName "A")
          expT = mkTypeVar "A" in
      fst <$> runTypeCheckM (srcTypeToTypeWithTypeVars (Set.fromList [TypeVar "A"]) srcType) initTypeEnv
        `shouldBe` Right expT

  describe "srcTypeToTypeWithTypeVarsOfKind" $ do
    it "performs kind checking for function type" $
      let srcType = SrcTyArrow (SrcTyTypeCon $ TypeName "Unit") (SrcTyTypeCon $ TypeName "Bool")
          tcError = SrcTypeIncorrectKind srcType StarK (mkKind 1) in
      fst <$> runTypeCheckM (srcTypeToTypeWithTypeVarsOfKind Set.empty (mkKind 1) srcType) initTypeEnv
        `shouldBe` Left tcError

    it "performs kind checking for forall type" $
      let srcType = SrcTyForAll (TypeVar "A") (SrcTyTypeCon $ TypeName "Id")
          t = TyForAll (TypeVar "A") (TyMonad $ MTyMonad (SinMonad Id)) in
      fst <$> runTypeCheckM (srcTypeToTypeWithTypeVarsOfKind Set.empty (mkKind 1) srcType) initTypeEnv
        `shouldBe` Right t

    it "performs kind checking for tuple type" $
      let srcType = SrcTyTuple [SrcTyTypeCon (TypeName "Unit")]
          tcError = SrcTypeIncorrectKind srcType StarK (mkKind 1) in
      fst <$> runTypeCheckM (srcTypeToTypeWithTypeVarsOfKind Set.empty (mkKind 1) srcType) initTypeEnv
        `shouldBe` Left tcError

-- * Infrastructure

successCase :: SrcType -> Type -> IO ()
successCase = successCaseWithSetup (return ())

successCaseWithSetup :: TypeCheckM () -> SrcType -> Type -> IO ()
successCaseWithSetup setup srcType expT =
  fst <$> runTypeCheckM (setup >> srcTypeToType srcType) initTypeEnv
    `shouldBe` Right expT

failureCase :: SrcType -> TcError -> IO ()
failureCase = failureCaseWithSetup (return ())

failureCaseWithSetup :: TypeCheckM () -> SrcType -> TcError -> IO ()
failureCaseWithSetup setup srcType tcError =
  fst <$> runTypeCheckM (setup >> srcTypeToType srcType) initTypeEnv
    `shouldBe` Left tcError

