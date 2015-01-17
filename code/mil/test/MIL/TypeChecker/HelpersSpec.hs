-- | Type/lint checker helpers tests.
module MIL.TypeChecker.HelpersSpec (main, spec) where

import Test.Hspec
import Control.Applicative
import qualified Data.Set as Set

import MIL.AST
import MIL.BuiltIn
import MIL.TypeChecker.Helpers
import MIL.TypeChecker.TypeCheckM
import MIL.TypeChecker.TypeEnv
import MIL.TypeChecker.TcError

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "checkType" $ do
    it "checks a built-in type" $
      let t = unitType
      in successCase t

    it "checks a type constructor" $
      let t = TyTypeCon (TypeName "T")
      in successCaseWithSetup (addTypeM (TypeName "T") StarK) t

    it "gives an error when type constructor is undefined" $
      let t = TyTypeCon (TypeName "T")
          tcError = TypeNotDefined (TypeName "T")
      in failureCase t tcError

    it "gives an error when type variable is not in scope" $
      let t = TyVar $ TypeVar "A"
          tcError = TypeVarNotInScope $ TypeVar "A"
      in failureCase t tcError

    it "checks a function type (with built-in types)" $
      let t = TyArrow boolType intType
      in successCase t

    it "checks a forall type (simple type var)" $
      let t = TyForAll (TypeVar "A") (mkTypeVar "A")
      in successCase t

    it "checks a nested forall type" $
      let t = TyForAll (TypeVar "A")
                (TyForAll (TypeVar "B")
                   (TyArrow (mkTypeVar "A") (mkTypeVar "B")))
      in successCase t

    it "doesn't allow a type variable in forall to shadow a type" $
      let t = TyForAll (TypeVar "T") (mkTypeVar "T")
          tcError = TypeVarShadowsType (TypeVar "T")
      in failureCaseWithSetup (addTypeM (TypeName "T") StarK) t tcError

    it "doesn't allow a type variable in forall to shadow another type variable" $
      let t = TyForAll (TypeVar "A") (TyForAll (TypeVar "A") unitType)
          tcError = TypeVarShadowsTypeVar (TypeVar "A")
      in failureCase t tcError

    it "checks forall types with the same type variable name on different sides of function arrow" $
      let t = TyArrow (TyForAll (TypeVar "A") (mkTypeVar "A"))
                      (TyForAll (TypeVar "A") (mkTypeVar "A"))
      in successCase t

    it "checks type application" $
      let t = TyApp (TyApp (mkSimpleType "Pair") unitType) boolType
      in successCaseWithSetup (addTypeM (TypeName "Pair") (mkKind 2)) t

    it "doesn't allow function type to be the left component of type application" $
      let t = TyApp (TyArrow unitType unitType) boolType
          tcError = IllFormedType t
      in failureCase t tcError

    it "doesn't allow tuple type to be the left component of type application" $
      let t = TyApp (TyTuple [unitType, unitType]) boolType
          tcError = IllFormedType t
      in failureCase t tcError

    it "performs a kind checking (too many arguments)" $
      let t = TyApp (TyApp (TyApp (mkSimpleType "Pair")
                                  unitType)
                           boolType)
                    intType
          tcError = TypeConIncorrectApp (TypeName "Pair") (mkKind 2) (mkKind 3)
      in failureCaseWithSetup (addTypeM (TypeName "Pair") (mkKind 2)) t tcError

    it "performs a kind checking (too few arguments)" $
      let t = TyApp (mkSimpleType "Pair") unitType
          tcError = TypeConIncorrectApp (TypeName "Pair") (mkKind 2) (mkKind 1)
      in failureCaseWithSetup (addTypeM (TypeName "Pair") (mkKind 2)) t tcError

    it "only allows types of kind '*' as components of function arrow" $
      let t = TyArrow (mkSimpleType "T") unitType
          tcError = TypeConIncorrectApp (TypeName "T") (mkKind 1) StarK
      in failureCaseWithSetup (addTypeM (TypeName "T") (mkKind 1)) t tcError

    it "only allows type variables of kind '*'" $
      let t = TyForAll (TypeVar "A") (TyApp (mkTypeVar "A") unitType)
          tcError = TypeVarApp (TypeVar "A")
      in failureCase t tcError

    it "checks a tuple type (with built-in types)" $
      let t = TyTuple [floatType, charType]
      in successCase t

    it "only allows types of kind '*' as tuple elements" $
      let t = TyTuple [unitType, mkSimpleType "T"]
          tcError = TypeConIncorrectApp (TypeName "T") (mkKind 1) StarK
      in failureCaseWithSetup (addTypeM (TypeName "T") (mkKind 1)) t tcError

    it "checks a type application with single built-in monad" $
      let t = TyApp (TyMonad (MTyMonad $ SinMonad IO)) unitType
      in successCase t

    it "checks a type application with parameterised built-in monad" $
      let t = TyApp (TyMonad (MTyMonad $ SinMonadApp (SinMonad Error) unitType)) intType
      in successCase t

    it "checks a type application with monad cons" $
      let t = TyApp (TyMonad $ MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad IO)) intType
      in successCase t

    it "checks a type application with monad cons (with parameterised monad)" $
      let t = TyApp (TyMonad $ MTyMonadCons (SinMonadApp (SinMonad Error) unitType) (MTyMonad $ SinMonad State)) unitType
      in successCase t

    it "performs a kind checking on built-in monads" $
      let t = TyMonad $ MTyMonad (SinMonad Id)
          tcError = TypeIncorrectKind t (mkKind 1) StarK
      in failureCase t tcError

    it "performs a kind checking on built-in monads (parameterised monad)" $
      let t = TyApp (TyMonad $ MTyMonad (SinMonad Error)) unitType
          tcError = TypeConIncorrectApp (TypeName "Error") (mkKind 2) (mkKind 1)
      in failureCase t tcError

    it "performs a kind checking on built-in monads (in monad cons)" $
      let t = TyApp (TyMonad $ MTyMonadCons (SinMonad Error) (MTyMonad $ SinMonad State))
                    boolType
          tcError = TypeConIncorrectApp (TypeName "Error") (mkKind 2) (mkKind 1)
      in failureCase t tcError

    it "checks a nested monad cons" $
      let t = TyApp (TyMonad $ MTyMonadCons (SinMonad NonTerm)
                                 (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad Id)))
                    unitType
      in successCase t

  describe "checkTypeWithTypeVars" $ do
    it "checks a type variable" $
      let t = mkTypeVar "A" in
      fst <$> runTypeCheckM (checkTypeWithTypeVars (Set.fromList [TypeVar "A"]) t) initTypeEnv
        `shouldBe` Right ()

  describe "checkTypeWithTypeVarsOfKind" $ do
    it "performs kind checking for function type" $
      let t = TyArrow unitType boolType
          tcError = TypeIncorrectKind t StarK (mkKind 1) in
      fst <$> runTypeCheckM (checkTypeWithTypeVarsOfKind Set.empty (mkKind 1) t) initTypeEnv
        `shouldBe` Left tcError

    it "performs kind checking for forall type" $
      let t = TyForAll (TypeVar "A") (TyMonad $ MTyMonad (SinMonad Id)) in
      fst <$> runTypeCheckM (checkTypeWithTypeVarsOfKind Set.empty (mkKind 1) t) initTypeEnv
        `shouldBe` Right ()

    it "performs kind checking for tuple type" $
      let t = TyTuple [unitType]
          tcError = TypeIncorrectKind t StarK (mkKind 1) in
      fst <$> runTypeCheckM (checkTypeWithTypeVarsOfKind Set.empty (mkKind 1) t) initTypeEnv
        `shouldBe` Left tcError

  describe "isMonadSuffixOf" $ do
    it "is reflexive" $
      let m = MTyMonad (SinMonad Id)
      in m `isMonadSuffixOf` m

    it "returns False for two different monads" $
      let m1 = MTyMonad (SinMonad Id)
          m2 = MTyMonad (SinMonad NonTerm)
      in not $ m1 `isMonadSuffixOf` m2

    it "handles alpha equivalence" $
      let m1 = MTyMonad (SinMonadApp (SinMonad Error) (TyForAll (TypeVar "A") (mkTypeVar "A")))
          m2 = MTyMonad (SinMonadApp (SinMonad Error) (TyForAll (TypeVar "B") (mkTypeVar "B")))
      in m1 `isMonadSuffixOf` m2

    it "is not commutative" $
      let m1 = MTyMonad (SinMonad Id)
          m2 = MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad Id)
      in (m1 `isMonadSuffixOf` m2) `shouldBe` not (m2 `isMonadSuffixOf` m1)

    it "handles two monad conses" $
      let m1 = MTyMonadCons (SinMonad NonTerm) (MTyMonad $ SinMonad IO)
          m2 = MTyMonadCons (SinMonad State) m1
      in m1 `isMonadSuffixOf` m2

    it "returns False for non-matching ending" $
      let m1 = MTyMonadCons (SinMonad NonTerm) (MTyMonad $ SinMonad IO)
          m2 = MTyMonadCons (SinMonad State) (MTyMonadCons (SinMonad NonTerm) (MTyMonad $ SinMonad Id))
      in not $ m1 `isMonadSuffixOf` m2

    it "returns False for a suffix with injected monad (bug)" $
      let m1 = MTyMonadCons (SinMonad NonTerm) (MTyMonad $ SinMonad Id)
          m2 = MTyMonadCons (SinMonad NonTerm) (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad Id))
      in not $ m1 `isMonadSuffixOf` m2

  describe "isCompatibleMonadWith" $ do
    it "is reflexive" $
      let m = MTyMonadCons (SinMonad NonTerm) (MTyMonad $ SinMonad Id)
      in m `isCompatibleMonadWith` m

    it "returns False for incompatible monads" $
      let m1 = MTyMonadCons (SinMonad NonTerm) (MTyMonad $ SinMonad Id)
          m2 = MTyMonadCons (SinMonad NonTerm) (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad Id))
      in not $ m1 `isCompatibleMonadWith` m2

    it "handles alpha equivalence" $
      let m1 = MTyMonad (SinMonadApp (SinMonad Error) (TyForAll (TypeVar "A") (mkTypeVar "A")))
          m2 = MTyMonad (SinMonadApp (SinMonad Error) (TyForAll (TypeVar "B") (mkTypeVar "B")))
      in m1 `isCompatibleMonadWith` m2

    it "is commutative" $
      let m1 = MTyMonadCons (SinMonad IO) (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad NonTerm))
          m2 = MTyMonadCons (SinMonad IO) (MTyMonad $ SinMonad State)
      in (m1 `isCompatibleMonadWith` m2) `shouldBe` (m2 `isCompatibleMonadWith` m1)

  describe "isCompatibleWith" $ do
    it "is reflexive" $
      let t = TyApp (TyMonad $ MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad IO)) intType
      in t `isCompatibleWith` t

    it "returns True for compatible types" $
      let t1 = TyMonad $ MTyMonadCons (SinMonad IO) (MTyMonad $ SinMonad State)
          t2 = TyMonad $ MTyMonadCons (SinMonad IO) (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad NonTerm))
      in t1 `isCompatibleWith` t2

    it "returns False for incompatible types" $
      let t1 = unitType
          t2 = boolType
      in not $ t1 `isCompatibleWith` t2

    it "returns False for incompatible monadic types" $  -- TODO: future work, commutative effects?
      let t1 = TyMonad $ MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad IO)
          t2 = TyMonad $ MTyMonadCons (SinMonad IO) (MTyMonad $ SinMonad State)
      in not $ t1 `isCompatibleWith` t2

    it "returns False when monad application is incompatible in the result type" $
      let t1 = TyApp (TyMonad $ MTyMonad (SinMonad IO)) (TyMonad $ MTyMonad (SinMonad State))
          t2 = TyApp (TyMonad $ MTyMonad (SinMonad IO)) (TyMonad $ MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad IO))
      in not $ t1 `isCompatibleWith` t2

    it "handles alpha equivalence" $
      let t1 = TyApp (TyMonad $ MTyMonad (SinMonadApp (SinMonad Error) (TyForAll (TypeVar "A") (mkTypeVar "A")))) intType
          t2 = TyApp (TyMonad $ MTyMonad (SinMonadApp (SinMonad Error) (TyForAll (TypeVar "B") (mkTypeVar "B")))) intType
      in t1 `isCompatibleWith` t2

    it "is not commutative" $
      let t1 = TyApp (TyMonad $ MTyMonadCons (SinMonad IO) (MTyMonad $ SinMonad State)) boolType
          t2 = TyApp (TyMonad $ MTyMonadCons (SinMonad IO) (MTyMonadCons (SinMonad State) (MTyMonad $ SinMonad NonTerm))) boolType
      in (t1 `isCompatibleWith` t2) `shouldBe` not (t2 `isCompatibleWith` t1)

-- * Infrastructure

successCase :: Type -> IO ()
successCase = successCaseWithSetup (return ())

successCaseWithSetup :: TypeCheckM () -> Type -> IO ()
successCaseWithSetup setup t =
  fst <$> runTypeCheckM (setup >> checkType t) initTypeEnv
    `shouldBe` Right ()

failureCase :: Type -> TcError -> IO ()
failureCase = failureCaseWithSetup (return ())

failureCaseWithSetup :: TypeCheckM () -> Type -> TcError -> IO ()
failureCaseWithSetup setup t tcError =
  fst <$> runTypeCheckM (setup >> checkType t) initTypeEnv
    `shouldBe` Left tcError

