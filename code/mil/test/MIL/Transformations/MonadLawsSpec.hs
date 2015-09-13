-- | MIL monad laws transformations tests.
module MIL.Transformations.MonadLawsSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.MonadLaws
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "leftIdentity" $ do
    it "applies left identity monad law (simplest case)" $
      transformationTestCase "LeftIdentity" leftIdentity

    it "applies left identity monad law (all syntactic forms)" $
      transformationTestCase "LeftIdentityFullSyntax" leftIdentity

  describe "rightIdentity" $ do
    it "applies right identity monad law (simplest case)" $
      transformationTestCase "RightIdentity" rightIdentity

    it "applies right identity monad law (all syntactic forms)" $
      transformationTestCase "RightIdentityFullSyntax" rightIdentity

    it "does not apply when different variable than in the bind is returned" $
      transformationTestCase "RightIdentityDifferentVar" rightIdentity

  describe "associativity" $ do
    it "applies associativity monad law (simplest case)" $
      transformationTestCase "Associativity" associativity

    it "applies associativity monad law (all syntactic forms)" $
      transformationTestCase "AssociativityFullSyntax" associativity

