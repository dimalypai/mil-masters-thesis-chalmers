-- | MIL lift operator transformations tests.
module MIL.Transformations.LiftSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.Lift
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec


-- | Main specification function.
spec :: Spec
spec = do
  describe "liftIdentity" $ do
    it "removes redundant lift" $
      transformationTestCase "LiftIdentity" liftIdentity

  describe "composeLift" $ do
    it "combines two lifts into one" $
      transformationTestCase "ComposeLift" composeLift

