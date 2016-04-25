-- | MIL Error (exception) transformations tests.
module MIL.Transformations.ErrorSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.Error
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec


-- | Main specification function.
spec :: Spec
spec = do
  describe "eliminateThrowCatch" $ do
    it "removes redundant throw_error and catch_error" $
      transformationTestCase "EliminateThrowCatch" eliminateThrowCatch

  describe "eliminateThrowCatchNoException" $ do
    it "removes redundant return and catch_error" $
      transformationTestCase "EliminateThrowCatchNoException" eliminateThrowCatchNoException

