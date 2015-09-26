-- | MIL State monad transformations tests.
module MIL.Transformations.StateSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.State
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec


-- | Main specification function.
spec :: Spec
spec = do
  describe "exchangeNew" $ do
    it "reorders new_refs applied to values" $
      transformationTestCase "ExchangeNew" exchangeNew

  describe "exchangeRead" $ do
    it "reorders read_refs" $
      transformationTestCase "ExchangeRead" exchangeRead

  describe "useRead" $ do
    it "eliminates one of the two similar read_refs" $
      transformationTestCase "UseRead" useRead

  describe "useWrite" $ do
    it "eliminates read from just written reference" $
      transformationTestCase "UseWrite" useWrite

