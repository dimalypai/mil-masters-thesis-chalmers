-- | MIL Id monad transformations tests.
module MIL.Transformations.IdSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.Id
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec


-- | Main specification function.
spec :: Spec
spec = do
  describe "exchange" $ do
    it "reorders two computations" $
      transformationTestCase "IdExchange" exchange

