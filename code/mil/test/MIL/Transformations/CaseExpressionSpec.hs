-- | MIL case expression transformations tests.
module MIL.Transformations.CaseExpressionSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.CaseExpression
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec


-- | Main specification function.
spec :: Spec
spec = do
  describe "eliminateConstantCase" $ do
    it "removes redundant case expression" $
      transformationTestCase "EliminateConstantCase" eliminateConstantCase

  describe "extractCommonBind" $ do
    it "moves common bind out of the case alternatives" $
      transformationTestCase "ExtractCommonBind" extractCommonBind

