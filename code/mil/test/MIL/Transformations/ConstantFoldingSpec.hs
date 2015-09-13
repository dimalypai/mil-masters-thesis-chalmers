-- | MIL constant folding transformations tests.
module MIL.Transformations.ConstantFoldingSpec (main, spec) where

import Test.Hspec

import MIL.Transformations.ConstantFolding
import MIL.TestUtils

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec


-- | Main specification function.
spec :: Spec
spec = do
  describe "foldConstants" $ do
    it "folds constants in add_int" $
      transformationTestCase "AddIntConstFolding" foldConstants

    it "folds constants in add_float" $
      transformationTestCase "AddFloatConstFolding" foldConstants

    it "folds constants in sub_int" $
      transformationTestCase "SubIntConstFolding" foldConstants

    it "folds constants in sub_float" $
      transformationTestCase "SubFloatConstFolding" foldConstants

    it "folds constants in mul_int" $
      transformationTestCase "MulIntConstFolding" foldConstants

    it "folds constants in mul_float" $
      transformationTestCase "MulFloatConstFolding" foldConstants

