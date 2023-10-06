module Spec where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec $ do
  describe "Example test case" $ do
    prop "Example property" prop_Example

prop_Example :: Property
prop_Example = forAll arbitrary \(b :: Bool) -> b && True == b
