module Test.DList.Instances where

import Prelude

import Test.QuickCheck.Arbitrary
import Data.DList
import qualified Data.List as L

instance arbDList :: (Arbitrary a) => Arbitrary (DList a) where
  arbitrary = (toDList :: Array a -> DList a) <$> arbitrary

instance arbList :: (Arbitrary a) => Arbitrary (L.List a) where
  arbitrary = (L.toList :: Array a -> L.List a) <$> arbitrary
