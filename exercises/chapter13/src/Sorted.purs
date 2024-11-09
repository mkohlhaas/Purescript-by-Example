module Sorted where

import Prelude

import Data.Array (sort)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype Sorted a = Sorted (Array a)

sorted ∷ ∀ a. Sorted a → Array a
sorted (Sorted xs) = xs

derive newtype instance Show a ⇒ Show (Sorted a)

instance (Arbitrary a, Ord a) ⇒ Arbitrary (Sorted a) where
  arbitrary = map (Sorted <<< sort) arbitrary
