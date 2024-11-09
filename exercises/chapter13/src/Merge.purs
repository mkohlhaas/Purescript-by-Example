module Merge where

import Prelude

import Data.Function (on)
import Data.List (List(..), fromFoldable, reverse, toUnfoldable, (:))

merge ∷ Array Int → Array Int → Array Int
merge = mergePoly

mergePoly ∷ ∀ a. Ord a ⇒ Array a → Array a → Array a
mergePoly = mergeWith identity

mergeWith ∷ ∀ a b. Ord b ⇒ (a → b) → Array a → Array a → Array a
mergeWith f = \xs ys →
  toUnfoldable (go Nil (fromFoldable xs) (fromFoldable ys))
  where
  go acc Nil ys = reverse acc <> ys
  go acc xs Nil = reverse acc <> xs
  go acc xs@(x : xs') ys@(y : ys') =
    case (compare `on` f) x y of
      LT → go (Cons x acc) xs' ys
      _ → go (Cons y acc) xs ys'
