module Tree where

import Prelude

import Data.Array (singleton)
import Data.Foldable (foldr)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance (Arbitrary a, Ord a) ⇒ Arbitrary (Tree a) where
  arbitrary = map fromArray arbitrary

instance (Coarbitrary a) ⇒ Coarbitrary (Tree a) where
  coarbitrary Leaf = identity
  coarbitrary (Branch l a r) = coarbitrary l <<< coarbitrary a <<< coarbitrary r

insert ∷ ∀ a. (Ord a) ⇒ a → Tree a → Tree a
insert a Leaf = Branch Leaf a Leaf
insert a (Branch l v r) | a < v = Branch (insert a l) v r
insert a (Branch l v r) = Branch l v (insert a r)

member ∷ ∀ a. (Ord a) ⇒ a → Tree a → Boolean
member _ Leaf = false
member a (Branch _ v _) | a == v = true
member a (Branch l v _) | a < v = member a l
member a (Branch _ _ r) = member a r

toArray ∷ ∀ a. Tree a → Array a
toArray Leaf = []
toArray (Branch l a r) = toArray l <> singleton a <> toArray r

fromArray ∷ ∀ a. (Ord a) ⇒ Array a → Tree a
fromArray = foldr insert Leaf

anywhere ∷ ∀ a. (Tree a → Boolean) → Tree a → Boolean
anywhere f Leaf = f Leaf
anywhere f t@(Branch l _ r) = anywhere f l || f t || anywhere f r
