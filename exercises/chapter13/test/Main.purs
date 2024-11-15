module Test.Main where

import Prelude

import Data.Array (sort, sortBy)
import Data.Foldable (foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable, (:))
import Effect (Effect)
import Merge (merge, mergePoly, mergeWith)
import Sorted (sorted)
import Test.QuickCheck (mkSeed, quickCheck, quickCheckPure, (<?>))
import Tree (Tree, anywhere, insert, member, toArray)

isSorted ∷ ∀ a. (Ord a) ⇒ Array a → Boolean
isSorted = go <<< fromFoldable
  where
  go Nil = true
  go (_ : Nil) = true
  go (x1 : t@(x2 : _)) = x1 <= x2 && go t

ints ∷ Array Int → Array Int
ints = identity

intToBool ∷ (Int → Boolean) → Int → Boolean
intToBool = identity

treeOfInt ∷ Tree Int → Tree Int
treeOfInt = identity

main ∷ Effect Unit
main = do

  ------------------------------
  -- Tests for module 'Merge' --
  ------------------------------

  quickCheck \xs ys →
    let
      result = merge (sort xs) (sort ys)
      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys →
    eq (merge (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys →
    eq (mergePoly (sorted xs) (sorted ys) # ints) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys f →
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected

  -----------------------------
  -- Tests for module 'Tree' --
  -----------------------------

  quickCheck \t a → member a $ insert a $ treeOfInt t

  quickCheck \t xs → isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck \f g t →
    anywhere (\s → f s || g s) t == anywhere f (treeOfInt t) || anywhere g t
