module Test.Examples where

import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, filter, tail, (..), (:))
import Data.Foldable (product)
import Data.Maybe (fromMaybe)
import Data.Path (Path, ls)

factorial ∷ Int → Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib ∷ Int → Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length ∷ ∀ a. Array a → Int
length [] = 0
length arr = 1 + (length $ fromMaybe [] $ tail arr)

factors ∷ Int → Array (Array Int)
factors n = filter (\xs → product xs == n) do
  i ← 1 .. n
  j ← i .. n
  pure [ i, j ]

factorsV2 ∷ Int → Array (Array Int)
factorsV2 n = filter (\xs → product xs == n) do
  i ← 1 .. n
  j ← i .. n
  [ [ i, j ] ]

factorsV3 ∷ Int → Array (Array Int)
factorsV3 n = do
  i ← 1 .. n
  j ← i .. n
  guard $ i * j == n
  pure [ i, j ]

factorialTailRec ∷ Int → Int → Int
factorialTailRec 0 acc = acc
factorialTailRec n acc = factorialTailRec (n - 1) (acc * n)

lengthTailRec ∷ ∀ a. Array a → Int
lengthTailRec arr = length' arr 0
  where
  length' ∷ Array a → Int → Int
  length' [] acc = acc
  length' arr' acc = length' (fromMaybe [] $ tail arr') (acc + 1)

allFiles ∷ Path → Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' ∷ Path → Array Path
allFiles' file = file : do
  child ← ls file
  allFiles' child
