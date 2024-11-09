module Test.MySolutions where

import Data.Maybe
import Data.Path
import Prelude

import Control.Alternative (guard)
import Data.Array (filter, foldl, head, tail, (..))
import Test.Examples as TE

isEven ∷ Int → Boolean
isEven n = n `mod` 2 == 0

countEven ∷ Array Int → Int
countEven [] = 0
countEven arr
  | fromMaybe 0 (head arr) `mod` 2 == 0 = 1 + countEven (fromMaybe [] (tail arr))
  | otherwise = countEven (fromMaybe [] (tail arr))

squared ∷ ∀ a. Semiring a ⇒ Array a → Array a
squared arr = square <$> arr
  where
  square = \n → n * n

keepNonNegative ∷ Array Number → Array Number
keepNonNegative arr = filter (_ >= 0.0) arr

infix 5 filter as <$?>

keepNonNegativeRewrite ∷ Array Number → Array Number
keepNonNegativeRewrite arr = (_ >= 0.0) <$?> arr

isPrime ∷ Int → Boolean
isPrime n
  | n > 1 = TE.length (TE.factors n) == 1
  | otherwise = false

cartesianProduct ∷ ∀ a. Array a → Array a → Array (Array a)
cartesianProduct arr1 arr2 = do
  a1 ← arr1
  a2 ← arr2
  pure [ a1, a2 ]

-- Write a function triples :: Int -> Array (Array Int), which takes a number n
-- and returns all Pythagorean triples whose components (the a , b , and c
-- values) are each less than or equal to n . A Pythagorean triple is an array of numbers [a,b,c]
-- such that a2+b2=c2 . Hint: Use the guard function in an array comprehension.
triples ∷ Int → Array (Array Int)
triples n = do
  a ← 1 .. n
  b ← a .. n
  c ← b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

-- Array pattern matching is a nightmare. Should use uncons from Data.Array but too messy.
-- Use Lists from the get go!
primeFactors ∷ Int → Array Int
primeFactors n | n > 1 = go (TE.factors n) []
  where
  go ∷ Array (Array Int) → Array Int → Array Int
  go [ [ 1, n ] ] acc = acc <> [ n ]
  go [ [ _, _ ], [ p, r ] ] acc = go (TE.factors r) $ acc <> [ p ]
  go [ [ _, _ ], [ p, r ], _ ] acc = go (TE.factors r) $ acc <> [ p ]
  go [ [ _, _ ], [ p, r ], _, _ ] acc = go (TE.factors r) $ acc <> [ p ]
  go [ [ _, _ ], [ p, r ], _, _, _ ] acc = go (TE.factors r) $ acc <> [ p ]
  go [ [ _, _ ], [ p, r ], _, _, _, _ ] acc = go (TE.factors r) $ acc <> [ p ]
  go [ [ _, _ ], [ p, r ], _, _, _, _, _ ] acc = go (TE.factors r) $ acc <> [ p ]
  go [ [ _, _ ], [ p, r ], _, _, _, _, _, _ ] acc = go (TE.factors r) $ acc <> [ p ]
  go _ _ = [] -- should never happen
primeFactors _ = []

allTrue ∷ Array Boolean → Boolean
allTrue = foldl (\b a → b && a) true

fibTailRec ∷ Int → Int
fibTailRec n = go n [ 0, 1 ]
  where
  go 0 [ a, _ ] = a
  go 1 [ _, b ] = b
  go n [ a, b ] = go (n - 1) [ b, a + b ]
  go _ _ = 0 -- should never happen

reverse ∷ ∀ a. Array a → Array a
reverse = foldl (\as a → [ a ] <> as) []

onlyFiles ∷ Path → Array Path
onlyFiles path = filter (not isDirectory) $ TE.allFiles path

whereIs ∷ Path → String → Maybe Path
whereIs path fileName = head $ do
  path' ← TE.allFiles path
  child ← ls path'
  guard $ filename child == filename path' <> fileName
  pure path'
