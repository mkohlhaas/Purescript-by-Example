module Test.MySolutions where

import Prelude

import Data.Array (nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (fromJust)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Show.Generic (genericShow)

newtype Point = Point
  { x ∷ Number
  , y ∷ Number
  }

instance Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

-- derive newtype instance Show Point
-- "{ x: 1.0, y: 2.0 }"

newtype Complex = Complex
  { real ∷ Number
  , imaginary ∷ Number
  }

instance Show Complex where
  show (Complex { real, imaginary }) =
    show real
      <> (if imaginary < 0.0 then "" else "+")
      <> show imaginary
      <> "i"

derive instance Eq Complex

-- instance Semiring Complex where
--   add (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = Complex { real: r1 + r2, imaginary: i1 + i2 }
--   zero = Complex { real: 0.0, imaginary: 0.0 }
--   mul (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = Complex { real: r1 * r2 - i1 * i2, imaginary: r1 * i2 + i1 * r2 }
--   one = Complex { real: 1.0, imaginary: 0.0 }

derive instance Newtype Complex _

instance Semiring Complex where
  add = over2 Complex add
  -- over2 ∷ Newtype Complex Record ⇒ (Record → Complex) → (Record → Record → Record) → Complex → Complex → Complex
  mul = over2 Complex
    \{ real: r1, imaginary: i1 }
     { real: r2, imaginary: i2 } →
      { real: r1 * r2 - i1 * i2
      , imaginary: r1 * i2 + r2 * i1
      }
  zero = wrap zero
  one = wrap { real: one, imaginary: zero }

-- instance Ring Complex where
--   sub = over2 Complex sub

derive newtype instance Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance Generic Shape _

instance Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

derive instance Eq a ⇒ Eq (NonEmpty a)

derive instance Generic (NonEmpty a) _

instance Show a ⇒ Show (NonEmpty a) where
  show = genericShow

instance Semigroup (NonEmpty a) where
  append (NonEmpty init1 arr1) (NonEmpty init2 arr2) = NonEmpty init1 (arr1 <> [ init2 ] <> arr2)

derive instance Functor (NonEmpty)

data Extended a = Finite a | Infinite

derive instance Eq a ⇒ Eq (Extended a)

derive instance (Eq a, Ord a) ⇒ Ord (Extended a)

instance Foldable (NonEmpty) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (NonEmpty init arr) = foldMap f $ [ init ] <> arr

data OneMore f a = OneMore a (f a)

instance Foldable f ⇒ Foldable (OneMore f) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (OneMore start container) = f start <> foldMap f container

derive instance Eq Point
derive instance Ord Point
derive instance Eq Shape
derive instance Ord Shape

dedupShapes ∷ Array Shape → Array Shape
dedupShapes = nubEq

dedupShapesFast ∷ Array Shape → Array Shape
dedupShapesFast = nub

unsafeMaximum ∷ Partial ⇒ Array Int → Int
unsafeMaximum arr = fromJust $ maximum arr

class Monoid m ⇐ Action m a where
  act ∷ m → a → a

newtype Multiply = Multiply Int

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act ∷ Multiply → Int → Int
  act (Multiply n) m = n * m

instance Action Multiply String where
  act (Multiply n) s = power s n

instance Action m a ⇒ Action m (Array a) where
  act ∷ m → Array a → Array a
  act m = map (act m)

newtype Self m = Self m

instance Monoid m ⇒ Action m (Self m) where
  act ∷ m → Self m → Self m
  act m (Self n) = Self (m <> n)

derive newtype instance Eq m ⇒ Eq (Self m)
derive newtype instance Show m ⇒ Show (Self m)
derive newtype instance Eq Multiply
derive newtype instance Show Multiply

arrayHasDuplicates ∷ ∀ a. Hashable a ⇒ Array a → Boolean
arrayHasDuplicates arr = nubByEq (\a1 a2 → a1 == a2 && hash a1 == hash a2) arr /= arr

newtype Hour = Hour Int

instance Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance Hashable Hour where
  hash (Hour n) = hash $ mod n 12
