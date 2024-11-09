module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, Person, PhoneNumber, address, person)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldlDefault, foldrDefault, sequence, sequenceDefault, traverse)
import Data.Validation.Semigroup (V)

addMaybe ∷ Maybe Int → Maybe Int → Maybe Int
addMaybe = lift2 add

subMaybe ∷ Maybe Int → Maybe Int → Maybe Int
subMaybe = lift2 sub

mulMaybe ∷ Maybe Int → Maybe Int → Maybe Int
mulMaybe = lift2 mul

divMaybe ∷ Maybe Int → Maybe Int → Maybe Int
divMaybe = lift2 div

addApply ∷ ∀ f a. Semiring a ⇒ Apply f ⇒ f a → f a → f a
addApply = lift2 add

subApply ∷ ∀ f a. Ring a ⇒ Apply f ⇒ f a → f a → f a
subApply = lift2 sub

mulApply ∷ ∀ f a. Semiring a ⇒ Apply f ⇒ f a → f a → f a
mulApply = lift2 mul

divApply ∷ ∀ f a. EuclideanRing a ⇒ Apply f ⇒ f a → f a → f a
divApply = lift2 div

combineMaybeAdo ∷ ∀ f a. Applicative f ⇒ Maybe (f a) → f (Maybe a)
combineMaybeAdo Nothing = pure Nothing
combineMaybeAdo (Just fa) = ado
  a ← fa
  in Just a

combineMaybe ∷ ∀ f a. Applicative f ⇒ Maybe (f a) → f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = Just <$> fa

-- Write a regular expression stateRegex :: Regex to check that a string only
-- contains two alphabetic characters. Hint: see the source code for
-- phoneNumberRegex.
stateRegex ∷ Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex ∷ Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved ∷ Address → V Errors Address
validateAddressImproved a =
  ado
    street ← matches "Street" nonEmptyRegex a.street
    city ← matches "City" nonEmptyRegex a.city
    state ← matches "State" stateRegex a.state
    in address street city state

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

derive instance Eq a ⇒ Eq (Tree a)
derive instance Generic (Tree a) _

instance Show a ⇒ Show (Tree a) where
  show tree = genericShow tree

derive instance Functor (Tree)

instance Foldable (Tree) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap _ Leaf = mempty
  foldMap f (Branch left val right) = foldMap f left <> f val <> foldMap f right

instance Traversable (Tree) where
  sequence t = sequenceDefault t
  traverse ∷ ∀ a b m. Applicative m ⇒ (a → m b) → Tree a → m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch left val right) = ado
    left' ← traverse f left
    val' ← f val
    right' ← traverse f right
    in Branch left' val' right'

traversePreOrder ∷ ∀ a m b. Applicative m ⇒ (a → m b) → Tree a → m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch left val right) = ado
  val' ← f val
  left' ← traversePreOrder f left
  right' ← traversePreOrder f right
  in Branch left' val' right'

traversePostOrder ∷ ∀ a m b. Applicative m ⇒ (a → m b) → Tree a → m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch left val right) = ado
  left' ← traversePostOrder f left
  right' ← traversePostOrder f right
  val' ← f val
  in Branch left' val' right'

type PersonOptionalAddress =
  { firstName ∷ String
  , lastName ∷ String
  , homeAddress ∷ Maybe Address
  , phones ∷ Array PhoneNumber
  }

validatePersonAdo ∷ Person → V Errors Person
validatePersonAdo p =
  ado
    firstName ← nonEmpty "First Name" p.firstName
    lastName ← nonEmpty "Last Name" p.lastName
    address ← validateAddress p.homeAddress
    numbers ← validatePhoneNumbers "Phone Numbers" p.phones
    in person firstName lastName address numbers

personOptionalAddress ∷ String → String → Maybe Address → Array PhoneNumber → PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress ∷ PersonOptionalAddress → V Errors PersonOptionalAddress
validatePersonOptionalAddress p = ado
  firstName ← nonEmpty "First Name" p.firstName
  lastName ← nonEmpty "Last Name" p.lastName
  -- address ← (traverse validateAddress ∷ Maybe Address → V Errors (Maybe Address)) p.homeAddress
  -- (address ∷ Maybe Address) ← (traverse validateAddress p.homeAddress ∷ V (Array String) (Maybe Address))
  address ← traverse validateAddress p.homeAddress
  numbers ← validatePhoneNumbers "Phone Numbers" p.phones
  in personOptionalAddress firstName lastName address numbers

sequenceUsingTraverse ∷ ∀ a m t. Traversable t ⇒ Applicative m ⇒ t (m a) → m (t a)
sequenceUsingTraverse t = traverse (identity ∷ m a → m a) t

traverseUsingSequence ∷ ∀ a b m t. Traversable t ⇒ Applicative m ⇒ (a → m b) → t a → m (t b)
traverseUsingSequence f t = sequence $ (map f t ∷ t (m b))
