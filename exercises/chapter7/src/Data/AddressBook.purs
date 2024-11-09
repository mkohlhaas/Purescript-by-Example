module Data.AddressBook where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type Address =
  { street ∷ String
  , city ∷ String
  , state ∷ String
  }

address ∷ String → String → String → Address
address street city state = { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

{-
Eq and Show instances are needed by unit tests to
compare and report differences between PhoneType values
(HomePhone, WorkPhone, etc).
-}
derive instance Eq PhoneType

-- Generic Show instance
derive instance Generic PhoneType _

instance Show PhoneType where
  show = genericShow

type PhoneNumber =
  { type ∷ PhoneType
  , number ∷ String
  }

phoneNumber ∷ PhoneType → String → PhoneNumber
phoneNumber phonetype number =
  { type: phonetype
  , number: number
  }

type Person =
  { firstName ∷ String
  , lastName ∷ String
  , homeAddress ∷ Address
  , phones ∷ Array PhoneNumber
  }

person ∷ String → String → Address → Array PhoneNumber → Person
person firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

examplePerson ∷ Person
examplePerson =
  person "John" "Smith"
    (address "123 Fake St." "FakeTown" "CA")
    [ phoneNumber HomePhone "555-555-5555"
    , phoneNumber CellPhone "555-555-0000"
    ]
