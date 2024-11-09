module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet ∷ String → AddressBook → Maybe Entry
findEntryByStreet street = filter (_.address.street >>> (_ == street)) >>> head

isInBook ∷ String → String → AddressBook → Boolean
isInBook firstName lastName = filter filterEntry >>> not null
  where
  filterEntry ∷ Entry → Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates ∷ AddressBook → AddressBook
removeDuplicates = nubByEq eqFn
  where
  eqFn ∷ Entry → Entry → Boolean
  eqFn entry other = entry.firstName == other.firstName && entry.lastName == other.lastName
