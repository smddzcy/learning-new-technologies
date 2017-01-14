module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)


type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress address = address.street <> ", " <> address.city <> ", " <> address.state

showEntry :: Entry -> String
showEntry entry = entry.firstName <> ", " <> entry.lastName <> ": " <> showAddress entry.address

showBook :: AddressBook -> String
showBook Nil = ""
showBook (Cons entry rem) = "Book: " <> showEntry entry <> " // " <> showBook rem

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

-- Use eta conversion to shorten the fn declaration
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName  == lastName

-- Use full params
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == streetName

addressBookContains :: String -> AddressBook -> Boolean
addressBookContains _ Nil = false
addressBookContains name (Cons firstEntry remaining) = name == firstEntry.firstName || addressBookContains name remaining

removeDuplicateEntries :: AddressBook -> AddressBook
removeDuplicateEntries = nubBy haveSameNames
  where
    haveSameNames :: Entry -> Entry -> Boolean
    haveSameNames e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
