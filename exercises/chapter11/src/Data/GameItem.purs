module Data.GameItem where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Read (class Read)

data GameItem = Candle | Matches

instance Show GameItem where
  show Candle = "Candle"
  show Matches = "Matches"

derive instance Eq GameItem
derive instance Ord GameItem

instance Read GameItem where
  read "Candle" = Just Candle
  read "Matches" = Just Matches
  read _ = Nothing
