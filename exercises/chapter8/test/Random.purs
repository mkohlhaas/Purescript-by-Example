module Test.Random where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)

main ∷ Effect Unit
main = do
  n ← random
  logShow n
