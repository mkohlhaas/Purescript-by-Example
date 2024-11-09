module Main where

import Prelude

import Effect.Console (log)
import Euler (answer)

main = do
  log ("The answer is " <> show (answer 1000))
