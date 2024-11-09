module Main where

import Prelude

import Data.Hashable (hash, hashEqual)
import Effect (Effect)
import Effect.Class.Console (logShow)

main ∷ Effect Unit
main = do
  logShow $ hash 123
  logShow (hash true)
  logShow (hash [ 1, 2, 3 ])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")
