module Example.Rectangle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas
  ( fillPath
  , getCanvasElementById
  , getContext2D
  , rect
  , setFillStyle
  )
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = unsafePartial do
  Just canvas ← getCanvasElementById "canvas"
  ctx ← getContext2D canvas
  setFillStyle ctx "#08F"
  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 300.0
    , height: 100.0
    }
