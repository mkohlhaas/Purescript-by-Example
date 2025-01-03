module Example.Shapes where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Graphics.Canvas
  ( arc
  , closePath
  , fillPath
  , getCanvasElementById
  , getContext2D
  , lineTo
  , moveTo
  , rect
  , setFillStyle
  )
import Partial.Unsafe (unsafePartial)

translate
  ∷ ∀ r
  . Number
  → Number
  → { x ∷ Number, y ∷ Number | r }
  → { x ∷ Number, y ∷ Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

main ∷ Effect Unit
main = unsafePartial do
  Just canvas ← getCanvasElementById "canvas"
  ctx ← getContext2D canvas
  setFillStyle ctx "#00F"
  fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }
  setFillStyle ctx "#0F0"
  fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , radius: 50.0
    , start: 0.0
    , end: Number.tau * 2.0 / 3.0
    , useCounterClockwise: false
    }
  setFillStyle ctx "#F00"
  fillPath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx
