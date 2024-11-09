module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Person (Person)
import Data.Picture (Shape(..), getCenter, origin)

factorial ∷ Int → Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial ∷ Int → Int → Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
  | n < k = 0
  | otherwise = factorial n / (factorial k * factorial (n - k))

pascal ∷ Int → Int → Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

sameCity ∷ Person → Person → Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

fromSingleton ∷ ∀ a. a → Array a → a
fromSingleton _ [ a ] = a
fromSingleton default _ = default

circleAtOrigin = Circle origin 10.0

centerShape ∷ Shape → Shape
centerShape (Circle _ radius) = Circle origin radius
centerShape (Rectangle _ width height) = Rectangle origin width height
centerShape line@(Line start end) = Line (start - delta) (end - delta)
  where
  delta = getCenter line
centerShape (Text _ text) = Text origin text
centerShape (Clipped pic point w h) = Clipped pic (getCenter (centerShape (Rectangle point w h))) w h

scaleShape ∷ Number → Shape → Shape
scaleShape scale (Circle center radius) = Circle center (scale * radius)
scaleShape scale (Rectangle point width height) = Rectangle point (scale * width) (scale * height)
scaleShape scale (Line start end) = Line (start * scalePoint) (end * scalePoint)
  where
  scalePoint = { x: scale, y: scale }
scaleShape _ (Text center text) = Text center text
scaleShape scale (Clipped pic point w h) = Clipped pic (getCenter (scaleShape scale (Rectangle point w h))) w h

doubleScaleAndCenter ∷ Shape → Shape
doubleScaleAndCenter = centerShape >>> scaleShape 2.0

shapeText ∷ Shape → Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage ∷ Amp → Volt → Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

area ∷ Shape → Number
area (Circle _ r) = pi * r * r
area (Rectangle _ width height) = width * height
area _ = 0.0
