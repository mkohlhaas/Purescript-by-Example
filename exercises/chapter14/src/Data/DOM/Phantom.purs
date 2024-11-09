module Data.DOM.Phantom
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , class IsValue
  , toValue
  , a
  , p
  , img
  , href
  , _class
  , src
  , width
  , height
  , attribute
  , (:=)
  , text
  , elem
  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

-------------
-- Element --
-------------

newtype Element = Element
  { name ∷ String
  , attribs ∷ Array Attribute
  , content ∷ Maybe (Array Content)
  }

element ∷ String → Array Attribute → Maybe (Array Content) → Element
element name attribs content = Element
  { name: name
  , attribs: attribs
  , content: content
  }

-------------
-- Content --
-------------

data Content
  = TextContent String
  | ElementContent Element

text ∷ String → Content
text = TextContent

elem ∷ Element → Content
elem = ElementContent

---------------
-- Attribute --
---------------

newtype Attribute = Attribute
  { key ∷ String
  , value ∷ String
  }

class IsValue a where
  toValue ∷ a → String

instance IsValue String where
  toValue = identity

instance IsValue Int where
  toValue = show

attribute ∷ ∀ a. IsValue a ⇒ AttributeKey a → a → Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

-------------------
-- Attribute Key --
-------------------

newtype AttributeKey ∷ ∀ k. k → Type
newtype AttributeKey a = AttributeKey String

href ∷ AttributeKey String
href = AttributeKey "href"

_class ∷ AttributeKey String
_class = AttributeKey "class"

src ∷ AttributeKey String
src = AttributeKey "src"

width ∷ AttributeKey Int
width = AttributeKey "width"

height ∷ AttributeKey Int
height = AttributeKey "height"

------------------------
-- Smart Constructors --
------------------------

a ∷ Array Attribute → Array Content → Element
a attribs content = element "a" attribs (Just content)

p ∷ Array Attribute → Array Content → Element
p attribs content = element "p" attribs (Just content)

img ∷ Array Attribute → Element
img attribs = element "img" attribs Nothing

render ∷ Element → String
render (Element e) =
  "<" <> e.name
    <> " "
    <> joinWith " " (map renderAttribute e.attribs)
    <> renderContent e.content
  where
  renderAttribute ∷ Attribute → String
  renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""

  renderContent ∷ Maybe (Array Content) → String
  renderContent Nothing = " />"
  renderContent (Just content) =
    ">" <> joinWith "" (map renderContentItem content)
      <> "</"
      <> e.name
      <> ">"
    where
    renderContentItem ∷ Content → String
    renderContentItem (TextContent s) = s
    renderContentItem (ElementContent e') = render e'
