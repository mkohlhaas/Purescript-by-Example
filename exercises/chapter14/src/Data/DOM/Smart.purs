module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey
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

-- Smart Constructor

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

-- Smart Constructors

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

-- Smart Constructors

attribute ∷ AttributeKey → String → Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: value
  }

infix 4 attribute as :=

-------------------
-- Attribute Key --
-------------------

newtype AttributeKey = AttributeKey String

-- Smart Constructors

href ∷ AttributeKey
href = AttributeKey "href"

_class ∷ AttributeKey
_class = AttributeKey "class"

src ∷ AttributeKey
src = AttributeKey "src"

width ∷ AttributeKey
width = AttributeKey "width"

height ∷ AttributeKey
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
