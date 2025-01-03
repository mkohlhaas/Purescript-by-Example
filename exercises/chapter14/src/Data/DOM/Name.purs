module Data.DOM.Name
  ( Element
  , Attribute
  , Name
  , Content
  , ContentF
  , AttributeKey
  , class IsValue
  , toValue
  , Href(..)
  , a
  , p
  , img
  , href
  , _class
  , src
  , width
  , height
  , name
  , attribute
  , (:=)
  , text
  , elem
  , newName
  , render
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (get, put)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name ∷ String
  , attribs ∷ Array Attribute
  , content ∷ Maybe (Content Unit)
  }

newtype Name = Name String

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | NewName (Name → a)

instance Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (NewName k) = NewName (f <<< k)

type Content = Free ContentF

newtype Attribute = Attribute
  { key ∷ String
  , value ∷ String
  }

newtype AttributeKey ∷ ∀ k. k → Type
newtype AttributeKey a = AttributeKey String

element ∷ String → Array Attribute → Maybe (Content Unit) → Element
element name_ attribs content = Element { name: name_, attribs, content }

text ∷ String → Content Unit
text s = liftF $ TextContent s unit

elem ∷ Element → Content Unit
elem e = liftF $ ElementContent e unit

newName ∷ Content Name
newName = liftF $ NewName identity

class IsValue a where
  toValue ∷ a → String

instance IsValue String where
  toValue = identity

instance IsValue Int where
  toValue = show

instance IsValue Name where
  toValue (Name n) = n

attribute ∷ ∀ a. IsValue a ⇒ AttributeKey a → a → Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a ∷ Array Attribute → Content Unit → Element
a attribs content = element "a" attribs (Just content)

p ∷ Array Attribute → Content Unit → Element
p attribs content = element "p" attribs (Just content)

img ∷ Array Attribute → Element
img attribs = element "img" attribs Nothing

data Href
  = URLHref String
  | AnchorHref Name

instance IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name name)) = "#" <> name

href ∷ AttributeKey Href
href = AttributeKey "href"

name ∷ AttributeKey Name
name = AttributeKey "name"

_class ∷ AttributeKey String
_class = AttributeKey "class"

src ∷ AttributeKey String
src = AttributeKey "src"

width ∷ AttributeKey Int
width = AttributeKey "width"

height ∷ AttributeKey Int
height = AttributeKey "height"

type Interp = WriterT String (State Int)

render ∷ Element → String
render = flip evalState 0 <<< execWriterT <<< renderElement
  where
  renderElement ∷ Element → Interp Unit
  renderElement (Element e) = do
    tell "<"
    tell e.name
    for_ e.attribs $ \x → do
      tell " "
      renderAttribute x
    renderContent e.content
    where
    renderAttribute ∷ Attribute → Interp Unit
    renderAttribute (Attribute x) = do
      tell x.key
      tell "=\""
      tell x.value
      tell "\""

    renderContent ∷ Maybe (Content Unit) → Interp Unit
    renderContent Nothing = tell " />"
    renderContent (Just content) = do
      tell ">"
      runFreeM renderContentItem content
      tell "</"
      tell e.name
      tell ">"

    renderContentItem ∷ ∀ a. ContentF (Content a) → Interp (Content a)
    renderContentItem (TextContent s rest) = do
      tell s
      pure rest
    renderContentItem (ElementContent e' rest) = do
      renderElement e'
      pure rest
    renderContentItem (NewName k) = do
      n ← get
      let fresh = Name $ "name" <> show n
      put $ n + 1
      pure (k fresh)
