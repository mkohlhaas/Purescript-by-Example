{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "functions"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "strings"
  , "test-unit"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
