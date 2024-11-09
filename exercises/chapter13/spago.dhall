{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "prelude"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
