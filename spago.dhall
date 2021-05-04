{ name = "typelevel-shunting-yard"
, dependencies = [ "console", "effect", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository =
    "https://github.com/PureFunctor/purescript-typelevel-shunting-yard.git"
}
