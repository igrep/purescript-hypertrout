let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201206/packages.dhall sha256:c9ffd7577fb8ee2197309591d7ccc0f506ee37b9078866f0ef159f5abbb1b32b

in  upstream
  with trout =
    { dependencies = upstream.trout.dependencies
    , repo = "https://github.com/igrep/purescript-trout.git"
    , version = "f79bd75e4c513d7495afb11e3c431fc4b74121bd"
    }
