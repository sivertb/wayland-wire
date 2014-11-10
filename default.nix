{ cabal, cabalInstall, filterSource, hxt, networkMsg, utf8String, QuickCheck }:

cabal.mkDerivation
( self:
  { pname = "wayland-pure"
  ; version = "0.1.0.0"
  ; src = filterSource ./.
  ; buildTools = [ cabalInstall ]
  ; buildDepends = [ hxt networkMsg utf8String ]
  ; testDepends = [ QuickCheck ]
  ; doCheck = true
  ; enableSplitObjs = false
  ;
  }
)
