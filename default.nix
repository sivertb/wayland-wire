{ cabal, cabalInstall_1_20_0_6, filterSource, hxt, networkMsg, utf8String, QuickCheck, mtl, free, dietSet, thLift }:

cabal.mkDerivation
( self:
  { pname = "wayland-wire"
  ; version = "0.1.0"
  ; src = filterSource ./.
  ; buildTools = [ cabalInstall_1_20_0_6 ]
  ; buildDepends = [ hxt networkMsg utf8String mtl free dietSet thLift ]
  ; testDepends = [ QuickCheck ]
  ; doCheck = true
  ; enableSplitObjs = false
  ;
  }
)
