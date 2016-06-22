{ profiling ? false
, ghc ? null
}:
let
  orignalPackages =
    if ghc == null
    then (import <nixpkgs>{}).haskellPackages
    else builtins.getAttr ghc ((import <nixpkgs>{}).haskell.packages);

  haskellPackages =
  if profiling
  then orignalPackages.override {
    overrides = self : super: { mkDerivation = drv : super.mkDerivation (drv // {enableLibraryProfiling = true;});
    };
  }
  else orignalPackages;
  all = (import ../../nix-packages.nix) { haskellPackages = haskellPackages; isShell = true; };
in
all.wayland-wire.env
