{ haskellPackages ? (import <nixpkgs>{}).haskellPackages }:
let
  all = (import ../../nix-packages.nix) { haskellPackages = haskellPackages; isShell = true; };
in
all.wayland-wire.env
