{ mkDerivation, base, binary, bytestring, c2hs, containers
, diet-set, directory, free, hxt, mtl, network, QuickCheck, stdenv
, template-haskell, th-lift, transformers, unix, utf8-string
}:
mkDerivation {
  pname = "wayland-wire";
  version = "0.1.0";
  src = ./.;
  buildDepends = [
    base binary bytestring containers diet-set directory free hxt mtl
    network template-haskell th-lift transformers unix utf8-string
  ];
  testDepends = [
    base binary bytestring containers diet-set directory free hxt mtl
    network QuickCheck template-haskell th-lift transformers unix
    utf8-string
  ];
  buildTools = [ c2hs ];
  description = "Haskell implementation of the Wayland wire protocol";
  license = stdenv.lib.licenses.gpl3;
}
