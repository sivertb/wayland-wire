{ mkDerivation, base, binary, bytestring, containers, diet-set
, directory, free, hxt, mtl, network, network-msg, QuickCheck
, stdenv, template-haskell, th-lift, transformers, unix
, utf8-string
}:
mkDerivation {
  pname = "wayland-wire";
  version = "0.1.0";
  src = ./.;
  buildDepends = [
    base binary bytestring containers diet-set directory free hxt mtl
    network network-msg template-haskell th-lift transformers unix
    utf8-string
  ];
  testDepends = [
    base binary bytestring containers diet-set directory free hxt mtl
    network network-msg QuickCheck template-haskell th-lift
    transformers unix utf8-string
  ];
  description = "Haskell implementation of the Wayland wire protocol";
  license = stdenv.lib.licenses.gpl3;
}
