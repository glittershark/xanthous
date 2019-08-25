{ mkDerivation, ascii-art-to-unicode, base, brick, classy-prelude
, constraints, containers, data-default, deepseq, hpack, lens, mtl
, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck, vty
}:
let
  pkgs = import <nixpkgs> {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
in
mkDerivation {
  pname = "xanthous";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ascii-art-to-unicode base brick classy-prelude constraints
    containers data-default deepseq lens mtl QuickCheck vty
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    ascii-art-to-unicode base brick classy-prelude constraints
    containers data-default deepseq lens mtl QuickCheck tasty
    tasty-hunit tasty-quickcheck
  ];
  executableSystemDepends = [ hie pkgs.cabal-install ];
  preConfigure = "hpack";
  homepage = "https://github.com/glittershark/xanthous#readme";
  description = "A WIP TUI RPG";
  license = stdenv.lib.licenses.gpl3;
}
