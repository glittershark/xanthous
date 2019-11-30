{ nixpkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865" }:
let
  inherit (nixpkgs) pkgs;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
  xanthous = (pkgs.haskellPackages
    .extend (import ./haskell-overlay.nix { inherit nixpkgs; }))
    .callPackage (import ./pkg.nix { inherit nixpkgs; }) {};
in
xanthous // { inherit hie; }
