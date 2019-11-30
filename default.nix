{ nixpkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
, failOnWarnings ? false
}:
let
  inherit (nixpkgs) pkgs lib;
  inherit (lib) id;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
  xanthous =
    (if failOnWarnings then pkgs.haskell.lib.failOnAllWarnings else id)
      ((pkgs.haskellPackages
      .extend (import ./haskell-overlay.nix { inherit nixpkgs; })
    ).callPackage (import ./pkg.nix { inherit nixpkgs; }) {}); in
xanthous // { inherit hie; }
