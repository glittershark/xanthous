{ nixpkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
, failOnWarnings ? false
}:
let
  inherit (nixpkgs) pkgs lib;
  inherit (lib) id;
  inherit (pkgs) fetchurl;
  all-hies = import (fetchTarball {
    url = "https://github.com/infinisil/all-hies/archive/4b6aab017cdf96a90641dc287437685675d598da.tar.gz";
    sha256 = "0ap12mbzk97zmxk42fk8vqacyvpxk29r2wrnjqpx4m2w9g7gfdya";
  }) {};
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
  xanthous =
    (if failOnWarnings then pkgs.haskell.lib.failOnAllWarnings else id)
      ((pkgs.haskellPackages
      .extend (import ./haskell-overlay.nix { inherit nixpkgs; })
    ).callPackage (import ./pkg.nix { inherit nixpkgs; }) {}); in
xanthous // { inherit hie; }
