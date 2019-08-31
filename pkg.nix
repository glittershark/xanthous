{ nixpkgs ? import ./nixpkgs.nix {} }:
let inherit (nixpkgs) pkgs; in
import (pkgs.haskellPackages.haskellSrc2nix {
  name = "xanthous";
  src = ./.;
  extraCabal2nixOptions = "--hpack";
})
