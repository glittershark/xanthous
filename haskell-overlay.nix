{ nixpkgs ? import ./nixpkgs.nix {} }:
let inherit (nixpkgs) pkgs;
in self: super: rec {
  generic-arbitrary = pkgs.haskell.lib.appendPatch
    super.generic-arbitrary
    [ ./build/generic-arbitrary-export-garbitrary.patch ];
}
