{ pkgs ? (import ../../../. {}).third_party
, lib ? pkgs.lib
, ...
}:
pkgs.haskell.lib.failOnAllWarnings (
  pkgs.haskellPackages.callPackage (import ./pkg.nix { inherit pkgs; }) {}
)
