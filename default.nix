{ pkgs ? (import ../../../. {}).third_party
, lib ? pkgs.lib
, ...
}:
(pkgs.haskell.lib.failOnAllWarnings (
  pkgs.haskellPackages.callPackage (import ./pkg.nix { inherit pkgs; }) {}
)) // {
  # TODO(grfn): Get this passing (see https://buildkite.com/tvl/depot/builds/3055)
  meta.ci = false;
}
