{ depot ? (import ../../../. {})
, pkgs ? depot.third_party.nixpkgs
, ... }:

let
  ignore = depot.third_party.gitignoreSource.gitignoreFilter ./.;
in import (pkgs.haskellPackages.haskellSrc2nix {
  name = "xanthous";
  src = builtins.path {
    name = "xanthous-source";
    path = ./.;
    filter = path: type: ignore path type
      || builtins.baseNameOf path == "package.yaml";
  };
  extraCabal2nixOptions = "--hpack";
})
