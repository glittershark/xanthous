{ pkgs ? (import ../../../. {}).third_party }:

let
  ignore = pkgs.gitignoreSource.gitignoreFilter ./.;
in

import (pkgs.haskellPackages.haskellSrc2nix {
  name = "xanthous";
  src = builtins.path {
    name = "xanthous-source";
    path = ./.;
    filter = path: type: ignore path type
      || builtins.baseNameOf path == "package.yaml";
  };
  extraCabal2nixOptions = "--hpack";
})
