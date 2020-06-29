{ pkgs ? (import ../../../. {}).third_party }:

import (pkgs.haskellPackages.haskellSrc2nix {
  name = "xanthous";
  src = pkgs.gitignoreSource ./.;
  extraCabal2nixOptions = "--hpack";
})
