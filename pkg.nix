{ pkgs ? import ./nixpkgs.nix {} }:
let
  inherit (builtins) filterSource elem not;
  gitignoreSource = (import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
    # date = 2019-09-18T15:15:15+02:00;
  }) { inherit (pkgs) lib; }).gitignoreSource;
in
import (pkgs.haskellPackages.haskellSrc2nix {
  name = "xanthous";
  src = gitignoreSource ./.;
  extraCabal2nixOptions = "--hpack";
})
