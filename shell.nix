{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865", withHoogle ? true }:
let
  inherit (nixpkgs) pkgs;

  xan = import ./xanthous.nix;

  packageSet = (
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler}
  );

  haskellPackages = (
    if withHoogle
    then packageSet.override {
      overrides = (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      });
    }
    else packageSet
  );

  drv = haskellPackages.callPackage xan {};

  inherit (pkgs.haskell.lib) addBuildTools;
in
(addBuildTools drv (with haskellPackages; [ cabal-install ])).env
