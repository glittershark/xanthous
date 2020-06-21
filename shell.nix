{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "ghc865", withHoogle ? true }:
let
  inherit (nixpkgs) pkgs;

  pkg = import ./pkg.nix { inherit nixpkgs; };

  packageSet = (
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler}
  ).override {
    overrides = import ./haskell-overlay.nix { inherit nixpkgs; };
  };

  haskellPackages = (
    if withHoogle
    then packageSet.override {
      overrides = (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      } // (import ./haskell-overlay.nix { inherit nixpkgs; }) self super);
    }
    else packageSet
  );

  drv = haskellPackages.callPackage pkg {};

  inherit (pkgs.haskell.lib) addBuildTools;
in
(addBuildTools drv (with haskellPackages; [
  cabal-install
  ghc-prof-flamegraph
])).env
