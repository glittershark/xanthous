{ nixpkgs ? import ./nixpkgs.nix {}
, pkgs ? nixpkgs.pkgs
, compiler ? "ghc865"
, withHoogle ? true
}:
let
  pkg = import ./pkg.nix { inherit pkgs; };

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
      } // (import ../../../third_party/haskell_overlay { inherit pkgs; })
        self super);
    }
    else packageSet
  );

  drv = pkgs.haskell.lib.doBenchmark (haskellPackages.callPackage pkg {});

  inherit (pkgs.haskell.lib) addBuildTools;
in
(addBuildTools drv (with haskellPackages; [
  cabal-install
  ghc-prof-flamegraph
  hp2pretty
])).env
