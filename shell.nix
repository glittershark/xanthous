let
  depot = import ../../../. {};
  inherit (depot) third_party;
  pkgs = third_party.nixpkgs;
in

(pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  xanthous = third_party.gitignoreSource ./.;
})).shellFor {
  packages = p: [p.xanthous];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = (with pkgs.haskellPackages; [
    cabal-install
    ghc-prof-flamegraph
    hp2pretty
    hlint
    haskell-language-server
    cabal2nix
  ]) ++ (with pkgs; [
    qpdf
  ]);
}
