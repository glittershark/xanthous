let
  depot = import ../../../. { };
  inherit (depot) third_party;
  pkgs = third_party.nixpkgs;
in

(pkgs.haskell.packages.ghc8107.extend (pkgs.haskell.lib.packageSourceOverrides {
  xanthous = third_party.gitignoreSource ./.;
})).shellFor {
  packages = p: [ p.xanthous ];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = (with pkgs.haskell.packages.ghc8107; [
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
