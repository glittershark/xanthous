{ pkgs ? (import ../../../. {}).third_party, ... }:

(pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  xanthous = pkgs.gitignoreSource ./.;
})).shellFor {
  packages = p: [p.xanthous];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghc-prof-flamegraph
    hp2pretty
    hlint
    pkgs.haskell-language-server.ghc884
  ];
}
