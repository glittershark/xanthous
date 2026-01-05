let
  pkgs = import ./nix/nixpkgs.nix { };
  gitignoreSource = import (import ./nix/sources.nix { })."gitignore.nix" { };

in (pkgs.haskell.packages.ghc8107.extend
  (pkgs.haskell.lib.packageSourceOverrides {
    xanthous = gitignoreSource.gitignoreSource ./.;
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
    ]) ++ (with pkgs; [ qpdf ]);
  }
