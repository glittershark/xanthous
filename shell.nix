{ pkgs ? (import ../../../. {}).third_party, ... }:

(pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  xanthous = pkgs.gitignoreSource ./.;
})).shellFor {
  packages = p: [p.xanthous];
  withHoogle = true;
  buildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghc-prof-flamegraph
    hp2pretty
  ];

  nativeBuildInputs = [
    (import ./hie.nix { inherit pkgs; })
  ];
}
