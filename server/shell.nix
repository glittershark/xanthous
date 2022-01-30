let
  depot = import ../../../.. { };
  pkgs = depot.third_party.nixpkgs;
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    rustup
    rust-analyzer
  ];
}
