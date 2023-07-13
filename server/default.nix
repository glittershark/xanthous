args@{ depot ? import ../../../.. { }
, pkgs ? depot.third_party.nixpkgs
, ...
}:

depot.third_party.naersk.buildPackage {
  name = "xanthous-server";
  version = "0.0.1";
  src = depot.third_party.gitignoreSource ./.;

  # Workaround for a potential Nix bug related to restricted eval.
  # See https://github.com/nix-community/naersk/issues/169
  root = depot.nix.sparseTree {
    root = ./.;
    paths = [
      ./Cargo.toml
      ./Cargo.lock
    ];
  };

  passthru = {
    docker = import ./docker.nix args;
  };
}
