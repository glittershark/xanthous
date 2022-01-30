args@{ depot ? import ../../../.. { }
, pkgs ? depot.third_party.nixpkgs
, ...
}:

depot.third_party.naersk.buildPackage {
  name = "xanthous-server";
  version = "0.0.1";
  src = depot.third_party.gitignoreSource ./.;
  passthru = {
    docker = import ./docker.nix args;
  };
}
