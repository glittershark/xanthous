{ pkgs ? (import ../../../. {}).third_party }:

let
  all-hies = (fetchTarball {
    url = "https://github.com/infinisil/all-hies/archive/eff5d9a5e1a84150014095494331cf63e59923af.tar.gz";
    sha256 = "19bws9fyjhgiikig86cri05fxz1wrz60n69zrigq5wzbyn4hwv9h";
  });

  sources = import "${all-hies}/sources.nix";
  build = import "${all-hies}/build.nix";

in (build {
  glibcName = pkgs.glibc.name;
  inherit sources;
  ghcVersion = "8.8.3";
}).combined
