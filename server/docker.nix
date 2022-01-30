{ depot ? import ../../../.. { }
, pkgs ? depot.third_party.nixpkgs
, ...
}:

let
  inherit (depot.users.grfn) xanthous;
  xanthous-server = xanthous.server;
in
pkgs.dockerTools.buildLayeredImage {
  name = "xanthous-server";
  tag = "latest";
  contents = [ xanthous xanthous-server ];
  config = {
    Cmd = [
      "${xanthous-server}/bin/xanthous-server"
      "--xanthous-binary-path"
      "${xanthous}/bin/xanthous"
    ];
  };
}
