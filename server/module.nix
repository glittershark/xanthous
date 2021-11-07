{ config, lib, pkgs, depot, ... }:

let
  cfg = config.services.xanthous-server;
in {
  options = with lib; {
    services.xanthous-server = {
      enable = mkEnableOption "xanthous server";

      port = mkOption {
        type = types.int;
        default = 2222;
        description = "Port to listen to for SSH connections";
      };

      metricsPort = mkOption {
        type = types.int;
        default = 9000;
        description = "Port to listen to for prometheus metrics";
      };

      image = mkOption {
        type = types.package;
        default = depot.users.grfn.xanthous.server.docker;
        description = "OCI image file to run";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.oci-containers.containers."xanthous-server" = {
      autoStart = true;
      image = "${cfg.image.imageName}:${cfg.image.imageTag}";
      imageFile = cfg.image;
      ports = [
        "${toString cfg.port}:22"
        "${toString cfg.metricsPort}:9000"
      ];
    };
  };
}
