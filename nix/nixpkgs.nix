{ ... }:
let sources = import ./sources.nix { };
in import sources.nixpkgs { config.allowBroken = true; }
