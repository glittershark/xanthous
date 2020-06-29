args:
let pkgs = (import ../../../. args).third_party;
in pkgs // { inherit pkgs; }
