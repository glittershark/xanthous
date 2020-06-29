#!/usr/bin/env bash
set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1

exec nix-shell --run "$(nix-build -o dist/bin/hie hie.nix)/bin/hie $*"
