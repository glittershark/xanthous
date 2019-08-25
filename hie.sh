#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

argv=( "$@"  )
argv=( "${argv[@]/\'/\'\\\'\'}"  )
argv=( "${argv[@]/#/\'}"  )
argv=( "${argv[@]/%/\'}"  )

exec nix-shell --pure --run "exec $(nix-build -o dist/nix/hie -A hie)/bin/hie ${argv[*]}"
