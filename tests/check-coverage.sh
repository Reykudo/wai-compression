#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
cabal build wai-compression-tests:test:tests --offline -j1 --enable-coverage --ghc-options=-Werror
binary=$(cabal list-bin wai-compression-tests:test:tests --offline --enable-coverage)
temporary=$(mktemp -d)
trap 'rm -rf "$temporary"' EXIT
HPCTIXFILE="$temporary/tests.tix" "$binary" "$@"
directories=()
while IFS= read -r directory; do
  directories+=("--hpcdir=$directory")
done < <(find dist-newstyle -type d -path '*/hpc/vanilla/mix')
hpc report "$temporary/tests.tix" "${directories[@]}" --exclude=Main
