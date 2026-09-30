#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$project_root"
cabal_arguments=("$@")
package_directories=(
  src/wai-compression
  src/wai-compression-zlib
  src/wai-compression-brotli
  src/wai-compression-zstd
  tests
  example
  benchmarks
)

for directory in "${package_directories[@]}"; do
  printf '\nChecking package metadata: %s\n' "$directory"
  (cd "$directory" && cabal check)
  cmp LICENSE "$directory/LICENSE"
done

if command -v fourmolu >/dev/null 2>&1; then
  haskell_files=()
  while IFS= read -r -d '' source_file; do
    haskell_files+=("$source_file")
  done < <(find src tests example benchmarks -name '*.hs' -print0)
  fourmolu --mode check "${haskell_files[@]}"
fi

temporary=$(mktemp -d)
trap 'rm -rf "$temporary"' EXIT
mkdir -p "$temporary/archives" "$temporary/source"
cabal sdist all --output-directory="$temporary/archives"

archive_count=0
for archive in "$temporary/archives"/*.tar.gz; do
  archive_count=$((archive_count + 1))
  printf '\nChecking source archive: %s\n' "$(basename "$archive")"
  tar -tzf "$archive" > "$temporary/entries"
  while IFS= read -r entry; do
    case "$entry" in
      *.md)
        case "${entry##*/}" in
          README.md|readme.md) ;;
          *) printf 'Unexpected Markdown in archive: %s\n' "$entry" >&2; exit 1 ;;
        esac
        ;;
      */dist-newstyle/*|*/.direnv/*|*/.git/*|*/.codex/*|*/.agents/*|*/.serena/*|*/.claude/*|*/.gemini/*|*/.opencode/*|*.o|*.hi|*.dyn_o|*.dyn_hi|*.tix|*.hp|*.prof|*.log|*/results.csv|*/corpus-manifest.json)
        printf 'Unexpected artifact in archive: %s\n' "$entry" >&2
        exit 1
        ;;
    esac
  done < "$temporary/entries"
  tar -xzf "$archive" -C "$temporary/source"
done
if [[ "$archive_count" -ne "${#package_directories[@]}" ]]; then
  printf 'Expected %s source archives, found %s\n' "${#package_directories[@]}" "$archive_count" >&2
  exit 1
fi

printf 'packages: */*.cabal\ntests: True\njobs: 1\n' > "$temporary/source/cabal.project"
(
  cd "$temporary/source"
  printf '\nBuilding extracted release sources\n'
  cabal build all "${cabal_arguments[@]}" -j1 --ghc-options=-Werror
  cabal test all "${cabal_arguments[@]}" -j1 --ghc-options=-Werror \
    --test-show-details=direct --test-options='--quickcheck-tests=1000 +RTS -N2 -RTS'
  cabal haddock all "${cabal_arguments[@]}" -j1 --ghc-options=-Werror \
    --haddock-hyperlink-source
)

printf '\nRelease checks passed for all %s packages.\n' "$archive_count"
