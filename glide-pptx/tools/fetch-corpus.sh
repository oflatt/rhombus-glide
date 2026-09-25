#!/usr/bin/env bash
# Downloads a corpus of real .pptx files to test against.
#
# Two projects' regression suites, both written by people fixing bugs in their
# own readers: LibreOffice's is the larger, and Apache POI's overlaps little
# with it because the two hit different corners. They find far more than
# anything we would write ourselves -- an arrowhead we never drew, a path space
# of zero, a donut whose ring was thicker than the shape.
#
# The files are not committed: they belong to those projects, and
# `tests/corpus/` is ignored.
#
# Usage: tools/fetch-corpus.sh [count-per-source]
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
out="$here/tests/corpus"
count="${1:-400}"

mkdir -p "$out"

fetch() {
  local repo="$1" path="$2" prefix="$3"
  echo "listing $repo/$path ..."
  local names
  names=$(gh api "repos/$repo/contents/$path" --paginate \
            --jq '.[] | select(.name|test("[.]pptx$")) | .name' | head -n "$count")
  local n
  n=$(printf '%s\n' "$names" | grep -c . || true)
  echo "  $n decks"
  # Prefixed, because the two suites use some of the same names.
  printf '%s\n' "$names" | xargs -P 12 -I {} sh -c \
    'test -s "'"$out"'/'"$prefix"'{}" || curl -sSL --retry 2 -o "'"$out"'/'"$prefix"'{}" \
       "https://raw.githubusercontent.com/'"$repo"'/master/'"$path"'/{}"'
}

fetch "LibreOffice/core" "sd/qa/unit/data/pptx" ""
fetch "apache/poi" "test-data/slideshow" "poi-"

echo "$(ls -1 "$out"/*.pptx 2>/dev/null | wc -l | tr -d ' ') decks present"
