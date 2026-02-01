#!/usr/bin/env bash

set -euo pipefail

DIR="."
DRY_RUN=false

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Rename files from UPPERCASE to lowercase.

Options:
  --dir <dir>     Target directory (default: current directory)
  --dry-run       Show what would be renamed, without changing anything
  --help          Show this help message

Examples:
  $(basename "$0")
  $(basename "$0") --dir ./test
  $(basename "$0") --dir ./test --dry-run
EOF
}

# -------- argument parsing --------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dir)
            [[ $# -lt 2 ]] && { echo "Error: --dir requires an argument"; exit 1; }
            DIR="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# -------- validation --------
if [[ ! -d "$DIR" ]]; then
    echo "Error: directory does not exist: $DIR"
    exit 1
fi
DIR="${DIR%/}"

shopt -s nullglob

is_macos() {
    [[ "$(uname -s)" == "Darwin" ]]
}

exists_cs() {
    local path="$1"

    # Fast path: Linux & other case-sensitive systems
    if ! is_macos; then
        [[ -e "$path" ]]
        return
    fi

    # macOS safe path (case-sensitive check)
    local dir base
    dir="$(dirname "$path")"
    base="$(basename "$path")"

    [[ -d "$dir" ]] || return 1

    find "$dir" -maxdepth 1 -mindepth 1 -name "$base" -print -quit | grep -q .
}

# -------- rename logic --------
for f in "$DIR"/*; do
    base="$(basename "$f")"
    lower="$(printf '%s\n' "$base" | tr 'A-Z' 'a-z')"

    [[ "$base" == "$lower" ]] && continue

    target="$DIR/$lower"

    if exists_cs "$target" ; then
        echo "Skip (exists): $base -> $lower"
        continue
    fi

    if $DRY_RUN; then
        echo "[dry-run] $base -> $lower"
    else
        mv -- "$f" "$target"
        echo "Renamed: $base -> $lower"
    fi
done
