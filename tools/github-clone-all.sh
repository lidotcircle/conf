#!/usr/bin/env bash
set -euo pipefail

SCRIPT_NAME="$(basename "$0")"

print_help() {
  cat <<EOF
Usage:
  $SCRIPT_NAME -u USERNAME -t TOKEN [options]

Options:
  -u, --user USERNAME        GitHub username (required)
  -t, --token TOKEN          GitHub personal access token (required)
  -d, --dir DIRECTORY        Target directory (default: USERNAME-repos)
  --https                    Use HTTPS clone URLs (default: SSH)
  --bare                     bare repo
  -h, --help                 Show this help message

Examples:
  $SCRIPT_NAME -u alice -t ghp_xxxxx
  $SCRIPT_NAME --user alice --token ghp_xxxxx --https
  $SCRIPT_NAME -u alice -t ghp_xxxxx -d backup

Notes:
  - Token needs 'repo' scope for private repos
  - SSH requires configured SSH keys
  - jq is required
EOF
}

# --------------------
# Defaults
# --------------------
CLONE_MODE="ssh"
TARGET_DIR=""
GITHUB_USER=""
GITHUB_TOKEN=""
BARE_CLONE=""

# --------------------
# Argument parsing
# --------------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    -u|--user)
      GITHUB_USER="$2"
      shift 2
      ;;
    --bare)
      BARE_CLONE="--bare"
      shift 1
      ;;
    -t|--token)
      GITHUB_TOKEN="$2"
      shift 2
      ;;
    -d|--dir)
      TARGET_DIR="$2"
      shift 2
      ;;
    --https)
      CLONE_MODE="https"
      shift
      ;;
    -h|--help)
      print_help
      exit 0
      ;;
    *)
      echo "❌ Unknown argument: $1"
      echo
      print_help
      exit 1
      ;;
  esac
done

# --------------------
# Validation
# --------------------
if [[ -z "$GITHUB_USER" || -z "$GITHUB_TOKEN" ]]; then
  echo "❌ Error: --user and --token are required"
  echo
  print_help
  exit 1
fi

command -v jq >/dev/null 2>&1 || {
  echo "❌ jq is required but not installed"
  exit 1
}

TARGET_DIR="${TARGET_DIR:-${GITHUB_USER}-repos}"

mkdir -p "$TARGET_DIR"
cd "$TARGET_DIR"

# --------------------
# Select URL field (FIXED)
# --------------------
if [[ "$CLONE_MODE" == "https" ]]; then
  URL_FIELD="https_url"
else
  URL_FIELD="ssh_url"
fi

# --------------------
# Fetch + clone repos
# --------------------
PAGE=1
PER_PAGE=100

echo "📦 Downloading repositories for user: $GITHUB_USER"
echo "📁 Target directory: $TARGET_DIR"
echo "🔐 Clone mode: $CLONE_MODE"
echo

while :; do
  RESPONSE=$(curl -fsSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github+json" \
    "https://api.github.com/user/repos?per_page=$PER_PAGE&page=$PAGE")

  COUNT=$(echo "$RESPONSE" | jq length)
  [[ "$COUNT" -eq 0 ]] && break

  echo "$RESPONSE" | jq -r ".[].$URL_FIELD" | while read -r repo; do
    name="$(basename "$repo" .git)"

    if [[ -d "$name/.git" ]]; then
      echo "🔄 Updating $name"
      (cd "$name" && git pull --ff-only)
    else
      echo "⬇️  Cloning $name"
      git clone $BARE_CLONE "$repo"
    fi
  done

  ((PAGE++))
done

echo
echo "✅ Done. All repositories processed."
