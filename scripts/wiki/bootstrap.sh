#!/usr/bin/env bash
set -euo pipefail

WIKI_REMOTE="${WIKI_REMOTE:-https://github.com/coinerd/q.wiki.git}"
WORKDIR="${WORKDIR:-.tmp/q-wiki}"
SRC_DIR="${SRC_DIR:-wiki-src}"
COMMIT_MSG="${COMMIT_MSG:-docs(wiki): refresh wiki pages}"

if ! command -v git >/dev/null 2>&1; then
  echo "git is required" >&2
  exit 1
fi

if [ ! -d "$SRC_DIR" ]; then
  echo "Missing source directory: $SRC_DIR" >&2
  exit 1
fi

mkdir -p "$(dirname "$WORKDIR")"

if [ -d "$WORKDIR/.git" ]; then
  git -C "$WORKDIR" fetch origin
  git -C "$WORKDIR" reset --hard origin/HEAD
else
  rm -rf "$WORKDIR"
  git clone "$WIKI_REMOTE" "$WORKDIR"
fi

# Ensure git identity is configured in the wiki checkout
cd "$WORKDIR"
git config user.email "coinerd@users.noreply.github.com"
git config user.name "coinerd"

rsync -av --delete \
  --exclude '.git' \
  "$SRC_DIR"/ "$WORKDIR"/

if git diff --quiet && git diff --cached --quiet; then
  echo "No wiki changes."
  exit 0
fi

git add .
git commit -m "$COMMIT_MSG"
git push origin HEAD
