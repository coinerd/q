#!/usr/bin/env bash
set -euo pipefail

# Read version from info.rkt
VERSION=$(grep -oP '(?<=version ")[^"]+' info.rkt)
TARBALL_NAME="q-${VERSION}"
DIST_DIR="dist"

echo "Creating release tarball: ${TARBALL_NAME}.tar.gz"
mkdir -p "${DIST_DIR}"

# Create tarball from repo root, excluding dev files
git archive --format=tar.gz \
  --prefix="${TARBALL_NAME}/" \
  -o "${DIST_DIR}/${TARBALL_NAME}.tar.gz" \
  HEAD

echo "Created: ${DIST_DIR}/${TARBALL_NAME}.tar.gz"
ls -lh "${DIST_DIR}/${TARBALL_NAME}.tar.gz"
