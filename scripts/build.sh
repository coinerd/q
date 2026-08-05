#!/bin/bash
# scripts/build.sh — Compile q into q/build/q
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$PROJECT_DIR/build"
BINARY="$BUILD_DIR/q"

echo "=== Building q ==="

# 1. Pre-compile all bytecode (validates the source tree)
echo "[1/3] Pre-compiling bytecode..."
cd "$PROJECT_DIR"
racket -l raco make main.rkt

# 2. Create standalone executable
echo "[2/3] Creating standalone binary..."
racket -l raco exe -o "$BINARY" main.rkt

# 3. Verify
echo "[3/3] Verifying binary..."
"$BINARY" "What is 2+2?" --model deepseek-v4-flash 2>&1 | head -1

echo ""
echo "=== Build complete ==="
echo "Binary: $BINARY"
ls -lh "$BINARY"
echo ""
echo "To run: $BINARY --tui --auto-approve --model deepseek-v4-flash"
