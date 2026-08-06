#!/bin/bash
# scripts/build.sh — Pre-compile q bytecode into q/build/
#
# Racket loads bytecode from compiled/ subdirectories alongside .rkt files.
# raco make pre-compiles everything so subsequent runs skip compilation.
# The launcher at q/build/q just invokes racket from the project root.
#
# Why not raco exe? raco exe creates a standalone binary that cannot
# load extensions via dynamic-require (the embedded resolver doesn't
# consult filesystem paths). Extensions like gsd-planning require
# filesystem-accessible modules.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$PROJECT_DIR/build"
LAUNCHER="$BUILD_DIR/q"

echo "=== Building q (pre-compiled bytecode) ==="

# 1. Pre-compile all bytecode
#    Creates compiled/ subdirectories next to each .rkt file.
#    Running `racket main.rkt` afterward uses these .zo files.
echo "[1/2] Pre-compiling bytecode..."
cd "$PROJECT_DIR"
racket -l raco make main.rkt
# GSD extension is dynamically loaded — compile explicitly
echo "       Compiling GSD extension modules..."
racket -l raco make extensions/gsd/*.rkt 2>/dev/null
echo "       Done. Bytecode written to compiled/ directories."

# 2. Create launcher script
#    Runs racket from the project root directory.
#    When /go modifies q/ files, the running process is unaffected
#    because Racket loaded bytecode into memory at startup.
echo "[2/2] Creating launcher..."
mkdir -p "$BUILD_DIR"
cat > "$LAUNCHER" << 'LAUNCHEREOF'
#!/bin/bash
# q launcher — runs racket from project root with pre-compiled bytecode
DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT="$(cd "$DIR/../.." && pwd)"
cd "$PROJECT"
exec racket q/main.rkt "$@"
LAUNCHEREOF
chmod +x "$LAUNCHER"

echo ""
echo "=== Build complete ==="
echo "Launcher: $LAUNCHER"
echo ""
echo "To run: $LAUNCHER --tui --auto-approve --model deepseek-v4-flash"
