#!/usr/bin/env bash
# q installer - sets up Racket + q dependencies
set -euo pipefail

Q_DIR="${Q_DIR:-$HOME/.q}"
RACKET_VERSION="${RACKET_VERSION:-8.10}"

echo "=== q installer ==="

# Check for Racket
if command -v racket &>/dev/null; then
    echo "✓ Racket $(racket --version | grep -oP '\d+\.\d+')"
else
    echo "Installing Racket ${RACKET_VERSION}..."
    if [[ "$(uname)" == "Darwin" ]]; then
        brew install --cask racket
    elif [[ "$(uname)" == "Linux" ]]; then
        curl -L "https://download.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-cs.sh" -o /tmp/racket-install.sh
        sudo sh /tmp/racket-install.sh --in-place --create-links /usr/local
        rm /tmp/racket-install.sh
    else
        echo "Unsupported OS. Please install Racket manually: https://racket-lang.org/install/"
        exit 1
    fi
fi

# Install q
echo "Installing q..."
if [[ -d "${Q_DIR}" ]]; then
    echo "Updating existing q installation at ${Q_DIR}"
    cd "${Q_DIR}" && git pull
else
    git clone https://github.com/coinerd/q.git "${Q_DIR}"
fi

cd "${Q_DIR}"
raco pkg install --auto --batch

# Add to PATH
echo ""
echo "Add q to your PATH:"
echo "  export PATH=\"${Q_DIR}:\$PATH\""
echo ""
echo "Or create an alias:"
echo "  alias q='racket ${Q_DIR}/main.rkt'"
echo ""
echo "Run 'racket main.rkt --help' to get started."
echo "Run 'racket main.rkt doctor' to check your setup."
