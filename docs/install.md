# Installation Guide

Everything you need to get q running on your machine.

## Prerequisites

- **Racket 8.10+** — q is written in Racket and requires the Racket runtime.
  Download from [racket-lang.org](https://racket-lang.org) or install via your package manager.

## Quick Install (one command)

```bash
curl -fsSL https://raw.githubusercontent.com/coinerd/q/main/scripts/install.sh | bash
```

This script:

1. Checks for Racket — installs it automatically on macOS (Homebrew) and Linux (official installer).
2. Clones q into `~/.q` (or updates if already present).
3. Runs `raco pkg install --auto` to fetch dependencies.

Pass `Q_DIR` to customize the install location:

```bash
Q_DIR=~/projects/q curl -fsSL https://raw.githubusercontent.com/coinerd/q/main/scripts/install.sh | bash
```

## Manual Install

```bash
# 1. Install Racket — see https://racket-lang.org/install/

# 2. Clone and build
git clone https://github.com/coinerd/q.git
cd q
raco pkg install --auto

# 3. Verify
racket main.rkt --version
```

## First-Run Verification

After installing, confirm everything works:

```bash
racket main.rkt --version   # should print: q version 0.3.1
racket main.rkt doctor      # checks Racket version, dependencies, config
```

## Shell Setup

Add q to your shell so you can run it from anywhere.

**Option A — PATH** (recommended):

Add this to your `~/.bashrc`, `~/.zshrc`, or equivalent:

```bash
export PATH="$HOME/.q:$PATH"
```

Then reload: `source ~/.bashrc`

**Option B — Alias:**

```bash
echo "alias q='racket $HOME/.q/main.rkt'" >> ~/.bashrc
source ~/.bashrc
```

After setup you can run:

```bash
q --tui          # launch the terminal UI
q "hello"        # single-shot prompt
```

## Upgrading

```bash
cd ~/.q
git pull
raco pkg install --auto   # in case dependencies changed
```

Or re-run the install script — it will detect the existing installation and pull the latest changes.

## Uninstalling

```bash
rm -rf ~/.q
```

Also remove any PATH entries or aliases you added to your shell config.
