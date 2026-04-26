<!-- verified-against: 0.20.2 -->
# Installation Guide

<!-- This file is the canonical source. The docs/getting-started/installation.md copy is maintained for the doc site build. -->

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
raco q --version              # installed package
# From source tree instead: racket main.rkt --version
```

## First-Run Verification

After installing, confirm everything works:

```bash
raco q --version            # installed package; or: racket main.rkt --version
raco q doctor               # installed package; or: racket main.rkt doctor
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

> **From source tree** (without `raco pkg install`): prefix all commands with `racket main.rkt`.
> For example: `racket main.rkt --tui`, `racket main.rkt "hello"`.

## Upgrading

```bash
cd ~/.q
git pull
raco pkg install --auto   # in case dependencies changed
```

Or re-run the install script — it will detect the existing installation and pull the latest changes.

## Install from Tarball

Download the latest release tarball and install manually:

```bash
VERSION=$(curl -sL https://api.github.com/repos/coinerd/q/releases/latest | grep -oP '"tag_name": "v\K[^"]+')
curl -fsSL https://github.com/coinerd/q/releases/download/v${VERSION}/q-${VERSION}.tar.gz | tar xz
cd q-${VERSION}
raco pkg install --auto
```

See all releases at [github.com/coinerd/q/releases](https://github.com/coinerd/q/releases).

## Uninstalling

```bash
rm -rf ~/.q
```

Also remove any PATH entries or aliases you added to your shell config.

## Optional: Full TUI Support

For the enhanced TUI experience, install `tui-term` and `tui-ubuf`:

```bash
raco pkg install tui-term tui-ubuf
```

> **Note:** These packages have known compilation issues on Racket 8.10 (upstream bugs).
> The TUI works without them using built-in fallbacks. This is not required for normal use.

## Security Note

API keys stored in `~/.q/credentials.json` are saved in **plaintext** with owner-only permissions (`0600`). For production or CI environments, **use environment variables** instead of the credentials file:

```bash
export OPENAI_API_KEY=sk-...
export ANTHROPIC_API_KEY=sk-ant-...
export GEMINI_API_KEY=AIza...
```

This avoids writing secrets to disk and integrates with secret managers (Vault, AWS Secrets Manager, etc.). See the [Security section in README.md](../README.md#security) for details.

---

## Packaging Status

q is not yet published on a package registry. Install options:

- **One-command script** (recommended) — see Quick Install above
- **Manual clone** — see Manual Install above

Published packages (Racket catalog, Homebrew) are planned. Track progress in the [packaging roadmap](why-q.md#packaging-roadmap).
