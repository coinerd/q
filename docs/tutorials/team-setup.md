# Team Setup and Onboarding Guide

How to set up q for a team — shared configs, credentials, extensions, and onboarding new members.

---

## Table of Contents

1. [Quick Start for New Team Members](#quick-start-for-new-team-members)
2. [Configuration Architecture](#configuration-architecture)
3. [Global vs Project Config](#global-vs-project-config)
4. [Shared Credentials](#shared-credentials)
5. [Team Extensions](#team-extensions)
6. [Project Instructions](#project-instructions)
7. [CI/CD Integration](#cicd-integration)
8. [Onboarding Checklist](#onboarding-checklist)
9. [Troubleshooting](#troubleshooting)

---

## Quick Start for New Team Members

Get running in under 5 minutes:

```bash
# 1. Install Racket 8.10+
# macOS:
brew install racket
# Linux:
curl -fsSL https://download.racket-lang.org/installers/8.10/racket-8.10-x86_64-linux-cs.sh | sh

# 2. Install q
raco pkg install --auto q

# 3. Set your API key (team will tell you which provider)
export OPENAI_API_KEY="sk-..."
# or
export ANTHROPIC_API_KEY="sk-ant-..."

# 4. Clone the project
git clone <your-team-repo>
cd <project>

# 5. Run q
raco q --tui
```

That's it. The project's `.q/` directory contains team-shared config that q picks up automatically.

---

## Configuration Architecture

q uses a layered configuration system. Settings at higher priority override those below:

| Priority | Source | Scope |
|----------|--------|-------|
| 1 (highest) | CLI arguments | Current invocation |
| 2 | Environment variables | Current process |
| 3 | Project credentials (`.q/credentials.json`) | Project |
| 4 | Global credentials (`~/.q/credentials.json`) | All projects |
| 5 | Project config (`.q/config.json`) | Project |
| 6 | Global config (`~/.q/config.json`) | All projects |
| 7 | Built-in defaults | Everywhere |

**Team strategy**: Put shared defaults in project config. Let individuals override with global config or environment variables.

---

## Global vs Project Config

### Global Config: `~/.q/config.json`

Personal settings that apply to every project:

```json
{
  "default-provider": "openai",
  "default-model": "gpt-4o",
  "session-dir": "~/.q/sessions",
  "providers": {
    "openai": {
      "base-url": "https://api.openai.com/v1",
      "api-key-env": "OPENAI_API_KEY",
      "models": ["gpt-4o", "gpt-4-turbo", "gpt-3.5-turbo"],
      "default-model": "gpt-4o"
    },
    "anthropic": {
      "base-url": "https://api.anthropic.com",
      "api-key-env": "ANTHROPIC_API_KEY",
      "models": ["claude-3-opus-20240229", "claude-3-sonnet-20240229"]
    }
  }
}
```

### Project Config: `.q/config.json`

Team-shared settings committed to the repo:

```json
{
  "default-model": "openai/gpt-4o",
  "providers": {
    "openai": {
      "base-url": "https://api.openai.com/v1",
      "api-key-env": "OPENAI_API_KEY",
      "models": ["gpt-4o"]
    }
  }
}
```

**Key points**:

- Use `api-key-env` instead of `api-key` in project config. API keys should never be committed.
- Project config overrides global config for team consistency.
- If `.q/config.json` is absent, q checks `.pi/config.json` for backward compatibility.

### Setting Up a New Project

```bash
# Create the .q directory
mkdir -p .q

# Create project config
cat > .q/config.json << 'EOF'
{
  "default-provider": "openai",
  "default-model": "gpt-4o",
  "providers": {
    "openai": {
      "base-url": "https://api.openai.com/v1",
      "api-key-env": "OPENAI_API_KEY",
      "models": ["gpt-4o", "gpt-3.5-turbo"]
    }
  }
}
EOF

# Create project instructions
cat > .q/instructions.md << 'EOF'
This project uses TypeScript with Jest. Follow existing patterns.
Run tests with: npm test
EOF

# Commit (no secrets!)
echo ".q/credentials.json" >> .gitignore
git add .q/config.json .q/instructions.md
```

---

## Shared Credentials

### Credential File: `~/.q/credentials.json`

Each team member stores their own API keys locally:

```json
{
  "providers": {
    "openai": {
      "api-key": "sk-your-key-here"
    },
    "anthropic": {
      "api-key": "sk-ant-your-key-here"
    }
  }
}
```

**Rules**:

- **Never commit** `credentials.json`. Add to `.gitignore`.
- Use `api-key-env` in config to reference environment variables instead.
- For CI/CD, use environment variables exclusively.

### Setting Up Credentials

```bash
# Create credentials directory
mkdir -p ~/.q

# Create credentials file
cat > ~/.q/credentials.json << 'EOF'
{
  "providers": {
    "openai": {
      "api-key": "sk-..."
    }
  }
}
EOF

# Restrict permissions
chmod 600 ~/.q/credentials.json
```

### Provider-Specific Key Formats

| Provider | Key Prefix | Example |
|----------|------------|---------|
| OpenAI | `sk-` | `sk-proj-abc123...` |
| Anthropic | `sk-ant-` | `sk-ant-api03-xyz...` |
| Other | Any non-empty | `my-custom-key` |

q validates these formats and warns on mismatches.

---

## Team Extensions

Extensions let you hook into q's agent lifecycle — adding logging, enforcing policies, injecting context, or blocking unsafe operations.

### Extension Directory Layout

```
.q/
├── config.json
├── instructions.md
└── extensions/
    └── team-policy/
        ├── qpm.json          # manifest
        └── policy.rkt        # extension code
```

### Example: Team Policy Extension

A simple extension that blocks commands matching a deny-list:

```racket
#lang racket/base

;; .q/extensions/team-policy/policy.rkt
;; Blocks dangerous bash commands in the team project.

(require q/extensions/define-extension)

(define denied-commands '("rm -rf /" "DROP TABLE" "mkfs"))

(define (check-tool-call ctx)
  ;; Extract the tool call from context
  (define tool-name (hash-ref ctx 'tool-name #f))
  (define args (hash-ref ctx 'arguments #f))
  (when (and (equal? tool-name "bash")
             args)
    (define cmd (hash-ref args 'command ""))
    (for ([denied (in-list denied-commands)])
      (when (string-contains? cmd denied)
        (raise (hash 'action "block"
                     'reason (string-append
                               "Blocked by team policy: "
                               denied))))))
  ;; Pass through if no match
  (hash 'action "pass"))

(define-q-extension team-policy
  #:version "1.0.0"
  #:api-version "1"
  #:on tool-call check-tool-call)
```

### Extension Manifest

Every extension has a `qpm.json` manifest:

```json
{
  "name": "team-policy",
  "version": "1.0.0",
  "api-version": "1",
  "type": "extension",
  "description": "Blocks dangerous commands per team policy",
  "author": "your-team",
  "entry": "policy.rkt",
  "files": ["policy.rkt"]
}
```

### Sharing Extensions Across the Team

Commit the extension directory to the repo:

```bash
# Create extension
mkdir -p .q/extensions/team-policy

# Write code and manifest (see above)
# ...

# Commit
git add .q/extensions/team-policy/
git commit -m "feat: add team policy extension"
```

Every team member gets the extension automatically when they clone or pull.

---

## Project Instructions

The `.q/instructions.md` file provides project-specific context to the agent. Think of it as a `.editorconfig` for AI assistance.

### Example: Web Application Project

```markdown
# Project Instructions

## Tech Stack
- Backend: Python 3.11 with FastAPI
- Frontend: React 18 with TypeScript
- Database: PostgreSQL 15
- Tests: pytest (backend), Jest (frontend)

## Conventions
- Follow PEP 8 for Python code
- Use functional components with hooks in React
- All API endpoints must have OpenAPI docs
- Write tests for every new endpoint

## Commands
- Run all tests: `make test`
- Lint: `make lint`
- Start dev server: `make dev`
- Database migrations: `make migrate`

## Architecture
- src/api/ — FastAPI routes
- src/models/ — SQLAlchemy models
- src/services/ — Business logic
- frontend/src/ — React components

## Do NOT
- Never commit .env files
- Never modify migration files after merge
- Never use print() — use structlog
```

This file is automatically loaded when q starts in the project directory.

---

## CI/CD Integration

### Environment Variables Only

In CI, use environment variables for credentials — never files:

```yaml
# GitHub Actions example
- name: Run q
  env:
    OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}
  run: |
    raco q --model gpt-4o "Review the changed files for bugs"
```

### JSON Mode for Automation

Use JSON mode for machine-readable output:

```bash
# Pipe-friendly JSON output
echo '{"prompt": "Summarize this code"}' | raco q --json
```

### Non-Interactive Flags

```bash
# Single-shot, no interaction
raco q --model gpt-4o --max-turns 1 "explain main.rkt"

# With session for context
raco q --session $SESSION_ID "continue the refactoring"
```

---

## Onboarding Checklist

Use this checklist when onboarding a new team member:

- [ ] **Racket installed** — version 8.10 or later
  ```bash
  racket --version
  ```
- [ ] **q installed**
  ```bash
  raco pkg install --auto q
  ```
- [ ] **API key configured**
  ```bash
  # Option A: credentials file
  echo '{"providers":{"openai":{"api-key":"sk-..."}}}' > ~/.q/credentials.json
  chmod 600 ~/.q/credentials.json

  # Option B: environment variable (add to .bashrc/.zshrc)
  export OPENAI_API_KEY="sk-..."
  ```
- [ ] **Project cloned** and `.q/` config present
  ```bash
  git clone <repo> && cd <repo>
  ls .q/config.json  # should exist
  ```
- [ ] **Verify q works**
  ```bash
  raco q "Hello, what tools do you have?"
  ```
- [ ] **Extensions loaded** (if project has them)
  ```bash
  # Check extensions directory
  ls .q/extensions/
  ```
- [ ] **Read project instructions**
  ```bash
  cat .q/instructions.md
  ```

---

## Troubleshooting

### "No provider configured"

q can't find an API key. Check:
1. `~/.q/credentials.json` exists and has the right provider
2. Environment variable is set (`echo $OPENAI_API_KEY`)
3. `.q/config.json` references the right `api-key-env`

### "Invalid API key format"

The key doesn't match the expected prefix:
- OpenAI keys start with `sk-`
- Anthropic keys start with `sk-ant-`

### "Mock provider active"

q fell back to the mock provider (no real API calls). This means no valid credentials were found. See "No provider configured" above.

### Project config not picked up

- Make sure you're running q from the project root (where `.q/` lives)
- Check that `.q/config.json` is valid JSON: `python3 -c "import json; json.load(open('.q/config.json'))"`

### Extension not loading

- Check the extension directory is under `.q/extensions/`
- Verify `qpm.json` has `"type": "extension"` and a valid `"entry"` path
- Run `raco q --doctor` to diagnose configuration issues

### Permission denied on credentials

```bash
chmod 600 ~/.q/credentials.json
```

The credentials file should be readable only by your user.

---

## See Also

- [Installation Guide](install.md) — Full installation instructions
- [Builder Tutorials](tutorials/builder-tutorials.md) — Build custom tools, providers, and extensions
- [Configuration Reference](../docs/getting-started/configuration.md) — Complete config schema
- [Architecture Overview](../adr/0001-small-trusted-core.md) — How q works internally
