# Credential Management

This document describes how q manages API keys and provider credentials,
including the backend abstraction layer and secure storage options.

## Overview

q uses a **pluggable credential backend system** that supports multiple
storage mechanisms. Credentials are resolved in a priority chain:

```
1. Environment variables  →  q --provider.api-key-env Q_OPENAI_API_KEY
2. OS keychain            →  secret-tool / macOS Keychain
3. Config file            →  ~/.q/credentials.json (plaintext, mode 0600)
4. Provider config        →  ~/.q/config.json (embedded)
```

The first backend that returns a valid credential wins.

## Backend Types

### Environment Variables (Recommended for CI/CD)

Set provider API keys via environment variables:

```bash
export Q_OPENAI_API_KEY="sk-..."
export Q_ANTHROPIC_API_KEY="sk-ant-..."
export Q_GEMINI_API_KEY="AI..."
```

Naming convention: `Q_<PROVIDER>_API_KEY` where provider is uppercased
and hyphens replaced with underscores.

- **Read-only** — cannot be modified via q commands
- **Always available** — works on all platforms
- **Best for** — CI/CD, containers, shared infrastructure

### OS Keychain (Recommended for Desktop)

On Linux systems with `libsecret` installed, q can store credentials in
the system keychain via `secret-tool`:

```bash
# Store a credential
q credentials set openai
# Prompts for the API key, stores in keychain

# Verify
q credentials get openai

# Remove
q credentials delete openai
```

On macOS, the Keychain is used via the `security` CLI.

- **Encrypted at rest** — uses OS-provided encryption
- **Best for** — developer desktops, personal machines

### File Backend (Default Fallback)

Credentials stored in `~/.q/credentials.json`:

```json
{
  "providers": {
    "openai": { "api-key": "sk-..." },
    "anthropic": { "api-key": "sk-ant-..." }
  }
}
```

File permissions are set to `0600` (owner read/write only).

- **Plaintext on disk** — keys are not encrypted at rest
- **Best for** — local development, testing, fallback

### Memory Backend (Testing Only)

Stores credentials in-memory for the duration of the process. Used by
the test suite and for programmatic access.

## Backend Abstraction API

All backends implement the same interface:

```racket
(require q/runtime/credential-backend)

;; Create backends
(define file-be    (make-file-credential-backend))
(define env-be     (make-env-credential-backend))
(define keychain-be (make-keychain-credential-backend))
(define mem-be     (make-memory-credential-backend))

;; Chain them (priority order: first match wins)
(define backend (make-chained-credential-backend
                  (list env-be keychain-be file-be)))

;; Operations
(backend-store! backend "openai" "sk-...")
(backend-load backend "openai")           ; → (hash 'api-key "sk-..." 'source "env")
(backend-delete! backend "openai")
(backend-list-providers backend)          ; → '("openai" "anthropic")
(backend-available? backend)              ; → #t
```

## Security Considerations

### Plaintext Storage Warning

The file backend stores API keys as **plaintext** in `~/.q/credentials.json`.
The file has restrictive permissions (`0600`), but:

- Any process running as your user can read it
- Keys appear in plaintext in backups
- Disk snapshots may contain the keys

**Recommendation:** Use environment variables or OS keychain for
production credentials.

### Credential Masking

q never logs or displays full API keys. The `mask-api-key` function
shows only the first 3 and last 4 characters:

```
sk-proj-...7k3d
sk-ant-...w8xm
```

### Scoped Access

Use `with-credential` for scoped access — the raw key is only available
within the body and is not retained:

```racket
(with-credential "openai" provider-config key
  (make-api-call key prompt))
```

## Configuration

### Setting the Default Backend

In `~/.q/config.json`:

```json
{
  "credential-backend": "keychain"
}
```

Options: `"keychain"`, `"file"`, `"env"`, `"auto"` (default — chained).

### Per-Provider Backend Override

```json
{
  "providers": {
    "openai": {
      "credential-backend": "keychain",
      "api-key-env": "OPENAI_API_KEY"
    }
  }
}
```

## CLI Commands

```bash
q credentials list                    # List all stored credentials (masked)
q credentials get <provider>          # Show credential source (not the key)
q credentials set <provider>          # Store a credential (prompts for key)
q credentials delete <provider>       # Remove a stored credential
q credentials backends                # Show available backends
q credentials test <provider>         # Verify a credential resolves correctly
```

## Troubleshooting

### "No writable backend available"

This error occurs when all backends in the chain are read-only (e.g.,
only the env backend is configured). Add a file or keychain backend.

### "secret-tool not available"

Install `libsecret-tools`:

```bash
# Debian/Ubuntu
sudo apt install libsecret-tools

# Fedora
sudo dnf install libsecret

# macOS — Keychain is built-in, no extra install needed
```

### Credential File Permissions

If you see a warning about credential file permissions:

```bash
chmod 600 ~/.q/credentials.json
```
