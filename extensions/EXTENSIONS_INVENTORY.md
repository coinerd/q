# Q Extension Inventory

## Extension System Overview

Q has a rich extension system defined in `/home/user/src/q-agent/q/extensions/`. Extensions are Racket modules that provide an `extension` struct (via `define-q-extension`) with name, version, api-version, and hooks. They are discovered and loaded dynamically by `loader.rkt`, registered in a thread-safe registry (`api.rkt`), and governed by a tiered capability system (`tiers.rkt`) with five privilege levels: **hooks → commands → session → providers → tui**.

Each extension is defined using the `define-q-extension` macro from `define-extension.rkt` and can register hooks on specific lifecycle points (e.g., `register-tools`, `register-shortcuts`).

---

## Available Extensions

| # | Extension | File | Version | API Version | Tools | Commands | Tier |
|---|-----------|------|---------|-------------|-------|----------|------|
| 1 | **GSD Planning** | `gsd-planning.rkt` | 1.0.0 | 1 | 2 | 3 | commands |
| 2 | **GitHub Integration** | `github-integration.rkt` | 1.0.0 | 1 | 6 | 3 | commands |
| 3 | **Image Input** | `image-input.rkt` | 1.0.0 | 1 | 1 | — | hooks |
| 4 | **Racket Tooling** | `racket-tooling.rkt` | 1.0.0 | 1 | 3 | 3 | commands |
| 5 | **Remote Collaboration** | `remote-collab/remote-collab.rkt` | 1.0.0 | 1 | 1 | — | commands |
| 6 | **Session Export** | `session-export.rkt` | 1.0.0 | 1 | 1 | — | hooks |
| 7 | **Q Sync** | `q-sync.rkt` | 1.0.0 | 1 | 1 | — | commands |
| 8 | **Compact Context** | `compact-context.rkt` | 1.0.0 | 1 | 1 | — | hooks |

---

## Detailed Breakdown

### 1. GSD Planning Extension (`gsd-planning-extension`)
- **Purpose:** Supports the GSD (Get Stuff Done) workflow by managing planning artifacts in a `.planning/` directory.
- **Tools:**
  - `planning-read` — Read planning artifacts (PLAN, STATE, HANDOFF, VALIDATION, BUG_REPORT, etc.) from `.planning/`
  - `planning-write` — Write/update planning artifacts; auto-creates `.planning/` directory
- **Commands:** `/plan`, `/state`, `/handoff`
- **Hooks:** `register-tools`, `register-shortcuts`
- **Active:** ✅ Yes — provides `the-extension`, uses standard API v1, loaded by the extension loader

### 2. GitHub Integration Extension (`github-integration-extension`)
- **Purpose:** Wraps the `gh` CLI for GitHub project management within the GSD workflow.
- **Tools (6):**
  - `gh-issue` — Manage issues (create, close, update, get, list, close_tree)
  - `gh-pr` — Manage pull requests (create, merge, list, get)
  - `gh-milestone` — Manage milestones (create, close, list, create_from_spec)
  - `gh-board` — Project board operations (status, stale, autofix, verify, batch_set, reconfigure)
  - `gh-wave-start` — Start a GSD wave (create feature branch from main)
  - `gh-wave-finish` — Finish a GSD wave (commit, push, PR, merge, close issue, sync main)
- **Commands:** `/milestone`, `/issue`, `/pr`
- **Hooks:** `register-tools`, `register-shortcuts`
- **Runtime dependency:** Requires `gh` CLI installed; gracefully degrades if missing
- **Active:** ✅ Yes — provides `github-integration-extension` via `define-q-extension`

### 3. Image Input Extension (`image-input-extension`)
- **Purpose:** Multi-modal image support — encodes images to base64 and constructs multi-modal messages.
- **Tools:**
  - `image-input` — Actions: `encode` (base64), `message` (multi-modal message construction). Supports PNG, JPEG, GIF, WebP.
- **Commands:** None
- **Hooks:** `register-tools`
- **Active:** ✅ Yes — provides `the-extension`, uses standard API v1

### 4. Racket Tooling Extension (`racket-tooling-extension`)
- **Purpose:** Structural s-expression editing and validation for Racket source files.
- **Tools (3):**
  - `racket-check` — Validate Racket files (format, syntax/compile, test, expand modes)
  - `racket-edit` — Structural editing (modes: replace, form, skeleton, struct-add-field, provide-append, cond-insert-clause, match-insert-clause, rewrite-form). Auto-validates with `raco fmt` + `raco make`, reverts on failure.
  - `racket-codemod` — Pattern/template code transformation with `@@PLACEHOLDER` wildcards. Dry-run by default.
- **Commands:** `/fmt`, `/check`, `/expand`
- **Hooks:** `register-tools`, `register-shortcuts`
- **Runtime dependency:** Requires `raco` on PATH
- **Active:** ✅ Yes — provides `the-extension`, uses standard API v1

### 5. Remote Collaboration Extension (`remote-collab-extension`)
- **Purpose:** Control remote q agent instances via SSH + tmux.
- **Tools:**
  - `remote-q` — Actions: status, start, send, capture, wait, interrupt, stop
- **Commands:** None
- **Hooks:** `register-tools`
- **Active:** ✅ Yes — provides `the-extension`, uses standard API v1

### 6. Session Export Extension (`session-export-extension`)
- **Purpose:** Export session JSONL files to HTML, JSON, or Markdown.
- **Tools:**
  - `session-export` — Export session logs to HTML, JSON, or Markdown
- **Commands:** None
- **Hooks:** `register-tools`
- **Active:** ✅ Yes — provides `the-extension`, uses standard API v1

### 7. Q Sync Extension (`q-sync-extension`)
- **Purpose:** Sync project state between local and remote machines.
- **Tools:**
  - `q-sync` — Sync domains: planning, pi, scripts, git, all. Directions: push, pull, status, handoff
- **Commands:** None
- **Hooks:** `register-tools`
- **Active:** ✅ Yes — provides `the-extension`, uses standard API v1

### 8. Compact Context Extension (`compact-context-extension`)
- **Purpose:** Trigger context compaction with planning state preservation.
- **Tools:**
  - `compact-context` — Reads `.planning/` artifacts and injects them into compaction context
- **Commands:** None
- **Hooks:** `register-tools`
- **Active:** ✅ Yes — provides `compact-context-extension` via `define-q-extension`

---

## Supporting Infrastructure (Not Extensions)

These modules are part of the extension framework itself, not loadable extensions:

| Module | Role |
|--------|------|
| `api.rkt` | Extension struct, thread-safe registry (register/unregister/lookup/list) |
| `loader.rkt` | Discovery, dynamic loading, hot-reload, manifest validation, integrity checks |
| `manifest.rkt` | `qpm.json` manifest read/write/validate (qpm = Q Package Manager) |
| `define-extension.rkt` | `define-q-extension` macro DSL |
| `tiers.rkt` | 5-tier capability system (hooks → commands → session → providers → tui) |
| `context.rkt` | Extension execution context (tool registry, command registry, event bus access) |
| `dynamic-tools.rkt` | API for extensions to register/deregister tools at runtime |
| `ext-commands.rkt` | API for extensions to register slash commands |
| `hooks.rkt` | Hook dispatch mechanism |
| `quarantine.rkt` | Quarantine misbehaving extensions |
| `events.rkt` | Extension lifecycle events |
| `telemetry.rkt` | Performance timing/metrics for extension operations |
| `message-inject.rkt` | Message injection into agent conversation |
| `message-renderer.rkt` | Custom message rendering |
| `custom-renderer-registry.rkt` | Registry for custom renderers |
| `custom-ui-api.rkt` | Custom UI API for extensions |
| `dialog-api.rkt` | Dialog UI API |
| `widget-api.rkt` | Widget API for TUI extensions |
| `ui-channel.rkt` | UI communication channel |
| `resource-discovery.rkt` | Resource discovery for extensions |
| `ext-package-manager.rkt` | Extension package management |
| `package-audit.rkt` | Package auditing |
| `manifest-audit.rkt` | Manifest auditing |
| `test-harness.rkt` | Testing harness for extension development |

---

## Summary

- **8 loadable extensions** are available, all targeting **API version "1"**
- Total **16 tools** and **6 slash commands** are registered across all extensions
- The tier system ensures extensions only access capabilities they're authorized for
- Extensions support hot-reload, integrity verification (SHA-256), and compatibility checking
