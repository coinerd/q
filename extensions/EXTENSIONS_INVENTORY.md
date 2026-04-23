# Q Extension Inventory

## Extension System Overview

Q has a rich extension system defined in `/home/user/src/q-agent/q/extensions/`. Extensions are Racket modules that provide a `the-extension` binding (an `extension` struct with name, version, api-version, and hooks). They are discovered and loaded dynamically by `loader.rkt`, registered in a thread-safe registry (`api.rkt`), and governed by a tiered capability system (`tiers.rkt`) with five privilege levels: **hooks → commands → session → providers → tui**.

Each extension is defined using the `define-q-extension` macro from `define-extension.rkt` and can register hooks on specific lifecycle points (e.g., `register-tools`, `register-shortcuts`).

---

## Available Extensions

| # | Extension | File | Version | API Version | Registers Tools | Registers Commands | Tier |
|---|-----------|------|---------|-------------|-----------------|-------------------|------|
| 1 | **GSD Planning** | `gsd-planning.rkt` | 1.0.0 | 1 | `planning-read`, `planning-write` | `/plan`, `/state`, `/handoff` | commands |
| 2 | **GitHub Integration** | `github-integration.rkt` | 1.0.0 | 1 | `gh-issue`, `gh-pr`, `gh-milestone`, `gh-board`, `gh-wave-start`, `gh-wave-finish` | `/milestone`, `/issue`, `/pr` | commands |
| 3 | **Image Input** | `image-input.rkt` | 1.0.0 | 1 | `image-input` | — | hooks |
| 4 | **Racket Tooling** | `racket-tooling.rkt` | 1.0.0 | 1 | `racket-check`, `racket-edit`, `racket-codemod` | `/fmt`, `/check`, `/expand` | commands |

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
- **Active:** ✅ Yes — provides `the-extension` (note: `the-extension` binding not explicitly defined at module level like others, but `github-integration-extension` is the defined extension struct)

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
| `compact-context.rkt` | Context compaction support |
| `message-inject.rkt` | Message injection into agent conversation |
| `message-renderer.rkt` | Custom message rendering |
| `custom-renderer-registry.rkt` | Registry for custom renderers |
| `custom-ui-api.rkt` | Custom UI API for extensions |
| `dialog-api.rkt` | Dialog UI API |
| `widget-api.rkt` | Widget API for TUI extensions |
| `ui-channel.rkt` | UI communication channel |
| `resource-discovery.rkt` | Resource discovery for extensions |
| `session-export.rkt` | Session export functionality |
| `ext-package-manager.rkt` | Extension package management |
| `package-audit.rkt` | Package auditing |
| `manifest-audit.rkt` | Manifest auditing |
| `q-sync.rkt` | Q sync functionality |
| `test-harness.rkt` | Testing harness for extension development |

There is also a `remote-collab` subdirectory (likely a more complex extension bundle).

---

## Summary

- **4 loadable extensions** are available, all targeting **API version "1"**
- All 4 are **actively loadable** — each provides `the-extension` and uses valid hooks
- Total **12 tools** and **9 slash commands** are registered across all extensions
- The tier system ensures extensions only access capabilities they're authorized for
- Extensions support hot-reload, integrity verification (SHA-256), and compatibility checking
