# Port/I/O Abstraction Pilot — v0.99.36 W6

**Date:** 2026-06-22
**Wave:** W6 (#8419)
**Risk Assessment:** GREEN (reward 15, risk 7)

## 1. Pilot Summary

This wave pilots the **I/O abstraction pattern** for script tooling, making
file-reading operations injectable via parameters so that check logic can be
tested without real file system access.

**Pilot target:** `scripts/lint-version.rkt` — the version-consistency linter.

## 2. Problem Statement

`lint-version.rkt` had 5 check functions (`check-info-rkt`, `check-readme-badge`,
`check-md-files`, `check-changelog-integrity`, `main`) that directly called
`file-exists?`, `file->string`, and `in-directory`. This made the check logic:

- **Untestable** without running the script as a subprocess in a real directory
- **Untestable in isolation** — couldn't test the checking logic separately from I/O
- **Hard to mock** — no way to inject test file content without creating real files

## 3. I/O Abstraction Solution

### New module: `scripts/lint-version-io.rkt`

Defines 4 parameters for file I/O operations:

| Parameter | Default | Purpose |
|-----------|---------|---------|
| `current-lint-file-exists?` | `file-exists?` | Check file existence |
| `current-lint-file->string` | `file->string` | Read file content as string |
| `current-lint-file->lines` | `file->lines` | Read file content as lines |
| `current-lint-read-md-paths` | `#f` (uses directory scan) | List .md files |

Plus mock file system helpers:

| Helper | Purpose |
|--------|---------|
| `make-mock-fs` | Create hash: `path-string -> content-string` from alist |
| `make-mock-exists` | Create mock `file-exists?` function |
| `make-mock-read-string` | Create mock `file->string` function |
| `make-mock-read-lines` | Create mock `file->lines` function |
| `make-mock-md-paths` | Create mock directory listing function |

### Pure functions extracted from lint-version.rkt

| Function | Input | Output | I/O? |
|----------|-------|--------|------|
| `check-info-content` | info.rkt content, canonical version | error list | No I/O |
| `check-readme-content` | README content, canonical version | error list | No I/O |
| `check-changelog-content` | CHANGELOG content | error list | No I/O |
| `find-mismatched-versions` | lines, version, filename | error list | No I/O (already existed) |

### Refactored I/O wrappers

All I/O wrapper functions now route through the parameters:
- `check-info-rkt` → uses `(current-lint-file-exists?)` and `(current-lint-file->string)`
- `check-readme-badge` → same
- `check-md-files` → uses `(current-lint-file->string)` and optionally `(current-lint-read-md-paths)`
- `check-changelog-integrity` → same
- `main` → uses parameters for reading `util/version.rkt`

### Guarded entry point

`(main)` was wrapped in `(module+ main (main))` so the script can be `require`d
from tests without side effects.

## 4. Benefits Demonstrated

1. **Pure-function testability:** Check logic tested with content strings — no file system needed
2. **Mock I/O injection:** Full I/O wrappers tested with in-memory mock file systems
3. **Zero behavioral change:** Script output identical when run normally (default parameters)
4. **Pattern reusability:** The mock-fs helpers can be reused for any script that adopts this I/O abstraction

## 5. Usage Pattern

```racket
;; In tests: inject mock file system
(parameterize ([current-lint-file-exists? (make-mock-exists fs)]
               [current-lint-file->string (make-mock-read-string fs)])
  (check-info-rkt "1.0.0"))

;; In production: default parameters use real file I/O (no change needed)
(racket scripts/lint-version.rkt)
```

## 6. Verification

- `raco make main.rkt` — PASS
- `test-lint-version-io.rkt` — 11 tests PASS
- `test-lint-version-pure.rkt` — 11 tests PASS (9 pure + 2 mock I/O)
- Smoke gate: 19/19 files, 286 tests PASS
- Script invocation: `racket scripts/lint-version.rkt` produces identical output
