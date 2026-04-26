# Publishing Guide

<!-- verified-against: 0.20.4 -->

How to package, validate, and publish a q extension to the package
index.

---

## Table of Contents

1. [Package Structure](#package-structure)
2. [info.rkt Requirements](#inforkt-requirements)
3. [Extension Manifest (qpm.json)](#extension-manifest-qpmjson)
4. [Compatibility Declaration](#compatibility-declaration)
5. [Example Manifests](#example-manifests)
6. [CI Validation](#ci-validation)
7. [Publishing to the Package Index](#publishing-to-the-package-index)
8. [Trust Considerations](#trust-considerations)
9. [See Also](#see-also)

---

## Package Structure

Every q extension package must follow this layout:

```
my-extension/
├── qpm.json            ← required: package manifest
├── main.rkt            ← entry point (loaded by extension loader)
├── README.md           ← documentation
├── LICENSE             ← license file
└── tests/              ← recommended
    └── test-my-ext.rkt
```

The extension loader (`q/extensions/loader.rkt`) discovers packages by
scanning for `qpm.json` files in configured extension directories.

---

## info.rkt Requirements

q packages are Racket packages and require an `info.rkt` at the root:

```racket
#lang info

(define collection "my-extension")
(define pkg-desc "A q extension for ...")
(define version "1.0.0")
(define pkg-authors '("your-name"))

(define deps '("base" "q-lib"))
(define build-deps '("rackunit-lib"))
```

### Required fields

| Field | Description |
|-------|-------------|
| `collection` | Collection name (must match directory name) |
| `pkg-desc` | One-line description |
| `version` | SemVer version string (X.Y.Z) |

The CI linter (`scripts/lint-pkg-metadata.rkt`) validates that `info.rkt`
starts with `#lang info`.

---

## Extension Manifest (qpm.json)

The `qpm.json` manifest is the authoritative metadata source for the q
package ecosystem. It is read, validated, and serialized by
`q/extensions/manifest.rkt`.

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | `string` | Unique package identifier (kebab-case, e.g. `my-extension`) |
| `version` | `string` | SemVer version (X.Y.Z) |
| `api_version` | `string` | q extension API version this targets (digit string, e.g. `"1"`) |
| `type` | `string` | Package type: `"extension"`, `"skill"`, or `"bundle"` |
| `description` | `string` | One-line description |
| `author` | `string` | Author name or handle |

### Optional Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `compat` | `string` | `null` | SemVer range for q compatibility (e.g. `">=0.10.0"`) |
| `files` | `string[]` | `[]` | List of files included in the package |
| `checksum` | `string` | `null` | SHA-256 hex digest of all listed files (sorted, concatenated) |
| `entry` | `string` | `null` | Entry-point module path (e.g. `"main.rkt"`) |
| `homepage` | `string` | `null` | Project URL |
| `license` | `string` | `null` | SPDX license identifier (e.g. `"MIT"`, `"Apache-2.0"`) |

### Validation Rules

The `validate-manifest` function enforces:

- **name** — must be a non-empty string
- **version** — must match `X.Y.Z` (digits separated by dots)
- **api_version** — must be a digit string (e.g. `"1"`, `"2"`)
- **type** — must be one of `extension`, `skill`, or `bundle`
- **description** — must be a non-empty string
- **author** — must be a non-empty string
- **compat** — if present, must contain a semver range operator (`<`, `>`,
  `=`, `~`, or `^`)

### Checksum Computation

The `compute-manifest-checksum` function (in
`q/extensions/manifest.rkt`) computes a SHA-256 digest:

1. Reads the `files` list from `qpm.json`
2. Sorts file paths lexicographically
3. Concatenates file contents
4. Returns the SHA-256 hex digest

```bash
# Compute checksum from Racket
racket -e '
  (require q/extensions/manifest.rkt)
  (displayln (compute-manifest-checksum "path/to/my-extension"))
'
```

---

## Compatibility Declaration

### In qpm.json: `compat` field

The `compat` field in `qpm.json` declares the q version range this
package is compatible with. Use semver range notation:

| Example | Meaning |
|---------|---------|
| `">=0.10.0"` | q 0.10.0 or later |
| `">=0.10.0 <1.0.0"` | q 0.10.x only |
| `"^0.10.0"` | Compatible with 0.10.0 (semver caret) |
| `"~0.10.0"` | Patch-level changes only (0.10.x) |

The extension loader checks these ranges at load time and refuses to
load incompatible packages.

### In the package index: `min-q-version` / `max-q-version`

When listing your package in `pkg/index.json`, declare compatibility
boundaries with explicit fields:

```json
{
  "name": "my-extension",
  "version": "1.0.0",
  "min-q-version": "0.10.0",
  "max-q-version": "1.0.0",
  "description": "...",
  "author": "...",
  "repo": "https://github.com/you/my-extension",
  "checksum": "sha256:abcdef..."
}
```

The `resolve-version` function in `q/pkg/registry.rkt` uses these
fields to filter packages during `q pkg install`:

- **`min-q-version`** — minimum q version required (inclusive)
- **`max-q-version`** — maximum q version supported (exclusive upper bound)

---

## Example Manifests

### Skill Extension

```json
{
  "name": "q-greeting-skill",
  "version": "1.2.0",
  "api_version": "1",
  "type": "skill",
  "description": "Adds a greeting command that personalizes messages",
  "author": "jane-dev",
  "compat": ">=0.10.0",
  "files": [
    "main.rkt",
    "greeting.rkt",
    "templates/default.txt",
    "README.md"
  ],
  "checksum": "a3f2b8c1d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1",
  "entry": "main.rkt",
  "homepage": "https://github.com/jane-dev/q-greeting-skill",
  "license": "MIT"
}
```

### Provider Extension

```json
{
  "name": "q-provider-cohere",
  "version": "0.3.0",
  "api_version": "1",
  "type": "extension",
  "description": "Cohere provider adapter for q — supports command-r-plus and embed models",
  "author": "acme-ai",
  "compat": ">=0.11.0 <1.0.0",
  "files": [
    "main.rkt",
    "cohere-adapter.rkt",
    "cohere-auth.rkt",
    "tests/test-cohere.rkt",
    "README.md"
  ],
  "checksum": "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4",
  "entry": "main.rkt",
  "homepage": "https://github.com/acme-ai/q-provider-cohere",
  "license": "Apache-2.0"
}
```

---

## CI Validation

### Linting package metadata

Run the built-in linter to validate `info.rkt` and the package index:

```bash
# Lint q's own info.rkt + pkg/index.json
racket scripts/lint-pkg-metadata.rkt

# Validate a specific index file
racket scripts/lint-pkg-metadata.rkt --index path/to/index.json

# Exit with code 1 on errors (useful in CI)
racket scripts/lint-pkg-metadata.rkt --check
```

The linter checks:
- `info.rkt` starts with `#lang info`
- Every index entry has required fields (`name`, `version`, `description`,
  `author`, `repo`, `checksum`)
- Name format matches `^[a-z][a-z0-9-]*$`
- Checksum is a valid 64-character hex string (SHA-256)
- Version matches semver format

### Auditing extension manifests

Run the manifest auditor to verify a package's integrity:

```bash
# Audit a specific package directory
racket -e '
  (require q/extensions/manifest-audit.rkt)
  (for ([issue (audit-package "path/to/my-extension")])
    (displayln issue))
'
```

The auditor checks for:
- **Missing files** — files listed in `qpm.json` that don't exist on disk
- **Extra files** — files on disk not listed in `qpm.json` (excluding
  `qpm.json` itself and `compiled/`)
- **Checksum mismatch** — computed SHA-256 differs from the declared
  checksum

### GitHub Actions example

```yaml
name: Package CI
on: [push, pull_request]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Setup Racket
        uses: Bogdanp/setup-racket@v1
        with:
          version: '8.10'
      - name: Install q
        run: raco pkg install --auto q
      - name: Lint package metadata
        run: racket scripts/lint-pkg-metadata.rkt --check
      - name: Audit manifest
        run: |
          racket -e '
            (require q/extensions/manifest-audit.rkt)
            (define issues (audit-package "."))
            (for ([i issues]) (displayln i))
            (unless (null? issues) (exit 1))
          '
      - name: Run tests
        run: raco test tests/
```

---

## Publishing to the Package Index

### 1. Prepare your package

- Ensure `qpm.json` is complete and valid
- Run the linter: `racket scripts/lint-pkg-metadata.rkt --check`
- Compute and set the checksum in `qpm.json`
- Run all tests: `raco test tests/`

### 2. Package the tarball

```bash
tar czf my-extension-1.0.0.tar.gz \
  --exclude='.git' \
  --exclude='compiled' \
  --exclude='*.zo' \
  my-extension/
```

### 3. Submit to the package index

Submit a pull request to add your package to `q/pkg/index.json`:

```json
{
  "name": "my-extension",
  "version": "1.0.0",
  "description": "A q extension for ...",
  "author": "your-name",
  "repo": "https://github.com/you/my-extension",
  "tarball": "https://github.com/you/my-extension/releases/download/v1.0.0/my-extension-1.0.0.tar.gz",
  "checksum": "sha256-hex-of-tarball",
  "min-q-version": "0.10.0",
  "max-q-version": "1.0.0"
}
```

The index entry must pass `validate-index-entry` in
`q/pkg/registry.rkt`, which requires: `name`, `version`, `description`,
`author`, `repo`, and `checksum`.

### 4. Post-publish verification

Consumers install and verify:

```bash
# Search for the package
q pkg search my-extension

# Install
q pkg install my-extension

# Verify integrity
q pkg verify my-extension
```

See [Verification Workflow](verification-workflow.md) for full details.

---

## Trust Considerations

Before publishing, review the security implications of your extension:

- **Hooks** — extensions that register `tool-call` or `input` hooks
  intercept sensitive data flows. Declare which hooks your extension uses.
- **Permissions** — if your extension needs network access or sandbox
  escapes, document this clearly in your README.
- **Supply chain** — consumers rely on the checksum in `index.json` to
  verify tarball integrity. Never publish without a valid checksum.

See [Security & Trust Model](../security-trust-model.md) for the full
trust model, extension quarantine, and hook safety classifications.

---

## See Also

- [Verification Workflow](verification-workflow.md) — how consumers
  verify packages before and after installation
- [Security & Trust Model](../security-trust-model.md) — extension
  quarantine, hook safety, and trust tiers
- [Package Registry Spec](../package-registry-spec.md) — registry
  format and conventions (legacy `manifest.rkt`-based)
- [API Stability](../api-stability.md) — API version tiers and
  stability guarantees
- [Publish & Verify Workflow](../publish-verify-workflow.md) — legacy
  publishing guide
