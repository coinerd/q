# Package Registry Specification

<!-- verified-against: 0.20.0 -->

This document specifies the q extension package registry format and conventions.

## Package Manifest

Every q extension package MUST include a `manifest.rkt` at the package root
containing structured metadata. The manifest is validated by
`extensions/manifest.rkt` and `extensions/manifest-audit.rkt`.

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | `string?` | Unique package identifier (kebab-case) |
| `version` | `string?` | SemVer version string (X.Y.Z) |
| `api-version` | `string?` | q extension API version this targets |
| `description` | `string?` | One-line description |
| `author` | `string?` | Author name or handle |

### Optional Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `homepage` | `string?` | `""` | Project URL |
| `license` | `string?` | `"MIT"` | SPDX license identifier |
| `min-q-version` | `string?` | `"0.0.0"` | Minimum q version required |
| `max-q-version` | `string?` | `"999.999.999"` | Maximum compatible q version |
| `dependencies` | `(listof string?)` | `'()` | Names of other q packages required |
| `stability` | `(or/c 'stable 'evolving 'experimental 'internal)` | `'experimental'` | Package stability tier |
| `hooks` | `(listof symbol?)` | `'()` | Hook types this extension uses |
| `permissions` | `(listof symbol?)` | `'()` | Requested permissions (sandbox, network, etc.) |

## Package Structure

```
my-extension/
├── manifest.rkt         ← required metadata
├── main.rkt             ← entry point (loaded by extension loader)
├── README.md            ← documentation
├── LICENSE              ← license file
└── tests/               ← optional tests
    └── test-my-ext.rkt
```

## Version Compatibility

Packages declare compatibility using `min-q-version` and `max-q-version`.
At load time, the extension loader checks these against `q-version` and
refuses to load incompatible packages.

## Registry Format (Future)

A centralized registry (planned for v1.1) will provide:

```json
{
  "name": "my-extension",
  "version": "1.0.0",
  "q-compat": ">=0.10.0 <1.0.0",
  "archive": "https://registry.q-lang.org/packages/my-extension-1.0.0.tar.gz",
  "checksum": "sha256:abcdef...",
  "signature": "ed25519:..."
}
```

## Package Audit

Run `racket q/extensions/manifest-audit.rkt` to validate all extension manifests
in the current project for schema compliance.

## See Also

- [Extension API](api.rkt) — public extension interface
- [API Stability](../docs/api-stability.md) — tier definitions
- [Publishing Guide](../docs/publish-verify-workflow.md) — how to publish
