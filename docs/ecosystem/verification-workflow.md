# Verification Workflow

<!-- verified-against: v0.11.3 -->

How to verify a third-party q extension package before and after
installation.

---

## Table of Contents

1. [Overview](#overview)
2. [Checksum Validation](#checksum-validation)
3. [Compatibility Checks](#compatibility-checks)
4. [Extension Integrity Verification](#extension-integrity-verification)
5. [Using `q pkg verify`](#using-q-pkg-verify)
6. [Manual Verification Steps](#manual-verification-steps)
7. [Security Considerations](#security-considerations)
8. [See Also](#see-also)

---

## Overview

When installing a third-party q extension, you should verify:

| Check | What it protects against |
|-------|--------------------------|
| **Tarball checksum** | Tampered or corrupted downloads |
| **Version compatibility** | Extensions that crash on your q version |
| **Manifest integrity** | Modified or incomplete packages |
| **Source review** | Malicious code, excessive permissions |

The q toolchain provides automated checks for the first three. Source
review is a manual step you should perform for any untrusted author.

---

## Checksum Validation

### How it works

Every package in the index (`pkg/index.json`) includes a `checksum`
field — the SHA-256 hex digest of the tarball. The
`install-package!` function in `q/pkg/registry.rkt` validates this
automatically during installation:

1. Downloads the tarball to a temporary directory
2. Computes SHA-256 of the downloaded file
3. Compares against the `checksum` field in the index
4. **Rejects the package** on mismatch — the tarball is deleted and
   installation fails

```bash
# Installation fails with checksum mismatch
$ q pkg install my-extension
ERROR: Checksum mismatch for my-extension: expected abcdef..., got 123456...
```

### Manual checksum verification

Verify a tarball before installing:

```bash
# Download the tarball
curl -L -o my-extension-1.0.0.tar.gz https://example.com/my-extension-1.0.0.tar.gz

# Compute SHA-256
sha256sum my-extension-1.0.0.tar.gz
# Compare against the checksum listed in pkg/index.json

# Or use Racket directly
racket -e '
  (require file/sha1)
  (displayln (bytes->hex-string
    (sha256-bytes (open-input-file "my-extension-1.0.0.tar.gz"))))
'
```

### Index validation

Validate the entire package index for correct checksum formats:

```bash
racket scripts/lint-pkg-metadata.rkt --index pkg/index.json --check
```

This checks that every checksum is a valid 64-character lowercase hex
string.

---

## Compatibility Checks

### Automatic version resolution

The `resolve-version` function in `q/pkg/registry.rkt` checks
compatibility when you install a package:

```bash
# q automatically checks min/max version bounds
q pkg install my-extension
```

If your q version falls outside the declared range, the package is
skipped with a warning.

### Manual compatibility check

Check a package's compatibility before installing:

```racket
(require q/pkg/registry.rkt)

(define index (load-package-index "pkg/index.json"))
(define pkg (get-package-info index "my-extension"))

;; Check min/max q-version
(define min-v (hash-ref pkg 'min-q-version #f))
(define max-v (hash-ref pkg 'max-q-version #f))
(printf "Min q version: ~a~n" min-v)
(printf "Max q version: ~a~n" max-v)

;; Resolve for your current q version
(define resolved (resolve-version index "my-extension" "0.11.2"))
(unless resolved
  (displayln "Package is NOT compatible with your q version"))
```

### Compatibility fields

| Field | Location | Format | Example |
|-------|----------|--------|---------|
| `compat` | `qpm.json` | Semver range | `">=0.10.0 <1.0.0"` |
| `min-q-version` | `index.json` | SemVer | `"0.10.0"` |
| `max-q-version` | `index.json` | SemVer (exclusive) | `"1.0.0"` |

The `compat` field in `qpm.json` is checked at extension load time by
the extension loader. The index fields are checked at install/resolve
time by the registry client.

---

## Extension Integrity Verification

### Using `verify-package-checksum`

The `verify-package-checksum` function in
`q/extensions/manifest-audit.rkt` validates an installed package:

1. Reads `qpm.json` from the package directory
2. If a `checksum` field is present, recomputes the SHA-256 of all
   listed files (sorted lexicographically, concatenated)
3. Returns `#t` if the checksums match, `#f` otherwise

```racket
(require q/extensions/manifest-audit.rkt)

(verify-package-checksum "~/.q/packages/my-extension")
;; => #t if integrity is intact
```

### Using `audit-package`

For a full audit (missing files, extra files, checksum):

```racket
(require q/extensions/manifest-audit.rkt)

(define issues (audit-package "~/.q/packages/my-extension"))
(for ([issue issues])
  (displayln issue))
;; No output = package is clean
```

The auditor detects:

| Issue | Description |
|-------|-------------|
| Missing file | A file listed in `qpm.json` doesn't exist on disk |
| Extra file | A file exists on disk but isn't listed in `qpm.json` |
| Checksum mismatch | Recomputed SHA-256 differs from the declared value |

---

## Using `q pkg verify`

Verify all installed packages or a specific one:

```bash
# Verify a specific package
q pkg verify my-extension

# Verify all installed packages
q pkg verify --all
```

Under the hood, `q pkg verify` calls `verify-package` from
`q/pkg/registry.rkt`, which:

1. Looks up the package in the index
2. Compares the installed state against the index entry
3. Reports whether the package is present and its checksum matches

```bash
# Successful verification
$ q pkg verify my-extension
Package my-extension installed and verified

# Package not found
$ q pkg verify nonexistent
Package nonexistent not installed
```

---

## Manual Verification Steps

For packages from untrusted or unknown authors, perform these manual
checks in addition to the automated tools.

### 1. Inspect the source

```bash
# Clone the repository
git clone https://github.com/author/my-extension
cd my-extension

# Review the entry point
cat main.rkt

# Check for suspicious patterns
grep -rn 'system\|shell-execute\|subprocess\|file-delete' *.rkt
```

### 2. Review the manifest

```bash
# Read the manifest
cat qpm.json

# Validate it parses correctly
racket -e '
  (require q/extensions/manifest.rkt)
  (define m (read-qpm-manifest "qpm.json"))
  (if m
      (let-values ([(ok? errors) (validate-manifest m)])
        (if ok?
            (displayln "Manifest is valid")
            (for ([e errors]) (printf "ERROR: ~a~n" e))))
      (displayln "ERROR: Cannot parse qpm.json"))
'
```

### 3. Run the full audit

```bash
racket -e '
  (require q/extensions/manifest-audit.rkt)
  (define issues (audit-package "."))
  (cond
    [(null? issues) (displayln "Package audit PASSED")]
    [else
     (for ([i issues]) (printf "ISSUE: ~a~n" i))
     (exit 1)])
'
```

### 4. Check permissions and hooks

Look at the manifest for:

- **`files`** — are there files not obviously needed?
- **`entry`** — does the entry point make sense?
- **`compat`** — is the declared range reasonable?

Review the extension code for hook registrations:
- Extensions using `tool-call` hooks can intercept and modify tool
  calls
- Extensions using `input` hooks can see everything you type
- Extensions using `model-request-pre` hooks can modify prompts sent
  to LLM providers

See [Security & Trust Model](../security-trust-model.md) for the full
hook safety classification.

---

## Security Considerations

### Only install from trusted authors

The package index validates format and checksums but does **not**
perform code review. You are responsible for reviewing extension code
from authors you don't trust.

### Extension quarantine

If a package behaves unexpectedly after installation, quarantine it:

```racket
(require q/extensions/quarantine.rkt)

;; Quarantine: moves the extension to ~/.q/quarantine/
(quarantine-extension! "my-extension")

;; Disable: marks as disabled without moving files
(disable-extension! "my-extension")

;; Restore: moves a quarantined extension back
(restore-extension! "my-extension")
```

Quarantined extensions are completely isolated — they cannot be loaded
or executed.

### Safe mode

If you want to test an extension without risk of file modification:

```bash
q --safe    # blocks write, edit, bash, and extension-provided tools
```

In safe mode, only read-only tools (`read`, `grep`, `find`, `ls`,
`date`) are available. Extension-provided tools are blocked.

### Checklist for third-party packages

- [ ] Tarball SHA-256 matches the index checksum
- [ ] `min-q-version` / `max-q-version` are compatible with your q
- [ ] `qpm.json` passes validation
- [ ] `audit-package` reports no issues
- [ ] Source code reviewed for suspicious patterns
- [ ] Hook usage is documented and acceptable
- [ ] Author is trusted or code is audited

---

## See Also

- [Publishing Guide](publishing-guide.md) — how to package and publish
  extensions
- [Security & Trust Model](../security-trust-model.md) — safe mode,
  extension quarantine, hook safety, and credential handling
- [Package Registry Spec](../package-registry-spec.md) — registry
  format and conventions
- [API Stability](../api-stability.md) — API version compatibility
  guarantees
