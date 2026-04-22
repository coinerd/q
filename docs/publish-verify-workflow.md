# Publish & Verify Workflow

<!-- verified-against: v0.15.2 -->

This document describes the workflow for publishing a q extension package
and verifying it works correctly.

## Pre-Publish Checklist

Before publishing a q extension:

- [ ] `manifest.rkt` is complete and valid (run `manifest-audit.rkt`)
- [ ] All tests pass (`raco test tests/`)
- [ ] `min-q-version` and `max-q-version` are set correctly
- [ ] `api-version` matches the extension API you target
- [ ] README.md documents configuration, hooks used, and permissions needed

## Publishing Steps

### 1. Package

```bash
tar czf my-extension-1.0.0.tar.gz \
  --exclude='.git' \
  --exclude='compiled' \
  --exclude='*.zo' \
  my-extension/
```

### 2. Verify Locally

```bash
# Install from tarball
raco pkg install my-extension-1.0.0.tar.gz

# Verify manifest
racket -e '(require "q/extensions/manifest-audit.rkt")'

# Run extension tests
raco test my-extension/tests/
```

### 3. Publish

**Current (v0.10.x)**: Publish to GitHub releases or a package repository URL.

**Future (v1.1+)**: Submit to the q package registry:
```bash
raco q pkg submit my-extension-1.0.0.tar.gz
```

### 4. Post-Publish Verification

The consumer verifies:
```bash
raco pkg install my-extension
racket -e '(require "q/extensions/loader.rkt") (list-extensions)'
```

## Automated Verification (CI)

Extension authors should add CI that:
1. Installs q from the target version range
2. Installs the extension
3. Runs the extension's tests
4. Runs `manifest-audit.rkt`

Example GitHub Actions workflow:

```yaml
name: Extension CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v6
      - name: Setup Racket
        uses: Bogdanp/setup-racket@v1
        with:
          version: '8.10'
      - name: Install q
        run: raco pkg install --auto q
      - name: Audit manifest
        run: racket -e '(require "q/extensions/manifest-audit.rkt")'
      - name: Test
        run: raco test tests/
```

## Version Compatibility Testing

To test your extension against multiple q versions:

```bash
for VERSION in 0.10.0 0.10.1 0.10.2; do
  raco pkg install q@$VERSION
  raco test tests/
done
```

## Troubleshooting

| Problem | Fix |
|---------|-----|
| Manifest validation fails | Check required fields in `docs/package-registry-spec.md` |
| Extension not loading | Verify `api-version` matches your q version |
| Hook not firing | Check `hooks` list in manifest matches actual hook usage |
| Permission denied | Add required permissions to manifest `permissions` list |
