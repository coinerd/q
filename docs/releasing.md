# Release Process

This document describes how to cut a new release of **q**.

## Version Policy

q follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html) (**MAJOR.MINOR.PATCH**).

| Bump | When |
|------|------|
| **MAJOR** | Breaking public API changes |
| **MINOR** | New features, backwards-compatible |
| **PATCH** | Bug fixes, backwards-compatible |

## Pre-release Checklist

Before starting a release, verify every item below:

- [ ] All tests pass: `raco test tests/` from the `q/` root
- [ ] Version bumped in `info.rkt` **and** `util/version.rkt` (both must match)
- [ ] `CHANGELOG.md` updated with the new version entry
- [ ] `README.md` metrics (test count, module count) updated
- [ ] Clean git status (`git status` shows no uncommitted changes)

## Release Steps

### 1. Update version in `info.rkt` and `util/version.rkt`

Both files must contain the same version string. Edit them together:

**`info.rkt`**:
```racket
(define version "0.6.2")
```

**`util/version.rkt`**:
```racket
(define q-version "0.6.2")
```

> **Tip:** Run `racket scripts/lint-version.rkt` to verify both files match.

### 2. Update `CHANGELOG.md`

- Move the `[Unreleased]` heading to a new versioned section with today's date.
- Add entries under the appropriate sub-headings (`Added`, `Changed`, `Fixed`, etc.).
- Add a new bottom link for the version and update the `[Unreleased]` link.

### 3. Commit

```bash
git add -A
git commit -m "Release v0.6.2"
```

### 4. Tag

```bash
git tag -a v0.6.2 -m "Release v0.6.2"
```

### 5. Push

```bash
git push origin main --follow-tags
```

### 6. Create GitHub Release

Use the GitHub UI or API to create a release from the tag.
Paste the relevant `CHANGELOG.md` section as the release description.

## Post-release

After publishing the release:

- [ ] Add a fresh `## [Unreleased]` section at the top of `CHANGELOG.md`
- [ ] Commit and push the changelog housekeeping
