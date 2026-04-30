<!-- verified-against: 0.24.4 --># Release Process

This document describes how to cut a new release of **q**.

## Version Policy

q follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html) (**MAJOR.MINOR.PATCH**).

| Bump | When |
|------|------|
| **MAJOR** | Breaking public API changes |
| **MINOR** | New features, backwards-compatible |
| **PATCH** | Bug fixes, backwards-compatible |

## Canonical Version Source

`util/version.rkt` is the **single canonical source** for the version string. All other version references are synced from it:

| Target | Updated by |
|--------|-----------|
| `info.rkt` | `racket scripts/sync-version.rkt --write` |
| `README.md` badge | `racket scripts/sync-version.rkt --write` |
| `README.md` verify snippet | `racket scripts/sync-version.rkt --write` |
| CI version-consistency job | Auto-checks on every push/PR |

> **Never edit version strings in `info.rkt` or `README.md` directly.**
> Bump `util/version.rkt`, then run `racket scripts/sync-version.rkt --write`.

## Pre-release Checklist

Before starting a release, verify every item below:

- [ ] All tests pass: `racket scripts/run-tests.rkt` from the `q/` root
- [ ] Version bumped in `util/version.rkt` (canonical source)
- [ ] Run `racket scripts/sync-version.rkt --write` to propagate to `info.rkt` and `README.md`
- [ ] Run `racket scripts/lint-version.rkt` to verify consistency
- [ ] `CHANGELOG.md` updated with the new version entry
- [ ] `README.md` metrics (test count, module count) updated
- [ ] Clean git status (`git status` shows no uncommitted changes)

## Release Steps

### 1. Update version in `util/version.rkt`

Edit the canonical source:

```racket
;; util/version.rkt
(define q-version "0.24.4")```

Then propagate:

```bash
racket scripts/sync-version.rkt --write
racket scripts/lint-version.rkt  # verify
```

### 2. Update `CHANGELOG.md`

- Move the `[Unreleased]` heading to a new versioned section with today's date.
- Add entries under the appropriate sub-headings (`Added`, `Changed`, `Fixed`, etc.).
- Add a new bottom link for the version and update the `[Unreleased]` link.

### 3. Commit

```bash
git add -A
git commit -m "Release v0.24.4"```

### 4. Tag

```bash
git tag -a v0.24.4 -m "Release v0.24.4"```

### 5. Push

```bash
git push origin main --follow-tags
```

### 6. Automated Release

The tag push triggers `.github/workflows/release.yml` which:

1. **Verifies** version consistency (`lint-version.rkt` + tag vs canonical check)
2. **Runs** the full test suite
3. **Builds** the release tarball (`q-VERSION.tar.gz`)
4. **Generates** release notes from `CHANGELOG.md` (`scripts/gen-release-notes.rkt`)
5. **Generates** the release manifest (`release-manifest.json` with SHA-256 checksums)
6. **Creates** a GitHub Release with notes, tarball, and manifest as assets

### 7. Smoke Test

The smoke test runs as a dedicated job (`smoke`) inside `.github/workflows/release.yml`, triggered on tag push. It runs after the `release` job completes:

1. Installs a clean Racket runtime (no composite action — tests the tarball against a pristine environment)
2. Downloads the release tarball from GitHub Release assets
3. Installs from tarball (`raco pkg install --auto --batch`)
4. Verifies `racket main.rkt --version` output matches the tag
5. Verifies `release-manifest.json` checksums and version
6. Runs the test suite (`racket scripts/run-tests.rkt --suite fast --jobs 4`)

> **Note:** The smoke job is merged into `release.yml` rather than a separate `post-release.yml` workflow. This ensures the smoke test runs as part of the same release pipeline and gates the overall workflow status.

## Post-release

After the automated release completes:

- [ ] Verify the GitHub Release page looks correct
- [ ] Verify the post-release smoke test passed
- [ ] Add a fresh `## [Unreleased]` section at the top of `CHANGELOG.md`
- [ ] Commit and push the changelog housekeeping

## Troubleshooting

| Problem | Fix |
|---------|-----|
| Version lint fails | Run `racket scripts/sync-version.rkt --write` |
| CI version-consistency fails | Check `util/version.rkt` matches the tag |
| Release notes are empty | Ensure `CHANGELOG.md` has a section for the version |
| Manifest missing from release | Check `gen-release-manifest.rkt` ran in the workflow |
