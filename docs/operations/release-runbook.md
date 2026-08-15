# Release Runbook

Operational runbook for cutting a release of `q`. Sections follow the order a
release actually proceeds in: local pre-flight, then the tag-driven CI pipeline.

## 1. Pre-flight (one command, seconds — run BEFORE pushing a tag)

From the repository root:

```
racket scripts/release-preflight.rkt v<X>.<Y>.<Z>
```

The same invariants the Release workflow checks first in CI. Fails fast on the
**first** violated invariant and prints a remediation command:

1. **Tag exists and is annotated** — `git cat-file -t <tag>` must print `tag`.
   A lightweight tag prints `commit` and is rejected. Remediation:
   `git tag -fa <tag> -m "<tag>" && git push origin <tag> --force`
2. **Tag ↔ version consistency** — the version encoded in the tag must equal the
   canonical `q-version` (`util/version.rkt`) and the package version (`info.rkt`).
   After bumping `util/version.rkt`, run `racket scripts/sync-version.rkt --write`.
3. **Manifest dry-run** — `racket scripts/gen-release-manifest.rkt --dry-run <tag>`
   must validate inputs and render the manifest that would be generated (no
   tarball required, nothing published).

Exit codes: `0` ready · `1` invariant violated (fix as printed) · `2` usage error.

## 2. CI wiring (fail-fast job ordering)

`.github/workflows/release.yml` (trigger: `push: tags: v*`) runs the same command
as its **first** job:

```yaml
preflight:
  ...
  - run: racket scripts/release-preflight.rkt ${{ github.ref_name }}
```

Every expensive job declares `needs: preflight` (starting with the full test
suite), so a structurally invalid tag fails the run in seconds instead of after
an hour-long pipeline.

Note: the `preflight` job checks out with `fetch-depth: 1` **and**
`fetch-tags: true` — the tag *object* must be present locally for
`git cat-file -t` to distinguish annotated from lightweight tags.

## 3. Manifest generator dry-run (secondary tool)

```
racket scripts/gen-release-manifest.rkt --dry-run v<X>.<Y>.<Z>
```

Validates inputs (annotated tag object, full 40-char SHAs, tag version ==
canonical q-version) and prints the manifest that *would* be generated, with
placeholder asset size/checksum until the real tarball exists. Nothing is
published. When HEAD is not the tagged commit, the dry-run notes it and
describes the tagged commit.
