# Release Runbook

Operational runbook for cutting a release of `q`. Sections follow the order a
release actually proceeds in: local pre-flight, then the tag-driven CI pipeline.

## 1. Pre-flight (one command, seconds — run BEFORE pushing a tag)

From the repository root:

```
racket scripts/release-preflight.rkt v<X>.<Y>.<Z> --readiness
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
4. **Release readiness** (`--readiness`, BUG-0008) — every fix *required* for this
   release version is verifiably contained in the tagged commit (see §4).

Without `--readiness` only the structural invariants (1–3) run.

Exit codes: `0` ready · `1` invariant violated (fix as printed) · `2` usage error.

## 2. CI wiring (fail-fast job ordering)

`.github/workflows/release.yml` (trigger: `push: tags: v*`) runs the same command
as its **first** job:

```yaml
preflight:
  ...
  - run: racket scripts/release-preflight.rkt ${{ github.ref_name }} --readiness
```

Every expensive job declares `needs: preflight` (starting with the full test
suite), so a structurally invalid tag fails the run in seconds instead of after
an hour-long pipeline.

Note: the `preflight` job checks out with `fetch-tags: true` (the tag *object*
must be present locally for `git cat-file -t` to distinguish annotated from
lightweight tags) and `fetch-depth: 0` (readiness ancestry checks need the
history that contains the required fixes' landing commits). It also gets
`GITHUB_TOKEN` with `issues: read` for the milestone part of the readiness
derivation.

## 3. Manifest generator dry-run (secondary tool)

```
racket scripts/gen-release-manifest.rkt --dry-run v<X>.<Y>.<Z>
```

Validates inputs (annotated tag object, full 40-char SHAs, tag version ==
canonical q-version) and prints the manifest that *would* be generated, with
placeholder asset size/checksum until the real tarball exists. Nothing is
published. When HEAD is not the tagged commit, the dry-run notes it and
describes the tagged commit.

## 4. Release-readiness gate (BUG-0008)

The readiness stage answers one question before anything expensive runs: **does
the tagged commit verifiably contain every fix this release is required to
ship?** Without it, a tag cut on a commit that predates a required fix is
guaranteed to run RED, and remediation means force-moving the tag.

### How the required-fix list is derived (single source of truth, no per-release hand list)

1. **Bug registry** (`.planning/bugs/INDEX.md`): every row whose **Fixed in**
   column targets this release version becomes a required fix for it.
2. **Cross-checked against the tracker**: BUG-NNNN issues on the
   `v<version>` GitHub milestone (via the same helpers as
   `q/scripts/milestone-gate.rkt`) are merged into the same list.

The override `Q_BUG_REGISTRY=<path-to-INDEX.md>` points the derivation at an
alternative registry (used by tests/dry-runs).

### How containment is proven

Each fix's **landing commit SHA** is resolved (in order) from:

1. a `Landing commit:` line in the fix's `.planning/bugs/BUG-NNNN-*.md` report, or
2. the squashed merge commit of the fix's PR cross-referenced on its tracker issue.

Then, for every required fix:

```
git merge-base --is-ancestor <landing-sha> <tagged-sha>
```

must succeed. Missing SHA, unknown SHA, or non-ancestor → the stage **fails**,
naming exactly which BUG-ID/issue is missing, the SHA that would satisfy it,
and the remediation (merge the fix PR and/or record the landing SHA, then
re-point the tag at a commit that contains it). A required fix whose landing
SHA cannot be resolved from *either* source also hard-fails — readiness must be
provable, not assumed.

### The "record landing SHA on merge" convention

When a fix PR merges, record the squashed landing commit SHA on a
`Landing commit:` line in the fix's bug report
(`.planning/bugs/BUG-NNNN-*.md`, companion section). This is what the gate
consumes first; see `.planning/bugs/README.md` for the registry-side rules.
In CI the registry lives outside the repo, so the milestone fallback resolves
landing SHAs there — locally the registry is authoritative.

## 5. Close-out (one command, full audit trail)

After the readiness gate passes and the tag exists, the entire close-out runs as:

```
racket scripts/release-closeout.rkt v1.00.01
```

Dry mode (prints every stage and its sources, makes **no** writes):

```
racket scripts/release-closeout.rkt v1.00.01 --dry-run
```

(The flags are order-independent; both `v1.00.13 --dry-run` and
`--dry-run v1.00.13` behave identically.)

### Stages and what each verifies

| # | Stage | Verifies |
|---|-------|----------|
| 1 | NOTES | release-note input assembled from the closed issues in the version's milestone (`gh-issue list state=closed milestone_number=…`) **plus** fixed registry rows for that version; feeds `gen-release-notes.rkt` / `lint-release-notes.rkt` |
| 2 | READINESS | one final `release-preflight.rkt <tag> --readiness` run — the W2 gate, re-proven at close-out time |
| 3 | TAG | annotated tag created and pushed (idempotent: existing tag is verified, not duplicated) |
| 4 | WORKFLOW | `.github/workflows/release.yml` run for the tag goes green end-to-end (preflight → full suite → build → publish) |
| 5 | ARCHIVE | shipped planning/registry artifacts archived via `scripts/archive-planning.rkt` |
| 6 | REGISTRY | `scripts/check-registry.rkt` re-run so INDEX counts stay truthful post-archive (exit 0 required) |
| 7 | MILESTONE | the version's milestone is closed (`gh-milestone action=close`) |

The run ends with a `close-out report` listing `ok`/`FAIL` per stage and
`ALL STAGES GREEN` only when every stage succeeded. Any failure names the stage
and the remediation; the command is safe to re-run — every stage is idempotent
or lookup-first, so an interrupted close-out resumes without double side
effects.

If the `gh` CLI is unavailable, issue/milestone sources are reported as
skipped with a warning instead of failing the run; tag push and workflow
stages require `git`/`gh` respectively and fail loudly when missing.
