# Artifact Provenance and Determinism — v1.00.31 W5

Wave: W5 · issue #9728 · milestone #896
Plan: `fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75`

## What this wave closes

F7 — recorded provenance drift and internal artifact inconsistency — and
F13 — the frozen contract going silently stale (tracked in this wave per the
frozen plan; implementation landed in the same wave).

## The provenance lint

`scripts/ci/verify-artifact-provenance.rkt` checks every declared artifact
version directory under `artifacts/**/v*/`:

1. **SHA256SUMS binding** — every recorded digest must match the named file;
   every file under the directory must be recorded. Two historical path
   conventions (repository-root-relative and directory-relative) are accepted
   as recorded spellings; the bytes decide. Coverage spans every declared
   version directory, including the dash-suffixed historical ones (`-cN`,
   `-final`, `-census`, `-hotspots`, `-prepared-env`, ...), not just `-wN`
   waves; historical findings there are notes, never hard failures.
2. **Provenance heads** — values of head/sha/commit-named fields (40-hex)
   must resolve to real commits and, for the current wave, be ancestors of the
   wave tip. Recorded heads that are neither prove stale provenance. A
   tree-named field is not a commit identity: it is validated against its
   paired head (the tree must equal that commit's tree), so a valid tree is
   never refused as a non-commit.
3. **Canonical JSON** — the current wave's non-raw JSON artifacts must equal
   their canonical form (sorted keys, 1-space indent, trailing newline,
   Python-`json.dumps(ensure_ascii=True)`-compatible escapes) byte-for-byte.
   The escaping contract is what makes regeneration with the Python generator
   byte-identical: non-ASCII and control characters use the same lowercase
   `\uXXXX` forms (astral characters as surrogate pairs).
4. **Cross-artifact consistency** — inside an artifact that declares a
   structured `timing` object with `*-ms` keys, every prose `<n> ms` value
   must equal one of the structured numbers. A disagreement is refused naming
   both sources (the F7 rollback-drill shape). The same agreement is enforced
   between the current wave's structured timings and every markdown report
   bound by the directory's `SHA256SUMS`: unmarked prose milliseconds must
   equal a delivered timing, while blocks explicitly labelled as red-fixture,
   refusal, blocked-branch, history, or an older wave (`v1.00.2x-`/`v1.00.30-`)
   are exempt because a report may legitimately cite non-delivered values.
   The comparison set is the union of every artifact's structured timings in
   the version — collected before the reports are scanned, so a report may
   cite a value recorded by any artifact regardless of file order.
5. **Recorded digest fields** — a JSON object carrying a `path` beside a
   64-hex `sha256` must name an existing file whose bytes hash to exactly that
   digest. A stale or wrong recorded digest is refused on the current wave and
   reported as a note for historical artifacts.
6. **Recursive coverage** — every check traverses nested subdirectories of the
   version directory; only `raw/` is exempt (captured payloads are bound by
   SHA256SUMS but intentionally unenforced in content). A bound nested
   artifact therefore cannot bypass any check.

The F7 failure mode is also registered with the wave-delivery integrity
harness: `tests/test-wave-delivery-integrity-register.rkt` now guards row F7
by running this lint's own contract suite, so W6's expect-refused replay can
exercise the original defect through the canonical harness rather than a
re-statement of its fixture.

Failures are typed `provenance-drift` (exit 2). Historical observations that
cannot be strictly verified — blocked-branch pinned heads recorded as
reproduction evidence, digest drift discovered after delivery, formatting
frozen before the canonical form existed — are printed as `provenance-note`
and never silently ignored.

## Historical findings (reported, not repaired)

The lint's first run found genuine pre-existing historical drift on main:
`test-runtime/v1.00.28-w1` and `tier-ownership/v1.00.28-w0` SHA256SUMS entries
no longer match the committed artifact bytes. These are recorded here and
reported by every lint run as `provenance-note`s. Repairing them would mean
rewriting delivered records; this wave's scope forbids that.

## Determinism

`artifacts/wave-delivery-integrity/v1.00.31-w5/raw/matrix-gen.py` regenerates
`provenance-matrix.json` and `SHA256SUMS` byte-identically from stable inputs
(recorded git identity, file bytes). No timestamps are embedded. The fixture
that reproduces the F7 timing inconsistency carries the measured values; the
matrix itself carries only digests of committed bytes — measured values are
separated from digested structure.

## Red-first evidence

- `raw/red-first-f7-lint-refusal.txt` — the F7 fixture (prose 260/245 ms vs
  `eager-fallback-ms` 247/256 ms; recorded head `8299409c` = the blocked
  v1.00.30-w4 pin) emitted as a declared artifact and refused by the lint.
- `raw/red-first-f13-baseline.txt` — the F13 baseline reproduction against
  the pre-fix commit `d1df9e9b`: silent stale snapshot reuse, generic drift
  classification, and a plan-id blind to the plan body.
