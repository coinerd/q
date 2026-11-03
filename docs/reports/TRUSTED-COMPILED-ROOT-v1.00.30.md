# Trusted compiled root prototype (v1.00.30 W3)

Wave: v1.00.30 W3 — trusted compiled root prototype
Issue: <https://github.com/coinerd/q/issues/9689> (milestone 895)
Branch: `campaign/v1.00.30-w3`
Status: prototype implemented and verified offline; **workflow reuse is not activated in this
wave** (activation is W4). Delivery binding pending.

## Resolve-to-launch boundary

A parse or usage failure does not remove a flags file: raw argument tokens cannot
safely identify a path or command. A parsed `resolve` attempts to remove its
parser-bound flags file on failure, but deletion can itself fail. In either case
stale flags can remain. Direct CLI `launch` does not check the outcome of a prior
`resolve` and can consume those flags; callers must gate the flags path on a
successful resolve, rather than on file existence. The CI workflow publishes its
flags output only after resolve succeeds and selects its eager gate on failure.

## Goal

Build and prove, offline, a relocation-safe, authenticated, immutable **external compiled root**
for Racket bytecode, so a later wave can reuse same-run, same-head bytecode instead of paying the
eager full-path compilation cost on every shard. W3 proves the mechanism; W4 decides activation.

## Mechanism (Racket 8.10 semantics, probe-verified in this wave)

- A `.zo` under an explicit `current-compiled-file-roots` entry is consulted for a source `S`
  **unconditionally**: the `'modify-seconds` freshness gate governs the source-adjacent default
  `compiled/` directory, not external roots. The resolver still ages consumer source mtimes
  relative to root zo mtimes as defense in depth, but identity is proven by digests, never mtime.
- For an absolute source `S = /D0/.../Dn/name.rkt` and an absolute root entry `R`, the default
  compiled-load handler consults exactly
  `R/D0/.../Dn/compiled/name_rkt.zo` — i.e. `reroot-path(S-dir, R) = R/<S-dir without the leading
  slash>` plus the `compiled/` mode subdir and the **munged** member name
  (`path-add-extension(name.rkt, ".zo")`). Layouts that drop components, insert the source file
  name as a directory, or use the un-munged name are never consulted.
- A producer-built tree is therefore relocatable **only** when the consumer's absolute
  source-directory string matches the producer's. That is never assumed: a job-local
  **per-consumer read-only mapping directory** materializes the required layout as symlinks from
  the consumer's absolute path structure into the immutable published payload. The published root
  itself is never written.

## Identity dimensions (manifest `q-compiled-root-manifest-1`)

Pure validation runs **before any compiled code loads** and is independent of the artifact's own
code (`ci/prepared-environment/compiled-root-manifest.rkt`).

| Dimension | Manifest field | Enforcement |
|---|---|---|
| Schema | `schema` | must equal `q-compiled-root-manifest-1` |
| Producer provenance | `producer.label`, `producer.trusted` | label must be in the consumer's trusted set and flagged trusted |
| Exact executable | `racket.executable-digest` | SHA-256 of the consumer's `racket` binary must match |
| Racket version | `racket.version` | exact `(version)` match |
| Platform / ABI | `platform.os`, `platform.arch`, `platform.so-suffix` | exact match on all three |
| Locked dependency set | `packages.lockfile`, `packages.lockfile-digest` | lockfile digest must match the consumer's expectation |
| Source content | `sources[].path`, `sources[].digest`, `sources[].bytes` | per-source digest and size |
| Payload integrity | `sources[].zo`, `sources[].zo-digest`, `sources[].zo-bytes` | per-member digest and size |
| Whole payload | `payload.algorithm`, `payload.digest`, `payload.files` | canonical `sha256` over the ordered coverage set: every declared `sources[].zo` member, **plus** the `load-keys` source and zo members when the field is bound; `payload.files` counts the declared `sources` entries only (see the compatibility addendum) |
| Child transfer binding (optional) | `load-keys` | absent, or a hash binding `path`/`digest`/`zo`/`zo-digest` for the fixed publication names (`load-keys.rkt`, `compiled/load-keys_rkt.zo`); when present, both members are containment-checked, individually digest-verified, and covered by `payload.digest`. Optionality is **presence-based** (`hash-has-key?`): a present-but-false field fails the shape gate closed |
| Containment | `sources[].zo` paths | traversal / symlink-escape / missing-member gate (`path-stays-inside-root?`) |

Any mismatch raises `exn:fail:compiled-root-manifest` with a precise reason; the resolver then
selects the **single** eager current-source fallback and records a counted fallback. Producer-era
bytes are never executed unverified.

## Manifest compatibility addendum: optional `load-keys` (v1.00.31 W1 repair round)

The v1.00.31 W1 repair round added the child transfer table as a compiled module inside the root
(`write-compiled-root-load-keys!`) and bound it with a **new optional manifest field**. The exact
contract, superseding the shorter v1.00.30 table row above where they differ:

- **Exact optionality.** `load-keys` may be absent — every artifact published before the repair
  round, and any root built without the transfer module, has no such key. Absence is detected by
  **key presence** (`hash-has-key?`), never by field truthiness, and is never a *validation*
  error (but see the old-roots section below: a root without the field cannot launch). Presence
  is fail-closed: a manifest carrying `load-keys` with a `#f` (or any non-hash) value is neither
  absent nor well-shaped — it is rejected by the shape gate (`load-keys field is not a hash`),
  never silently treated as absent. When present it must be a hash with `path`, `digest`, `zo`
  and `zo-digest` (hex digests, safe rel paths). The producer verifies the claimed digests
  against the staged bytes at `make-compiled-root-manifest` time; the consumer re-verifies shape,
  containment, and both digests at validation time.
- **Digest coverage.** `payload.digest` is the canonical `sha256` over the sorted,
  `rel\0len:bytes`-framed coverage set = every declared `sources[].zo` member **plus**
  `load-keys.path` and `load-keys.zo` **iff the field is present**. With the field absent the
  coverage set is exactly the pre-field member set, so the digest algorithm is byte-identical to
  the one pre-field readers compute. The transfer members are additionally pinned individually by
  `load-keys.digest`/`zo-digest`, so no byte of the child transfer path can mutate below the
  manifest.
- **`payload.files` semantics.** `payload.files` is `(length sources)` — the number of declared
  classic payload members. It is deliberately **not** the digest-coverage count: with
  `load-keys` bound, the digest covers two members that `payload.files` does not count.
  Consumers must never derive coverage from `payload.files`.
- **Intentional fail-closed directional compatibility.** The schema string is deliberately
  **not** bumped for this optional field; the whole-payload digest is the version gate:
  - *old reader × new artifact* → **rejects** (`whole-payload digest mismatch`): a pre-field
    reader computes the sources-only digest, which cannot equal a coverage digest that includes
    the transfer members' bytes. Old readers can never partially trust a new artifact.
  - *new reader × old artifact* → **accepts**: with the field absent the new reader computes
    exactly the old coverage set, so every pre-field artifact validates unchanged.
  - *field stripped from a new artifact* → **rejects** under a new reader: deleting the
    `load-keys` key makes the validator recompute the old sources-only digest, which mismatches
    the published coverage digest — the transfer binding cannot be removed by field deletion.
  - **Migration is one-directional**: once a producer binds `load-keys`, old readers cannot
    consume the root; a downgrade must republish with the field absent (a fresh, complete
    publication — the second-publication-fails-closed rule still applies to the same path).

This directional property is pinned by a focused regression in `tests/test-compiled-root-manifest.rkt`
(old algorithm rejects a `load-keys` artifact when the field is missing; old-style manifests still
validate under the new reader). Do not weaken the coverage gate and do not rewrite historic
artifacts to match.

## Preexisting limitations of the per-load canary (contract-review qualification)

The per-load digest canary is a **hash-then-load (two-open) mechanism for ordinary payload
members**: the `current-load` wrapper digests one open of the zo, and the module system's
default handler then re-opens the same path for the actual load. Consequently:

- the canary deterministically rejects any payload mutation that lands **before** its digest
  read (fail closed at the digest check);
- it is **not** a blanket race guarantee: a same-user process can in principle swap the bytes
  between the two opens, so the executed bytes are not *provably* the digested bytes for
  ordinary payloads. The deterministic executed-equals-authenticated property exists only for
  the child transfer module (`authenticate-compiled-module` reads the zo exactly once and evals
  the compiled form **from those bytes** — `ci/prepared-environment/transfer-auth.rkt`), whose
  digests root the rest of the transfer table;
- a **mapping limitation** compounds it: the guard maps incoming load paths back to payload
  members through the manifest's key tables (raw + link-blind simplified spellings, plus the
  by-zo table). A load arriving under a spelling outside those key sets — e.g. an unresolved
  symlink spelling on a symlinked checkout (macOS `/tmp`), the W3 deferred observation below —
  passes through **without** a digest check (the guard silently no-ops).

Both are preexisting properties of the shipped design, recorded here so the guarantee wording
elsewhere in this report is read as bounded by them. **No guard is weakened by this
documentation**: resolve-time digests, containment, the baked startup canaries, and the
per-load checks are unchanged; structurally closing the two-open window is future work, not
a license.

## Old roots: validation succeeds, launch fails closed (rebuild requirement)

Roots published before the transfer module existed (manifest with the `load-keys` field
absent) are backward-compatible readers' artifacts: they **validate** end-to-end under the
new reader (the optional-field acceptance above). They **cannot launch**:

- `compiled-root-launch-arguments` **raises** for a manifest without the field — launch
  arguments cannot be derived without the compiled transfer module — and the raised error
  names the explicit remedy: `rebuild with the current producer`.
- The `resolve` CLI step (`ci/prepared-environment/compiled-root.rkt resolve`) derives launch
  arguments **before** writing the flags file, so an old root produces a **non-zero resolve
  and does not write new flags**. After successful parsing, failure attempts to delete the
  parser-bound preexisting flags file, but deletion may fail; parse errors do not attempt
  deletion. A direct subsequent launch can therefore consume stale flags. Callers must gate
  launch on resolve success rather than relying on file absence.
- A present-but-false `load-keys` field cannot slip past this boundary either: the
  presence-based shape gate rejects it at validation (see the addendum).

The remedy for an old root is an **explicit rebuild with the current producer**
(`scripts/ci/compiled-root.rkt build …`), never a hand-edit of the historic artifact. This
validation-vs-launch split is pinned by regressions in `tests/test-compiled-root.rkt`
(old-style artifact resolves; launch-argument derivation raises with the rebuild requirement;
the CLI resolve exits non-zero, writes no flags file, and invalidates a preexisting flags
file; `launch` under an active `Q_CI_COMPILED_ROOT` lane runs the child unmapped, never
consuming the invalidated flags).

## Publication and containment

- Publication is atomic: the payload is staged, the manifest is written into staging, then the
  staging directory is renamed over the final path, then write permission is revoked recursively.
  Readers see either the previous complete root or the new complete root — never a partial state.
- A second publication to the same final path fails closed.
- The producer rejects dirty / untracked / generated compilation inputs at the boundary, and
  restores the producer checkout by deleting only the `compiled/` directories the build created.

## Red-first matrix

`tests/test-compiled-root.rkt` (30 cases, `@speed slow`, `@timeout 600`) and
`tests/test-compiled-root-manifest.rkt` (46 cases, `@speed fast`, `@timeout 300`) — 76 cases
total. Coverage:

- producer build from a clean checkout; dirty/untracked rejection;
- atomic publication; read-only payload; duplicate-publication rejection;
- relocation to a **different absolute checkout path** via a per-consumer map dir;
- in-process root hit (telemetry-observed, no consumer `compiled/` directory created);
- subprocess inheritance of the root via `compiled-root-launch-arguments`;
- flags-file lifecycle: a successful resolve writes a well-formed file; a failed resolve
  attempts to invalidate a parser-bound preexisting file after parse success; parse errors
  and deletion failures may leave it intact. Launch is read-only and callers must gate it
  on successful resolution; option values are not speculatively deleted on parse errors;
- shared map-dir re-entry (shard retries) remaps idempotently — same-target links are a
  no-op, stale links are replaced; the same-target probe compares resolved identities
  (`resolve-path`), never file content;
- fail-closed on marker change (consumer source digest mismatch) with eager current-source
  fallback and zero producer-era marker executions;
- fail-closed on missing published root;
- mutation-after-verification (TOCTOU) rejected at load time (deterministically for the
  transfer module; ordinary payload members are covered by the per-load hash-then-load
  canary — bounded by the race-window qualification above);
- pure manifest attack matrix: schema, producer trust, executable/version, platform/ABI,
  lockfile, payload corruption, traversal, absolute path, symlink escape, missing member,
  non-hash, empty sources, producer rejections.

## Independent review findings and fixes

An independent read-only review of the reanchored head found four blocking defects; all were
fixed red-first (failing regression added first, then the fix):

1. **Producer CLI always failed** — `scripts/ci/compiled-root.rkt` `build` applied the
   `compiled-root` struct accessor to the manifest hash returned by `build-compiled-root!`,
   raising a contract violation. The same CLI could not parse its documented
   `build --checkout ...` order, because Racket's `command-line` stops flag parsing at the first
   non-flag token. Fixed by passing the manifest hash directly and by reordering argv (flags
   first, subcommand last) via `#:argv` while capturing the `--` child-argument tail from the
   raw argv so `run` keeps its positional child args.
2. **Multi-`zo` checkout restore aborted** — the producer deleted each `compiled/` directory once
   per harvested `.zo`; a module whose in-checkout dependency closure shares one `compiled/`
   directory hit `delete-directory/files` on an already-removed path and raised
   `exn:fail:filesystem`. Fixed by deleting each created directory exactly once.
3. **Stale flags file survived a failed resolve** — the resolve CLI reported failure without
   touching a flags file left on the same path by an earlier successful resolve (retried
   shard, reused runner temp directory, different root); `launch` treats an existing
   well-formed file as proof of a verified root and bootstrapped children from a root the
   failing resolve had just rejected (the old-root case). Fixed red-first: every resolve
   post-parse failure path attempts to delete the parser-bound flags file before reporting
   failure. Deletion can fail, and parse errors bypass this handler; direct launch can
   still consume surviving stale flags. The regression also caught a
   second defect: the map-dir remap probe decoded a link target's file content as UTF-8
   (payload members are binary zo), so any resolve re-entering a shared map dir failed and a
   retried shard could never recover; the probe now compares resolved identities
   (`resolve-path`), making same-target re-entry a no-op that reads no payload data.
4. **Launch-intent scanner mistook option values for commands** — the raw pre-parse argv
   scanner that recovers the flags paths and the launch/resolve intent hunted the command
   token as the FIRST non-flag token, without skipping a value-taking switch's VALUE. In
   `--root launch --flags-file F --unknown-option resolve` the value of `--root` spells
   "launch", so the scanner classified the resolve-intent invocation as launch intent and
   the parse failure at the unknown option SKIPPED its destructive invalidation — a stale
   flags file survived for a later `launch` to consume unverified. The mirror argv
   `--flags-file F --unknown-option launch` (flags-first: the trailing "launch" is the
   command token) was misclassified as resolve intent, so the launch-side parse failure
   DELETED the preexisting flags file, violating launch's read-only contract. Fixed red-
   first with a scanner, but later review found additional ambiguous argv cases. The scanner
   was removed: parse errors leave files untouched, and only a successfully parsed resolve
   attempts invalidation. Callers must enforce the resolve-success boundary.

Regression cases added to the e2e matrix: a module with an in-checkout dependency closure (two
`zo`s in one `compiled/` directory) and an end-to-end CLI `build` invocation.

### Deferred (non-blocking) review observations

- The TOCTOU guard keys by `simplify-path` (symlink-resolved) while `current-load` may supply an
  unresolved source path; on symlinked checkouts (e.g. macOS `/tmp`) the key can miss and the
  guard silently no-ops.
- The resolver ages consumer source mtimes (metadata mutation); the surrounding comment wording
  ("consumer tree is never written to") should be read as "no files are written", and the field
  named `zo-mtime-ms` actually holds seconds.
- `with-compiled-root` installs the `current-load` wrapper and does not restore it after the
  thunk.
- `sources[].bytes` is validated for shape only (not compared to any on-disk value).

These are recorded here rather than fixed in W3, to keep the wave narrow; they should be
addressed before W4 activation.

## Scope / activation

W3 is an **offline prototype**: no workflow consumes the external root yet, and the checkout
purge and eager full path remain the defaults. W4 is where producer and consumer are integrated
on required PR fast lanes with a global off/eager rollback.

## Launch HOLD

The wave was provisioned as `Inbox` under a launch HOLD (predecessor `gsd-governance` failure
`evidence is not a hash` in W11 binding; live branch protection omitted policy-required
`lint-quality`). Per the campaign contract, missing remote evidence means HOLD and no weakened
checks or fabricated evidence — this report records only what was locally observed.