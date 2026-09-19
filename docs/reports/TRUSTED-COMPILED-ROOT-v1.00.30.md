# Trusted compiled root prototype (v1.00.30 W3)

Wave: v1.00.30 W3 — trusted compiled root prototype
Issue: <https://github.com/coinerd/q/issues/9689> (milestone 895)
Branch: `campaign/v1.00.30-w3`
Status: prototype implemented and verified offline; **workflow reuse is not activated in this
wave** (activation is W4). Delivery binding pending.

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
| Whole payload | `payload.algorithm`, `payload.digest`, `payload.files` | canonical `sha256` over the ordered member set |
| Containment | `sources[].zo` paths | traversal / symlink-escape / missing-member gate (`path-stays-inside-root?`) |

Any mismatch raises `exn:fail:compiled-root-manifest` with a precise reason; the resolver then
selects the **single** eager current-source fallback and records a counted fallback. Producer-era
bytes are never executed unverified.

## Publication and containment

- Publication is atomic: the payload is staged, the manifest is written into staging, then the
  staging directory is renamed over the final path, then write permission is revoked recursively.
  Readers see either the previous complete root or the new complete root — never a partial state.
- A second publication to the same final path fails closed.
- The producer rejects dirty / untracked / generated compilation inputs at the boundary, and
  restores the producer checkout by deleting only the `compiled/` directories the build created.

## Red-first matrix

`tests/test-compiled-root.rkt` (11 cases, `@speed slow`, `@timeout 600`) and
`tests/test-compiled-root-manifest.rkt` (12 cases, `@speed fast`, `@timeout 300`) — 23 cases
total. Coverage:

- producer build from a clean checkout; dirty/untracked rejection;
- atomic publication; read-only payload; duplicate-publication rejection;
- relocation to a **different absolute checkout path** via a per-consumer map dir;
- in-process root hit (telemetry-observed, no consumer `compiled/` directory created);
- subprocess inheritance of the root via `compiled-root-launch-arguments`;
- fail-closed on marker change (consumer source digest mismatch) with eager current-source
  fallback and zero producer-era marker executions;
- fail-closed on missing published root;
- mutation-after-verification (TOCTOU) rejected at load time;
- pure manifest attack matrix: schema, producer trust, executable/version, platform/ABI,
  lockfile, payload corruption, traversal, absolute path, symlink escape, missing member,
  non-hash, empty sources, producer rejections.

## Independent review findings and fixes

An independent read-only review of the reanchored head found two blocking defects; both were
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