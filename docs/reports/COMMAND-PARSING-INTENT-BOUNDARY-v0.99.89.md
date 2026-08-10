# Command Parsing & Intent Boundary — v0.99.89 W3 (#9229, milestone #876)

**Status:** DONE (PR pending)
**Branch:** `feature/v09989-w3-command-parsing-intent-boundary`
**Base:** `74da7e8a` (W2 merge, PR #9256) + W2-review fold (`f3a6fc87`)
**Gate:** parser fitness + command corpus + Fast + review

## Goal (roadmap + issue, immutable)

Keep the parser I/O-free; separate command INTENT from the executor;
preserve /go N assertion semantics. Acceptance: malformed and valid
commands preserve facade behavior with no skip semantics.

## What was delivered

### Pure intent boundary (extensions/gsd/command-parser.rkt — still pure)
The executor (command-handlers.rkt) previously re-parsed the trailing
numeric token of `/go N` from the raw input text (`requested-wave-index`).
That intent extraction now lives in the PURE parser layer:

- `command-wave-intent` — trailing numeric token extraction (byte-identical
  semantics to the executor's former `requested-wave-index`; `/go 3` → 3,
  `/go` → #f, `/go 3 extra` → #f).
- `gsd-command-intent` — classifies a parsed command into a neutral intent
  spec: `(go-wave n) | (go-all) | (skip-wave n) | (skip-all) |
  (wave-done-wave n) | (wave-done-unspecified) | (done-force) |
  (done-default) | (plan-text s) | (plan-display) | (display artifact) |
  (status) | (replan) | (reset) | (unknown)`.
- `go-wave-valid?` — pure mirror of the executor-side /go N assertion
  (go-orchestrator `assert-go-n`: requested == earliest actionable wave),
  making the semantics checkable at the intent boundary without touching
  the campaign record.

### Executor consumes the pure intent
`command-handlers.rkt` `requested-wave-index` is now a one-line delegate to
`command-wave-intent` — the executor no longer re-parses. The /go N
rejection flow (`prepare-go-campaign` → `assert-go-n`) is unchanged, so the
assertion semantics are preserved exactly.

### Tests — NEW tests/test-gsd-command-intent.rkt (13 tests, @suite extensions)
- **Command corpus** (30 entries): every valid/alias/malformed input parses
  to the expected kind AND classifies to the expected intent — pins `--force`
  substring behavior, non-numeric args → fallback intents, aliases.
- **Malformed**: unknown cmds → #f + `(unknown)` intent; valid cmd with
  non-slash input still parses (args boundary pinned).
- **command-wave-intent** extraction edge cases (0, multi-token, decimals,
  negatives, whitespace).
- **/go N assertion semantics**: `go-wave-valid?` table + full
  (requested × next) cross-product equivalence with the `assert-go-n`
  definition — no skip semantics.
- **Parser fitness** (require-scan): command-parser.rkt and
  command-helpers.rkt import no I/O modules (no file/port/path/system/...);
  command-parser.rkt imports only `racket/base racket/match racket/string
  util/command-helpers util/command-types`.

## Equivalence evidence

- Golden traces 16/16 UNCHANGED (the executor delegate is
  behavior-preserving).
- GSD surface batch: 1087 tests passed (1038 + intent/corpus additions).
- Existing parser/dispatch/handlers-unit/normalization/go-orchestrator
  suites: 27+18+26+… all green (assert-go-n D8 tests untouched).
- lint-format 2089 files 0/0; Fast suite ✅ 1060 files.

## Notes

1. `parse-gsd-command` keys on the cmd token only — a valid cmd with a
   non-slash input-text still parses (extract-cmd-args returns "" for
   non-slash input). Pinned in the corpus test as the existing contract.
2. `/done --forcex` parses as force (substring match) — legacy behavior,
   pinned intentionally (changing it is out of W3 scope).
3. The W2 post-merge reviewer fold (`f3a6fc87`) rides this branch as a
   separate commit: missing-STATE.md reconcile guard, atomic-write DRY,
   inventory requires column.

## Reviewer acceptance criteria (pre-review checklist)

1. Parser stays I/O-free (fitness test).
2. Intent classification is pure and separate from the executor.
3. Executor consumes the pure intent (requested-wave-index delegate).
4. /go N assertion semantics preserved (assert-go-n untouched; go-wave-valid?
   mirror + cross-product test).
5. Malformed/valid commands preserve facade behavior (corpus pins; golden
   16/16 unchanged).
6. Gates: Fast + lint-format green; inventory consistent (no new modules —
   parser extended in place, deps unchanged).
