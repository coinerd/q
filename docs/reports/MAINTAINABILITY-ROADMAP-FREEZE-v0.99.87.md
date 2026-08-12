# Maintainability Roadmap Freeze — v0.99.87 W4

**Issue:** #9216 · **Milestone:** #874
**Evidence baseline:** `274de500e86666b54c10bacda09e99d006bd5501`
**Freeze candidate:** `d18c6898ded4086aa534316d167de184fdb6ec5a` (v0.99.87 W3 merge)
**Machine contract:** `docs/architecture/maintainability-roadmap-freeze-v0.99.87.rktd`
**Authority:** `.planning/Racket-Wartbarkeits-und-Architekturanalyse-q-Coding-Agent.md`

## 1. Freeze decision

The 32-wave series is frozen as five completed v0.99.87 waves plus 27
follow-up waves: v0.99.88 (5), v0.99.89 (5), v0.99.90 (6), v0.99.91 (5),
and v0.99.93 (6). Wave IDs, order, goals, scope, gates, and acceptance become
immutable when each campaign starts. Any substantive change requires a dated
amendment, a new plan hash, controlled campaign migration, and retained
finding history. New findings use the Scope Stop (`IN_SCOPE`, `DEFERRED`, or
`SEPARATE_MILESTONE`); they are never silently folded into a running wave.

### v0.99.91 decision gate: **PATH B — Provider Contract & Test Hardening**

Path A required at least two semantically identical primitives with repeated
co-change evidence. W3 found only C9 with one notable top-five provider
co-change pair; C9 is additionally blocked by G1 status-threshold divergence.
C10–C15 have no notable repeated co-change history. The threshold is therefore
not met. The frozen v0.99.91 wave IDs are `W0`, `W1-B`, `W2-B`, `W3-B`, and
`W4-B`. Path-A extraction waves must not appear in its canonical plan.

## 2. Master finding traceability (12/12 assigned)

Status vocabulary: `CLOSED`, `GUARDED`, `PARTIAL`, `OPEN`, `REJECTED`.
`GUARDED` means the implementation is closed but remains a mandatory
regression invariant through series completion.

| ID | Status | Accountable domain | Frozen target | Evidence / merge | Terminal closure proof | Release |
|---|---|---|---|---|---|---|
| MA-01 | CLOSED | Architecture governance | .87 W0/W4 | W0 baseline; `ca84d20a` | deterministic raw + Markdown baseline, reconciliation, review, release record | v0.99.87 |
| MA-02 | PARTIAL | Extensions boundary | .88 W2–W4 | pair policy + fitness; `7b62932a` | every dated exception removed or pair-precisely re-approved; none stale/expired | v0.99.88 |
| MA-03 | PARTIAL | Extension/runtime provider service | .88 W0–W2 | baseline + pair policy | neutral registry service; Runtime import/exception removed; differential/dynamic-load proof | v0.99.88 |
| MA-04 | OPEN | Extension package lifecycle | .88 W3 | package exception; `7b62932a` | adapter/capability removal or narrow evidence-backed waiver; package/security gates | v0.99.88 |
| MA-05 | OPEN | TUI/Extension bridges | .88 W4 | three pair exceptions + fitness | individual remove-or-permanent-waiver decision with headless/smoke evidence | v0.99.88 |
| MA-06 | PARTIAL | GSD domain architecture | .89 W0–W4; .90 W0–W5 | 26/26 inventory; `3bf783c2` | Golden Traces, I/O-free pure cores, explicit effect ports, facade compatibility | v0.99.90 terminal (.89 intermediate) |
| MA-07 | OPEN | GSD projections/persistence | .89 W0/W2; .90 W1/W2/W5 | state/effect map + drift history | pure projection + atomic application survives crash injection without invented DONE/skip | v0.99.90 |
| MA-08 | OPEN | GSD execution adapters | .90 W0–W5 | effect inventory; `3bf783c2` | cohesive ports/fakes; explicit outcomes; idempotent retries; no duplicate external effect | v0.99.90 |
| MA-09 | PARTIAL | LLM provider adapters | .91 Path B W0–W4-B | C1–C23/P1–P8; `d18c6898` | complete contracts/fixtures/ownership guards; no capability regression; terminal no-abstraction decision | v0.99.91 |
| MA-10 | OPEN | Runtime session lifecycle | .92 W0–W5 | 600-LOC baseline + reassessment | trace-equivalent extraction measurably improves locality, or evidence-backed rejection | v0.99.93 |
| MA-11 | GUARDED | Agent iteration boundary | .92 W4/W5 guard | boundary cleanup `274de500` + fitness | zero `agent/iteration` → Runtime implementation imports through final release | v0.99.87 baseline; guard through .92 |
| MA-12 | GUARDED | Runtime session state ownership | .92 W2/W4/W5 guard | session-state series + `0bbed34c` fitness | parameter/session-ownership gates prove no hidden cross-turn side channel | v0.99.87 baseline; guard through .92 |

**Assignment result:** 12 unique IDs, 12 owners, 12 non-empty closure proofs,
12 release assignments, zero unassigned findings.

### Evidence corrections frozen from W2/W3

- MA-08 is not a claim that the 26 GSD modules currently perform GitHub or
  network effects: W2 found none. The live internal effects are filesystem,
  process/git (one `git` invocation in `plan-context-builder.rkt`), and event
  projection. v0.99.90 also covers external campaign/release orchestration at
  its explicit adapter boundary.
- MA-09 does not authorize a shared provider base abstraction. The W3
  rejection list C16–C23 remains normative; G1–G3 are explicit asymmetries.

## 3. Frozen follow-up campaign map

| Milestone | Waves | Broad gates | Terminal finding(s) | Release acceptance |
|---|---:|---|---|---|
| v0.99.88 Extension Host/Runtime Service Boundary (#875; #9221–#9225) | W0–W4 (5) | W2, W4 | MA-02–MA-05 | five old exceptions closed/newly decided; none expired |
| v0.99.89 GSD Pure Domain Decomposition (#876; #9226–#9230) | W0–W4 (5) | W2, W4 | MA-06 intermediate, MA-07 intermediate | pure transition/projection/parsing domains; Golden Traces equivalent |
| v0.99.90 GSD Execution/Persistence Isolation (#877; #9231–#9236) | W0–W5 (6) | W2, W5 | MA-06–MA-08 | truth/projections agree after every crash; no duplicate effect |
| v0.99.91 Provider Contract & Test Hardening (#878; #9237–#9241) | W0, W1-B–W4-B (5) | W2-B, W4-B | MA-09 | complete provider contracts; no capability regression or unsupported abstraction |
| v0.99.93 Lifecycle/Series Closure (#879; #9242–#9247) | W0–W5 (6) | W2, W5 | MA-10; guards MA-11/12 | reassessment, 100% final traceability, zero stale tracking, verified release |

The exact goals, gates, and acceptance text for all 27 waves are stored in the
machine contract and projected into versioned PLAN/STATE/VALIDATION skeletons
and the five reviewed GitHub specs under `docs/planning/` (tracked,
reviewable). The same content is mirrored to the executor's local
`.planning/` working set, which is git-ignored by design (`.gitignore` §
Internal planning) and therefore is not itself a versioned source.

Each follow-up milestone is additionally materialized on GitHub as an open
milestone with its wave issues in Backlog: see §3 for #875–#879 / #9221–#9247.

## 4. Baseline review inputs

| Wave | Artifact | Closure state |
|---|---|---|
| W0 | reproducible architecture `.rktd` + Markdown and baseline fitness | reviewer APPROVED; merged `ca84d20a` |
| W1 | pair-precise dependency policy and exception fitness | reviewer APPROVED; merged `7b62932a` |
| W2 | 26-module GSD responsibility/effect inventory and fitness | reviewer APPROVED; merged `3bf783c2` |
| W3 | provider duplication/parity matrix C1–C23 and P1–P8 | reviewer APPROVED; merged `d18c6898` |
| W4 | this freeze contract/report, planning scaffolds, release gates | independent review required before merge/release |

## 5. Release and rollback controls

v0.99.87 is an independently releasable evidence baseline. It introduces no
shared provider abstraction or broad architecture migration. Rollback is the
W4 PR revert plus tag/release withdrawal before any v0.99.88 campaign starts;
W0–W3 evidence remains valid at its pinned SHA.

The release must be tagged only after W4 is squash-merged and `main` is clean.
The annotated `v0.99.87` tag must dereference to the exact merge SHA. Broad,
Arch, lint-format, release-dry-run, Smoke/release-smoke, required CI, manifest,
bundle, and public asset verification are closure evidence—not assumptions.
