# State: v0.99.88 — Extension Host/Runtime Service Boundary

**Campaign status:** ACTIVE — W1 ready for PR/CI
**Plan-ID / Hash:** generated at campaign start
**Baseline SHA:** `0e78c51f` (v0.99.87 release)
**Current main:** `61fc7057` (W0 merge)
**Current wave:** W1 — Neutral Extension Host Service Protocol (#9222)
**Next:** open W1 PR, wait for CI, squash merge; then W2 provider-registry
service isolation from `extensions/context.rkt`

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE | #9249 | `61fc7057` | ✅ APPROVED (2 passes) | characterization 12/12; focused 257/257; Fast 15369 |
| W1 | READY | — | — | ✅ APPROVED | protocol 11/11; focused 76/76; Arch 231/231; Fast 15382 |
| W2 | PENDING | — | — | — | — |
| W3 | PENDING | — | — | — | — |
| W4 | PENDING | — | — | — | — |

## Finding state

MA-02 PARTIAL; MA-03 PARTIAL (neutral protocol + Runtime adapter implemented;
context migration remains W2); MA-04 OPEN; MA-05 OPEN.

## Projection consistency

Tracked scaffolds live in `q/docs/planning/`; executor mirror in `.planning/`.
STATE/VALIDATION/wave files advance together on every transition.
