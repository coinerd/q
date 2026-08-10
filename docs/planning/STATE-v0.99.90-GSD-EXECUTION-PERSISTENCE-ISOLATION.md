# State: v0.99.90 — GSD Execution, Persistence & Campaign Reliability Isolation

**Campaign status:** ACTIVE — W0 + W1 DONE (W0 `53801aab` PR #9259; W1 `f8f2f99d` PR #9260, #9232 closed)
**Plan-ID / Hash:** frozen plan `PLAN-v0.99.90-GSD-EXECUTION-PERSISTENCE-ISOLATION.md`
**Baseline SHA:** `93619627` (v0.99.89 release); main now `f8f2f99d`
**Current wave:** none — W0/W1 complete
**Next:** W2 Atomic Projection Transaction (#9233)

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE (PR #9259, `53801aab`) | — | — | ✅ APPROVED (recheck 0 findings) | ports + composition root + fakes; Fast 1063/15500; golden 16/16; Arch 22/22 |
| W1 | ✅ DONE (PR #9260, `f8f2f99d`) | — | — | ✅ APPROVED (2 MINOR folded) | campaign-repository boundary: fail-closed schema/fencing/plan/attempt + containment/no-follow/atomic; Fast 1064/15525; golden 16/16 |
| W1 | PENDING | — | — | — | — |
| W2 | PENDING | — | — | — | — |
| W3 | PENDING | — | — | — | — |
| W4 | PENDING | — | — | — | — |
| W5 | PENDING | — | — | — | — |

MA-06 PARTIAL, MA-07 OPEN, MA-08 OPEN. Terminal closure requires W5 recovery/release evidence.
