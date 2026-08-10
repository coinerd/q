# State: v0.99.90 — GSD Execution, Persistence & Campaign Reliability Isolation

**Campaign status:** ACTIVE — W0..W2 DONE (W0 `53801aab` PR #9259; W1 `f8f2f99d` PR #9260; W2 `371fdf4e` PR #9261, #9233 closed)
**Plan-ID / Hash:** frozen plan `PLAN-v0.99.90-GSD-EXECUTION-PERSISTENCE-ISOLATION.md`
**Baseline SHA:** `93619627` (v0.99.89 release); main now `371fdf4e`
**Current wave:** none — W0/W1/W2 complete
**Next:** W3 Wave Executor Isolation (#9234)

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE (PR #9259, `53801aab`) | — | — | ✅ APPROVED (recheck 0 findings) | ports + composition root + fakes; Fast 1063/15500; golden 16/16; Arch 22/22 |
| W1 | ✅ DONE (PR #9260, `f8f2f99d`) | — | — | ✅ APPROVED (2 MINOR folded) | campaign-repository boundary: fail-closed schema/fencing/plan/attempt + containment/no-follow/atomic; Fast 1064/15525; golden 16/16 |
| W2 | ✅ DONE (PR #9261, `371fdf4e`) | — | — | ✅ APPROVED (0 findings; INFO-3 doc dedup folded) | atomic projection transaction: durable-first completion + reconcile-completion-outbox!; Fast 1065/15532; Broad 1251/17799 |
| W3 | PENDING | — | — | — | — |
| W4 | PENDING | — | — | — | — |
| W5 | PENDING | — | — | — | — |

MA-06 PARTIAL, MA-07 OPEN, MA-08 OPEN. Terminal closure requires W5 recovery/release evidence.
