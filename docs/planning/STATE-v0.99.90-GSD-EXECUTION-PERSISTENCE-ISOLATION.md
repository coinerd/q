# State: v0.99.90 — GSD Execution, Persistence & Campaign Reliability Isolation

**Campaign status:** RELEASED v0.99.90 — W0..W5 DONE (W0 `53801aab` PR #9259; W1 `f8f2f99d` PR #9260; W2 `371fdf4e` PR #9261; W3 `848d8bda` PR #9262; W4 `587c8b65` PR #9263; W5 `9e1741de` PR #9264, #9236 closed; release PR #9265 merged `bb0cc98c`)
**Plan-ID / Hash:** frozen plan `PLAN-v0.99.90-GSD-EXECUTION-PERSISTENCE-ISOLATION.md`
**Baseline SHA:** `93619627` (v0.99.89 release); main now `bb0cc98c`
**Current wave:** none — campaign COMPLETE
**Next:** v0.99.91 (#878) Provider Contract & Test Hardening (Path B)

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE (PR #9259, `53801aab`) | — | — | ✅ APPROVED (recheck 0 findings) | ports + composition root + fakes; Fast 1063/15500; golden 16/16; Arch 22/22 |
| W1 | ✅ DONE (PR #9260, `f8f2f99d`) | — | — | ✅ APPROVED (2 MINOR folded) | campaign-repository boundary: fail-closed schema/fencing/plan/attempt + containment/no-follow/atomic; Fast 1064/15525; golden 16/16 |
| W2 | ✅ DONE (PR #9261, `371fdf4e`) | — | — | ✅ APPROVED (0 findings; INFO-3 doc dedup folded) | atomic projection transaction: durable-first completion + reconcile-completion-outbox!; Fast 1065/15532; Broad 1251/17799 |
| W3 | ✅ DONE (PR #9262, `848d8bda`) | — | — | ✅ APPROVED (0 findings; INFO-2 comment folded) | wave executor isolation: wave-runner-port.rkt (outcome/port contracts) + run-wave-with-timeout + outcome-kind switch + #:timeout-sec; inventory 33→34; Fast 1067/15554; Broad 1253/17821 |


| W5 | PENDING | — | — | — | — |

MA-06 PARTIAL, MA-07 OPEN, MA-08 OPEN. Terminal closure requires W5 recovery/release evidence.
