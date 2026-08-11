# State: v0.99.91 — Provider Contract & Test Hardening (PATH B)

**Campaign status:** IN PROGRESS
**Approved path:** PATH_B (immutable)
**Plan-ID / Hash:** PENDING_AT_START
**Baseline SHA:** v0.99.90 @ b006ff08 (released)
**Current wave:** W3-B DONE
**Next:** W4-B (Provider Hardening Reassessment and Release, #9241)

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE | #9267 | b3a229c2 | APPROVED (0M/3m/5i, MINORs folded) | golden matrix 5/5, focused 452, Fast 1070/15587 |
| W1-B | ✅ DONE | #9268 | bd6b8107 | 0M/2m/2i; MINORs folded | typed unsupported bijection + 4 real-parser cases; focused 461; Fast 1071/15591; CI 17/17 |
| W2-B | ✅ DONE | #9269 | 307bdec1 | 0M/3m/1i; all findings folded | v1 20-cell byte/JSON corpus; focused 507; Fast 1072/15599; Broad 1250/17866; Security 64/710; CI 17/17 |
| W3-B | ✅ DONE | #9270 | c822efc0 | APPROVED after 2 remediation rounds (0M/0m/4i final) | probes 10/10; focused arch 99; Arch 23/248; Fast 1073/15609; CI 17/17 |
| W4-B | PENDING | — | — | — | — |

MA-09 PARTIAL. C16–C23 rejection list remains normative. G1–G3 are explicit asymmetries, not silently normalized defects.
