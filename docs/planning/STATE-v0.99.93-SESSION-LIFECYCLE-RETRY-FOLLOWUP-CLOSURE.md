# State: v0.99.93 — Session Lifecycle & Retry Follow-up Closure

**Campaign status:** IN PROGRESS — W0 DONE; W1 active
**Plan-ID / Hash:** `bef99c6431462869fc9e750a05cd834e13b921a2a3070af557f07c6b50856413`
**Baseline SHA:** v0.99.92 release `128f825977a46f7b1c452f245dcac96f4d8f7726`
**Current wave:** W1 REVIEWED — PR pending
**Next:** merge #9279, then W2 #9281 rollback prompt-scope extraction

| Wave | Issue | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|---|
| W0 | #9276 | ✅ DONE | #9286 | 4c21962e | production APPROVED; 0M/1m/1i, MINOR evidence typo corrected | RED 1/8; GREEN 8/8; focused 35/35; Fast 1080/15668; lifecycle 566→563 LOC; CI 17/17; policy NONE unmet |
| W1 | #9279 | REVIEWED / PR PENDING | — | — | APPROVED final 0M/0m | reducer RED 70/1→GREEN 71/71 (114 checks); runtime RED 7/3→GREEN 10/10; final focused 127; adjacent 78; Fast 1080/15676 |
| W2 | #9281 | PENDING | — | — | — | — |
| W3 | #9277 | PENDING | — | — | — | — |
| W4 | #9280 | PENDING | — | — | — | — |
| W5 | #9278 | PENDING | — | — | — | — |
| W6 | — | PENDING | — | — | — | release v0.99.93 |

All six waves carry a W4 (#9246) deferred finding (W0-F1…F5, W3-F2). A wave cannot close with a failing focused/Fast gate or an unassigned finding. W5 (close/active-prompt concurrency) is a separate concurrency milestone with a Broad gate; W6 is the release wave (Broad + full gate matrix).
