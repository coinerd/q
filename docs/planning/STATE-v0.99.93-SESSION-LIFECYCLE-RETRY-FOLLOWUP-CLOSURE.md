# State: v0.99.93 — Session Lifecycle & Retry Follow-up Closure

**Campaign status:** IN PROGRESS — W0 candidate verified
**Plan-ID / Hash:** `bef99c6431462869fc9e750a05cd834e13b921a2a3070af557f07c6b50856413`
**Baseline SHA:** v0.99.92 release `128f825977a46f7b1c452f245dcac96f4d8f7726`
**Current wave:** W0 REVIEWED — PR pending
**Next:** merge #9276, then W1 #9279 compaction start-event ownership

| Wave | Issue | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|---|
| W0 | #9276 | REVIEWED / PR PENDING | — | — | REQUEST_CHANGES 0M/1m/1i; MINOR corrected | RED 1/8; GREEN 8/8; focused 35/35; Fast 1080/15668; lifecycle 566→563 LOC |
| W1 | #9279 | PENDING | — | — | — | — |
| W2 | #9281 | PENDING | — | — | — | — |
| W3 | #9277 | PENDING | — | — | — | — |
| W4 | #9280 | PENDING | — | — | — | — |
| W5 | #9278 | PENDING | — | — | — | — |
| W6 | — | PENDING | — | — | — | release v0.99.93 |

All six waves carry a W4 (#9246) deferred finding (W0-F1…F5, W3-F2). A wave cannot close with a failing focused/Fast gate or an unassigned finding. W5 (close/active-prompt concurrency) is a separate concurrency milestone with a Broad gate; W6 is the release wave (Broad + full gate matrix).
