# State: v0.99.93 — Session Lifecycle & Retry Follow-up Closure

**Campaign status:** IN PROGRESS — W0/W1/W2/W3/W4/W5 DONE; W6 release active
**Plan-ID / Hash:** `bef99c6431462869fc9e750a05cd834e13b921a2a3070af557f07c6b50856413`
**Baseline SHA:** v0.99.92 release `128f825977a46f7b1c452f245dcac96f4d8f7726`
**Current wave:** W3 DONE — #9277 canonical prompt terminal contract
**Next:** W4 #9280 cancellation-aware retry backoff + partial metadata

| Wave | Issue | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|---|
| W0 | #9276 | ✅ DONE | #9286 | 4c21962e | production APPROVED; 0M/1m/1i, MINOR evidence typo corrected | RED 1/8; GREEN 8/8; focused 35/35; Fast 1080/15668; lifecycle 566→563 LOC; CI 17/17; policy NONE unmet |
| W1 | #9279 | ✅ DONE | #9287 | 4d01830b | APPROVED final 0M/0m | reducer RED 70/1→GREEN 71/71 (114 checks); runtime RED 7/3→GREEN 10/10; final focused 127; adjacent 78; Fast 1080/15676; CI 17/17; policy NONE unmet |
| W2 | #9281 | ✅ DONE | #9288 | f3ff43ad | APPROVED after truth/coverage remediation | direct 4; oracle 7/7; focused 118; Arch 29/281; Fast 1081/15682; 11 units/35 edges; CI 17/17; policy NONE unmet |
| W3 | #9277 | ✅ DONE | PR PENDING | APPROVED 0M/2m/3i; MINOR/INFO folded | RED producer 27/4, TUI 71/2; GREEN 156/156; focused 269; lifecycle oracle 8/8; Arch 29/282; Fast 1081/15685; TUI 779 |
| W4 | #9280 | ✅ DONE | PR PENDING | APPROVED 0M/3m/2i; MINOR/INFO folded | RED 2 F5a + 1 F5b unit + 2 integration → GREEN; focused 217; oracle 9/9; Arch 29/283; Fast 1081/15690 |
| W5 | #9278 | ✅ DONE | PR PENDING | APPROVED 0M/2m/4i; MINORs folded | RED 2 F3 + 1 closed-claim → GREEN; focused 201; oracle 10/10; Broad green (flakes isolated); Fast 1081/15694 |
| W6 | — | PENDING | — | — | — | release v0.99.93 |

All six waves carry a W4 (#9246) deferred finding (W0-F1…F5, W3-F2). A wave cannot close with a failing focused/Fast gate or an unassigned finding. W5 (close/active-prompt concurrency) is a separate concurrency milestone with a Broad gate; W6 is the release wave (Broad + full gate matrix).
