# CL5 P2-F revised pilot result — NO-GO

**Run:** [46078773729](https://github.com/coinerd/q/actions/runs/46078773729)  
**Budget:** `CL5-P2F-REV1`, attempt 1 (no repeat)  
**Dispatch/pinned SHA:** `e117e145c57ce224169d0af4fbfca64a2ca11fd2` / `dd5c9995fc13bbc1253995768b2d78c4531e0a60`  
**Artifact:** `cl5-p2f-pilot-rev1` (id `15236718857`, ZIP SHA-256 `7ba7416d4f154b22b26605c85ab2b32880ccd58df496e101fc0a002953638904`)

## Verdict

**NO-GO.** A1, A2, A3, A5, A6 and budget compliance pass. **A4 comparability fails**: one file has
median pilot / CI reference elapsed = **2.108436**, above the pre-registered 2.0 upper bound
while the pooled median ratio is `1.063388`.
Per the pre-registration this is a host-comparability finding about the reference; it is not a pilot
infrastructure failure and is not retryable. No threshold was changed.

## Observed host

Ubuntu 26.04 (Ubuntu 26.04.1 LTS), image `ubuntu26:20261122.9.1`,
x86_64/X64, **AMD EPYC 7763 64-Core Processor**, 4 cores,
16366632 kB memory, kernel `7.0.0-1020-azure`, Racket 8.10 with executable
SHA-256 `7674f17865fa795b21641828d839e689e01a2f17bbdd2b7d71cc59cceafa873c`. The host gate accepted the pre-registered AMD EPYC class;
the exact model is 7763, which matches reference run `46067550544` and differs from the earlier
9V45 production observation. This was disclosed before dispatch.

## Integrity and stability

- One warm-up and five measured invocations all exited 0.
- Every run contained exactly the eight frozen files, 245 passing tests, zero failures, zero
  timeouts and zero missing results.
- Measured wall times (ms): `[200605, 200743, 201552, 200694, 200993]`; median `200743` ms;
  spread `0.004717` ≤ 0.15.
- The residual `malformed binding datum` issue did not appear; it was not retried or bypassed.

## Comparability detail

| File | CI ref (ms) | Pilot median (ms) | Ratio |
|---|---:|---:|---:|
| `tests/test-gsd-evidence-repair-production-contract.rkt` | 154171 | 160976 | 1.044139 |
| `tests/test-full-regression-invocation.rkt` | 145685 | 157724 | 1.082637 |
| `tests/test-gsd-delivery-merge.rkt` | 58211 | 80885 | 1.389514 |
| `tests/test-gsd-auto-review-loop-contract.rkt` | 61957 | 84904 | 1.37037 |
| `tests/test-gsd-supplemental-disposition-store.rkt` | 41702 | 87926 | 2.108436 |
| `tests/test-gsd-delivery-read-ladder.rkt` | 95973 | 94983 | 0.989685 |
| `tests/test-milestone-gate.rkt` | 69708 | 40295 | 0.578054 |
| `tests/test-gsd-go-orchestrator.rkt` | 30013 | 21805 | 0.726519 |

Pooled median ratio `1.063388` is inside
[0.67, 1.5], but a single file outside [0.5, 2.0] is a NO-GO by pre-registration. The outlier is
`tests/test-gsd-supplemental-disposition-store.rkt` at 2.108436.

## Interpretation and held decisions

This result says only that the eight-file workload ran reliably and stably on a validated host but
that per-file comparability to the post-repair production reference is not established on this
surface. It is **not** an efficiency result. P2-E, a selected-arm comparison, production selection,
P1/W3.3 and the 40 % improvement claim remain held. A subsequent infra expansion or narrower
objective requires a fresh owner decision; no new experiment is authorized by this report.

Raw evidence is in `docs/reports/CL5-P2F-REV1-EVIDENCE/` with per-file SHA-256 digests in
`CL5-P2F-PILOT-REV1.json`.
