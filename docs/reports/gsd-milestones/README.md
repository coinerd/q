# GSD milestone truth projections

`v0.99.52.json` is the current canonical projection. Its digest must be supplied independently to the evaluator; a digest stored beside or computed by the projection does not authenticate it.

`v0.99.51-historical.json` preserves the authentic mechanical/substantive split discovered by the independent post-release audit. It is pinned in tests at canonical projection SHA-256 `f6e409f9a9757ddc68d442667e86f58673f824d60dbc9be13ca0e86def6a2ba3`.

Its evidence digests correspond to retained bytes, not placeholders:

| Contract evidence | Retained source | SHA-256 |
|---|---|---|
| release stages | `docs/reports/gsd-milestones/v0.99.51-evidence/release-asset-verification.log` | `b3b4accb296f8dcf9a6b8e28d8d9c980ad22b0a3df400a6d2d7dd78c7ebce2a1` |
| W0 | `docs/reports/gsd-wave-validation/v0.99.51-w0.rktd` | `cb211b67b9661a7d32a2f03c36c7e74d29a0141fb5dc2c071f3b67f890bfd464` |
| W1 | `docs/reports/gsd-wave-validation/v0.99.51-w1.rktd` | `f281adad5672852d7e4a6382ac4b0e91570acc7acf44c43092cfae751d811e04` |
| W2 | `docs/reports/gsd-wave-validation/v0.99.51-w2.rktd` | `cf0078f6d08a72193b87ae88bf4edfdaba5a1d61aba28b766f7b19b1cceb4292` |
| W3 | `docs/reports/gsd-wave-validation/v0.99.51-w3.rktd` | `013a940b9e5b1f3326628a878511f25036c64e35b00dfe494eb89fbdde01c1dd` |
| W4 | `docs/reports/gsd-wave-validation/v0.99.51-w4.rktd` | `1e798fbf333a139c4b35af894a5b060252386469533bfed7cf98b2513877d0a6` |
| W5 | `docs/reports/gsd-wave-validation/v0.99.51-w5.rktd` | `7c1285ada28b528311fcd507bd9ef907b329266d182432bf8435be53b10863ea` |
| retained W6 validation (contradictory mechanical-era evidence; not accepted criterion evidence) | `docs/reports/gsd-wave-validation/v0.99.51-w6.rktd` | `bc9d1c3e5265171829aeeccfdf02fac5c7b5a1276f21640d3b8ac9bd0e8ff30f` |
| W7 | `docs/reports/gsd-wave-validation/v0.99.51-w7.rktd` | `b983d2dea1809f52b7e435d1e7dad01ff325c596d50fa0b0008680b6a9fb85c1` |
| W8 | `docs/reports/gsd-wave-validation/v0.99.51-w8.rktd` | `44deaebb6f807380ba8626325664c32d651766eed3eb03b9fb2eecd318092499` |
| W9 and validation | `docs/reports/gsd-milestones/v0.99.51-evidence/current-deterministic-gates.log` | `3759f04b376cf00a77d1264d5913839a8f1e330833308155cbe1fc045ba8d2a6` |
| independent review | `docs/reports/gsd-milestones/v0.99.51-evidence/AUDIT-v0.99.51-POST-RELEASE-IN-DEPTH.md` | `5ab0fc18dd6c38fb470d62639d9e9f73aa19fe4bebcea5fb3c9d0a5342b6ccae` |

The retained W6 file is authenticated because it records the mechanical-era validation state, but it explicitly leaves the required all-nine real-provider acceptance campaign outstanding. Accordingly, historical W6 is `in-progress` with an unmet criterion and null criterion evidence; the retained file/hash is contradictory context, not evidence that the criterion was met.

The three originally external audit artifacts above are copied byte-for-byte into this immutable report directory so CI can recompute their hashes. Historical evidence derives and establishes `published/rejected`; it must never be interpreted as accepted.

The JSON Schema documents the portable shape. `scripts/gsd-milestone-truth.rkt` is the authoritative validator for runtime-only ordered W0–W10, unique IDs, and cross-field invariants.
