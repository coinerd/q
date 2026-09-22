# Delivery-Tool API Contract — v1.00.31 W4

Wave: W4 — Delivery-tool GitHub API contract correctness (milestone #896)
Register rows closed: **F6** (resolver head filter), **F11** (unsatisfiable approval gate)
Recorded live contract: `artifacts/wave-delivery-integrity/v1.00.31-w4/api-contract.json`
Approval contract record: `artifacts/wave-delivery-integrity/v1.00.31-w4/approval-contract.json`
Red-first evidence: `artifacts/wave-delivery-integrity/v1.00.31-w4/raw/red-first-fixtures.txt`

## F6 — PR-by-branch resolver head filter

The resolvers constructed `pulls?…&head={owner}/{repo}:{branch}`. The live API
matches nothing for that form, so `resolve_existing_pr`/`resolve_merged_pr`
always returned none and the `governance` ladder action could never succeed.
Live probes (2026-09-22, `raw/f6-head-filter-probe-20260922.json`):

| branch | `head=coinerd/q:{branch}` (tool, pre-fix) | `head=coinerd:{branch}` (real contract) |
|---|---|---|
| `campaign/v1.00.31-w3` | 0 results | PR #9738 |
| `binding/fb67e0429ed1-w3` | 0 results | PR #9743 |
| `binding/fb67e0429ed1-w2` | 0 results | PR #9737 |

Fix: `branch_owner(slug)` + both resolvers issue `head={owner}:{branch}`.
Verified live after the fix by the coordinator-owned recorder
(`raw/record-api-contract.py` → `api-contract.json`):

- `resolve-pr` (open-set) resolves against the real API;
- `binding-resolve-pr` resolves binding PR #9743 at head `a9ed5989f…`;
- **`governance` now succeeds end-to-end on the real W3 binding branch**:
  `{"status": "governed", "pr": 9743, "publication-sha": "03f63857…fd8"}` —
  the exact ladder action F6 had permanently disabled.

Route audit: every constructed route (`pulls`, `pulls/{n}`,
`pulls/{n}/merge`, `commits/{sha}`, `commits/{sha}/pulls`,
`commits/{sha}/check-runs` (paginated, commit-scoped), `actions/runs/{id}`,
`branches/main/protection`) keeps its documented shape with fail-closed
accessors (`require` + `dig`); the pulls head filter was the sole malformed
route. The test fakes previously registered the same wrong key, so the suite
mirrored the bug; all fake keys now use the real `owner:branch` form, and
`tests/test-gsd-delivery-api-contract.py` asserts the tool's issued routes are
exactly the recorded live-contract routes (fixture fidelity).

## F11 — amended approval contract in the merge gate

`merge()` demanded a native GitHub review by a non-author human at the exact
head. GitHub refuses author self-approval and this repository has one account,
so the gate was unsatisfiable by construction: the ladder stop-typed
`awaiting-review` forever (observed on v1.00.30 W3, v1.00.31 W0 and the W3
binding PR #9743 with 20/20 checks green). Implemented instead, per the plan's
amended Approval contract:

1. **Recorded operator authorization** — the evidence record's
   `merge-authorization` object (`operator`, `wave`, `head`, `action`,
   `source`, all non-empty; `wave` matches; `head` equals the verified
   implementation head recorded by the same evidence; and the excluded-paths
   digest from the PR base to that receipt equals the digest to the merge tip,
   so the merge delivers exactly the authorized content).
2. **APPROVED independent non-author review artifact** bound to the verified
   implementation head (verdict APPROVED; reviewer non-empty and ≠ PR author;
   `reviewed-sha` = the receipt head — the F3 binding).

Typed refusals: `no-operator-authorization`, `no-review-artifact`,
`head-binding-mismatch`. The unsatisfiable second-account requirement is gone;
no other merge semantics (required checks, protection snapshot, squash-only
PUT, single-parent provenance, merge-SHA binding) changed. Accepted residual
risk (operator-owned): independence is satisfied by an independent agent
reviewer rather than a separate human colleague.

Head-binding design note: a committed record cannot contain its own commit
SHA, so "authorized head == PR head" is enforced where it is satisfiable —
against the receipt head (`implementation-sha`) named by the same evidence,
plus content-equality (digest) between that receipt and the merge tip.

## Register extension

F11 was appended consistently to the failure register JSON, the verbatim raw
plan excerpt, the contract document and the reproduction manifest; the frozen
F1–F10 materialization was not retro-edited (append-only) and the W0
`SHA256SUMS` entries were refreshed for the extended files. F6 and F11 are
guarded in `tests/test-wave-delivery-integrity-register.rkt` by the shipped
offline python contract suites, which fail on the unfixed tree (red-first run
retained under `raw/`).
