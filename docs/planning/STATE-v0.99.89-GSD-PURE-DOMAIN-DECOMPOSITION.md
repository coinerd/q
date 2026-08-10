# State: v0.99.89 — GSD Pure Domain Decomposition

**Status:** ACTIVE — W0 DONE; W1 next
**Baseline SHA:** `598fbd00` (v0.99.88 release)
**Current main:** `8214e6a4` (W0 merge, PR #9254)
**Current wave:** W1 — Pure Transition Kernel (#9227)

| Wave | Issue | Status | Merge | Gates |
|---|---|---|---|---|
| W0 Golden Workflow Traces | #9226 | ✅ DONE (PR #9254, `8214e6a4`) | golden 16/16; workflows 29/161; Fast 1057/15420; lint-format 0/0 | APPROVED (1 recheck round) |
| W1 Pure Transition Kernel | #9227 | NEXT | — | pure-kernel + Arch + Fast |
| W2 Plan/State Projection Kernel | #9228 | backlog | — | GSD governance/workflow + Broad |
| W3 Command Parsing & Intent Boundary | #9229 | backlog | — | parser fitness + command corpus + Fast |
| W4 Facade Thinning + Release | #9230 | backlog | — | Broad + Arch + Workflow + Smoke + Release + review |

## W0 deliverables

- `tests/helpers/gsd-golden-trace.rkt` — deterministic semantic trace
  harness (oracle). Production campaign machinery
  (`execute-campaign-request!` → `run-campaign!` → `try-complete-wave!`),
  nondeterministic fields excluded by construction, plan-id deterministic.
- `tests/workflows/gsd/test-gsd-golden-traces.rkt` — 16-test matrix
  (plan-creation, go-success, verifier-reject, failure, interruption,
  retry, resume, replan, milestone-close via production `/done` archive
  path, crash-between-commit-and-projection, crash-resume, determinism
  probes).
- `docs/reports/GSD-GOLDEN-TRACES-v0.99.89.md` — oracle docs + findings.

## Oracle findings (current behavior pins for W1–W4)

1. Interruption updates only the durable record — projections untouched.
2. Crash between commit and projection leaves stale projections; resume
   does not repair them (W2 scope; plan overall reflects stale projection).
3. `campaign-result` `completed` = waves completed in the current run.
4. Campaign path ends FSM in `verifying`; `/done` (milestone close) resets
   to fresh `idle` with cleared history.

## Projection consistency

Tracked scaffolds in `q/docs/planning/`; executor mirror `.planning/`.
STATE/VALIDATION/wave files advance together on every transition.
