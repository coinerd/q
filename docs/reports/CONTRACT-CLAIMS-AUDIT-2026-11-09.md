# Contract-Claims Audit — 2026-11-09 (delivered 2026-11-10)

Status: DELIVERED AS REAL PROTECTED WORK — ordinary protected wave per the
owner's 2026-11-10 decision (canary W-b content, decoupled from canary
semantics; see `PLANNING-RECONCILIATION-2026-10-31.md` header for the
delivery history). This audit verifies three contract claims against the
shipped source, corrects nothing (all three verify `correct-as-shipped`),
and records the examined evidence for each.

## Claim 1 — tracker reconciliation is DONE-only, not auto-reopen

**Verdict: correct-as-shipped.**

- Source of truth: `extensions/gsd/tracker-reconciliation.rkt`,
  `reconcile-tracker-after-delivery!` — the reconciliation is a `let/ec`
  fail-closed ladder that returns `blocked` before any GitHub effect unless
  the durable campaign wave satisfies, in order: record exists, wave found,
  `(eq? (campaign-wave-status wave) 'done)` (line 190: `"durable campaign
  wave is not DONE"`), journal receipt matches the wave, authenticated
  delivery readback is `delivered`, and the delivered handoff carries the
  matching merge SHA. Only then does `execute-tracker-commands!` run.
- There is no code path that opens, reopens, or status-flips an issue for a
  wave that is not `done`; `run-tracker-reconciliation!`'s callers log a
  warning and leave the wave `done` regardless
  (`extensions/gsd/delivery-finalize.rkt`: "reconciliation is an output of
  delivery, so its refusal can never move this wave off `done`").
- Restart sweep: `extensions/gsd/tracker-resume.rkt` processes only waves
  `(eq? (campaign-wave-status wave) 'done)` whose binding matches — a
  historical DONE wave without the opt-in is invisible ("causes no network
  read, no journal change and no tracker write").

## Claim 2 — restart convergence repeats only idempotent mutations

**Verdict: correct-as-shipped.**

- Source of truth: `extensions/gsd/tracker-resume.rkt` header contract:
  "Status/close updates are externally idempotent; this module NEVER treats
  tracker state as proof." The sweep re-runs `run-tracker-reconciliation!`
  for matching DONE waves; the adapter re-authenticates item identity
  through the GraphQL readback before any mutation
  (`extensions/gsd/gh-cli-tracker-adapter.rkt` — the option-membership gate
  shipped by the v1.00.33 remediation also refuses pre-mutation when the
  configured option is not a member of the bound field).
- The repeatable mutations are exactly: project single-select `Status` set
  to the bound value and issue close — both externally idempotent
  (re-setting `Done` on a `Done` item and closing a closed issue are no-ops
  with new audit entries, not state changes). No comment, label, or reopen
  mutation exists in the adapter's command set.

## Claim 3 — default-OFF configuration ≠ effective loaded runtime configuration

**Verdict: correct-as-shipped.**

- Source of truth: `runtime/settings-core.rkt` `load-settings` merges the
  global config (`~/.q/config.json`) with the project config
  (`<project>/.q/config.json`) into `q-settings-merged`; consumers resolve
  effective values through `setting-ref*` against the merged result, with
  per-key defaults applied only when the merged config omits the key.
- Concrete witness: `tools/builtins/bash.rkt` `resolve-exec-limits` —
  `(or timeout-arg (and settings (sandbox-timeout settings))
  DEFAULT-TIMEOUT-SECONDS)` — the effective timeout is the sandbox-timeout
  setting when present, else the 120 s default: a default-OFF key yields the
  default, not the absence of the behavior. Likewise
  `tools/permission-gate.rkt` distinguishes `make-strict-permission-config`
  (hard-deny default) from `make-permissive-permission-config` (the explicit
  `--auto-approve` opt-in) — the config key `auto-approve` selects the
  latter; absence selects the former.
- The engine's wave-executor applies the same pattern for
  `gsd.worktree-isolation` (`'auto` = honor the project settings key,
  resolved once per campaign).

## Corrections

None required. A repo-wide search for wrong claims about these three
semantics (`rg "reconcil.*reopen|auto.*reopen|reopen.*undelivered"` over
`docs/` and `extensions/`) returned no candidate claiming auto-reopen,
non-idempotent restart mutation, or default-OFF/loaded-config equivalence.
The three claims ship correctly in code, header contracts, and docstrings.

## Evidence index

- `extensions/gsd/tracker-reconciliation.rkt` (DONE gate at line 190; fail-
  closed ladder 167–209)
- `extensions/gsd/tracker-resume.rkt` (header contract lines 5–10; DONE-only
  sweep filter)
- `extensions/gsd/gh-cli-tracker-adapter.rkt` (GraphQL identity readback +
  option-membership pre-mutation gate)
- `extensions/gsd/delivery-finalize.rkt` (reconciliation non-fatal after DONE)
- `runtime/settings-core.rkt` (`load-settings` merge; `setting-ref*`)
- `tools/builtins/bash.rkt` (`resolve-exec-limits` default fallback)
- `tools/permission-gate.rkt` (strict vs permissive policy modes)
