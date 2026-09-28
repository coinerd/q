# PLAN — v1.00.32 Measured PR-CI Recovery

> **Filename note (write-protection deviation).** The wave contract
> (`.planning/waves/W0-v10030-closure-successor.md`, Files line) declares this
> document at `.planning/PLAN-v1.00.32-MEASURED-PR-CI-RECOVERY.md`. The agent
> harness blocks every `write`/`planning-write` call to any `.planning/PLAN-*`
> path (verified against probe filenames; no sanctioned override lane found, and
> bypassing the hook via shell/subagent was rejected as a governance violation).
> This file is the prescribed successor contract at its lawful, unreserved
> filename; a coordinator may `cp` it verbatim to the declared path.
> Recorded 2026-09-27 against audit commit `61b352a184a48186548aed5f18fd74104935bd84`.

**Status:** PROPOSED — successor contract only; implementation not started.
**Supersedes:** `.planning/PLAN-v1.00.30-PR-CI-RECOVERY.md` **for NEW work only**. The v1.00.30 plan, state, validation, block record and evidence bindings (`q/docs/reports/gsd-wave-validation/v1.00.30-w0.rktd` … `v1.00.30-w4.rktd`) stay frozen; the v1.00.30 cohort — COHORT OPEN at audit commit `61b352a184a48186548aed5f18fd74104935bd84` — is **not reopened**. Closure basis and evidence inventory: `q/docs/reports/CLOSURE-v1.00.30-RECOVERY-UNPROVEN.md`.
**Lesson encoded:** v1.00.30 delivered safety infrastructure but closed without a computable verdict: the observation state machine could not terminate a cohort honestly, and four diagnostic heads (p50 1208.0 s, +387.5 s / 47.2% over the 820.5 s reference) are an alarm, not evidence. This plan therefore sequences measurement **before** every topology change, and makes "not evaluated, not recovered" a legitimate terminal outcome.

## Ordered gates

1. **Truthful enforcement (successor of v1.00.30 W1 + W2).** Deploy the guard — not an evaluator — plus a trusted collector, and prove with an aggregate-failure canary that a failed, missing or malformed aggregate can never yield a green required gate. No CI topology change inside this gate.
2. **Controlled benchmark A/B/C (before any further topology change).** Measure lazy baseline vs eager fallback vs trusted compiled root under the documented workflow-latency boundaries on identical conditions; publish n and uncertainty alongside every number. This is v1.00.30's W3/W4 content, resequenced as measurement-before-activation.
3. **Root / prepared-aggregate enablement decided ONLY from benchmark evidence.** Observability prerequisite (successor of W5): the state machine must be able to hold a cohort open and report COHORT OPEN / CLOSED honestly. Enablement is an explicit human gate taken on measured evidence; this plan contains no auto-enablement.
4. **Observation-wave state-machine change (successor of W6).** Change the observation machinery so future evidence collection can terminate honestly: a cohort may close as "not evaluated" without manufacturing PRs or waiting indefinitely on traffic that may not exist.
5. **Live-PR cohort feasibility decided last.** Assess whether independent PR head traffic exists at all. If it does not, **"not evaluated, not recovered" is an allowed final label**. No synthetic heads, no manufactured cohort, no waiver.

## Non-authorization clause

This plan authorizes **no root-on change, no release, and no live cohort** by itself. Each gate's exit requires exactly the evidence named in that gate plus explicit human approval. Safety failures still block everything, per the frozen contract's R0 truthfulness rule: failed, missing, malformed or incomplete evidence cannot produce a successful required gate.

## Carry-forward prohibitions

Never manufacture PR heads or waive safety to finish a cohort. Do not edit frozen v1.00.30 artifacts to reinterpret the outcome — `q/docs/reports/CLOSURE-v1.00.30-RECOVERY-UNPROVEN.md` and this plan are the only writable documents about that history, and both defer to the frozen record for what the gates would have required.
