# GSD Architecture

Internal architecture documentation for the GSD (Get Shit Done) extension.
The operator-facing contract — commands, plan/wave format, the single
campaign-lifecycle diagram and the recovery table — lives in
[the GSD user guide](gsd-guide.md#campaign-lifecycle); this document describes
how the modules implement it. ADR-0011 records the decisions.

## Module Structure

```
extensions/gsd/
├── plan-types.rkt                  # gsd-plan, gsd-wave, gsd-task + normalized plan IR
├── plan-types-parser.rkt           # Markdown plan parsing (index + legacy inline)
├── plan-validator.rkt              # Mechanical plan validation
├── planning-paths.rkt              # ONE planning/wave path grammar + safe path joins
├── planning-artifact-validation.rkt# Shared preflight kernel (standalone /go + validator CLI)
├── planning-artifact-ownership.rkt  # THE authentic ownership-marker predicate (pure read shell)
├── wave-docs.rkt                   # THE required wave-doc header/section contract + I/O + lint
├── wave-status.rkt                 # THE wave-status vocabulary (canonical + legacy aliases)
├── verify-command-grammar.rkt      # THE Verify grammar (spans, reasons, shell-chain splitter)
├── verify-command-format.rkt       # Verify declaration layout; delegates every token decision
├── plan-snapshot.rkt               # Immutable bound snapshots + normalization/drift classes
├── campaign-state.rkt              # Durable campaign record, manifest identity, status aliases
├── campaign-repository.rkt         # Sole persistence authority for campaign transitions
├── campaign-decision.rkt           # ONE pure decide-campaign-action for /go
├── campaign-lease.rkt              # OS advisory lease (no force release)
├── campaign-request.rkt            # Interface-safe /go request value + token registry
├── wave-executor.rkt               # Per-wave lifecycle tracking
├── go-orchestrator.rkt             # The ONE /go effect interpreter (lease, fence, commit)
├── owned-lane.rkt                  # Owned author/verify/review lane adapter
├── delivery-*.rkt                  # Handoff, finalize, coordinator, receipt, journal, verifier
├── evidence-*.rkt / author-*.rkt   # Evidence readiness, repair, author scope/session
├── supplemental-evidence-*.rkt     # Governed supplemental T/K/S evidence lane
├── review-*.rkt                    # Independent review session, dispatch, proof, audit
├── projection-kernel.rkt / projection-effects.rkt # PLAN/wave/STATE projections
├── reconciliation-checkpoint.rkt   # Durable-state reconciliation
├── archive.rkt                     # Completed-plan archival (gsd-command-result)
├── state-machine.rkt               # Planning/execution MODE machine + transitions
├── runtime-state-types.rkt         # Canonical gsd-runtime-state struct
├── command-types.rkt               # gsd-command-result (gsd-ok / gsd-err)
├── command-parser.rkt / command-handlers.rkt # Command surface
├── policy.rkt                      # Unified policy engine (gsd-decide-action)
├── events.rkt / event-structs.rkt  # Event telemetry (gsd.<category>.<action>)
├── core.rkt                        # Dispatch, tool guard, write guard, transactions
├── prompts.rkt                     # Phase prompts; generates the wave-doc template
├── responsibility-inventory.rkt    # Machine-readable module responsibility/effect inventory
└── shared.rkt                      # Small shared helpers (slugify, ...)

extensions/
└── gsd-planning.rkt                # Extension entry point (registers commands/tools)

tools/
└── scratch.rkt                     # ONE trusted per-session scratch allocation
```

## State Machine

**Module:** `extensions/gsd/state-machine.rkt`

The mode machine guards planning versus execution. It is *not* the campaign
lifecycle: campaign routing is one pure decision per `/go` pass (see
[Campaign decision](#campaign-decision) and the single lifecycle diagram in
[the user guide](gsd-guide.md#campaign-lifecycle)).

```
States: idle, exploring, plan-written, executing, verifying

Transitions:
  idle         → exploring
  exploring    → plan-written
  exploring    → idle
  plan-written → executing
  plan-written → exploring
  executing    → verifying
  executing    → exploring
  verifying    → idle
  verifying    → executing
```

All transitions are validated; an invalid transition returns
`(err-result reason from to)`. Mode state is semaphore-protected.

### API

- `(gsm-current)` — Current state symbol
- `(gsm-transition! target)` — Attempt transition, returns ok-result or err-result
- `(gsm-reset!)` — Reset to idle
- `(gsm-valid-next-states)` — List of valid target states from current
- `(gsm-tool-allowed? tool-name)` — Check tool against per-state blocklist
- `(gsm-snapshot)` — Immutable hash of current state + transition history

## Campaign decision

**Module:** `extensions/gsd/campaign-decision.rkt`

`decide-campaign-action` is pure: no I/O, no parameters, no journal writes. It
consumes explicit views (durable record, authenticated delivery facts, lane
facts, runtime capability) and returns exactly one `campaign-decision`:

| Kind | Meaning |
|------|---------|
| `complete` | every wave is done/deferred and delivery-proven |
| `launch` | begin the next actionable wave (implementation effect) |
| `resume` | one named lane effect may advance the campaign (`coordinator-step`, `supplemental-author`, `supplemental-checkpoint`, `supplemental-retained`, `author-publication-recovery`, `author-permission`, `review-replay`) |
| `finalize` | an authenticated delivered proof commits the parked wave DONE |
| `block` | no route may advance; an optional secondary/idempotent seal runs *before* the typed denial |
| `cancel` | durable cancellation, or durable state moved during authenticated readback |

`go-orchestrator.rkt` is the only interpreter: it acquires the lease, reloads
authoritative state, obtains one decision, performs one effect, commits through
the existing fenced path, and loops only when the decision says to continue.
Decisions never inspect a human-readable message; the decision `code` is stable
but is not parsed to choose behaviour.

## Wave status vocabulary

**Module:** `extensions/gsd/wave-status.rkt` — the ONE owner of three layers:

1. **Display strings** (`Inbox`, `In-Progress`, `DONE`, `DEFERRED`, `FAILED`).
2. **Canonical durable statuses** (`pending`, `in-progress`, `verifying`,
   `awaiting-delivery`, `done`, `failed`, `interrupted`, `deferred`) with the
   documented legacy aliases (`COMPLETED` → `done`, `PENDING`/`NOT STARTED` →
   `pending`, `IN PROGRESS` → `in-progress`).
3. **PLAN.md index markers** (`[Inbox]`, `[In-Progress]`, `[DONE]`,
   `[DEFERRED]`, `[FAILED]`, plus the verbatim legacy `[COMPLETED]` and
   `[CANCELLED]` spellings).

Readers accept the aliases, writers emit one canonical spelling, and an
**unknown status is a typed rejection** (`exn:fail:wave-status` carrying the
value, the accepted spellings and optional plan/wave context) — never the
historical silent `pending` fallback. Comparison-only surfaces (status
divergence warnings) use `wave-status-comparison-key`, which gives an unknown
spelling its own explicit identity instead of pretending it is `pending`.
`deferred` stays a disjoint status: never completed, actionable or retryable.
Attempt and campaign lifecycle enums remain separate, named enums.

## Wave document contract

**Module:** `extensions/gsd/wave-docs.rkt`

One contract, two consumers: `lint-wave-doc` validates it and
`prompts.rkt` generates its authoring template from the same `WAVE-DOC-*`
constants (`WAVE-DOC-HEADING-PREFIX`, `WAVE-DOC-STATUS-PREFIX`,
`WAVE-DOC-FILE-LINE-PREFIX`, `WAVE-DOC-REQUIRED-SECTIONS`,
`WAVE-DOC-INITIAL-STATUS`).

| Part | Requirement | Lint section |
|------|-------------|--------------|
| Header | exactly one `# Wave <N>` H1 then exactly one `Status: <status>` | `status-header` |
| Second status | no second `Status:` line in the body | `duplicate-status` |
| `## Files` | at least one `- File: <path>` bullet | `files` |
| `## Verify` | non-empty | `verify` |
| `## Done` | non-empty | `done` |

The header is a projection of durable state; the body is bound input. An
active executor may edit neither to unblock itself.

## Verify grammar

**Modules:** `extensions/gsd/verify-command-grammar.rkt` (token decisions) and
`verify-command-format.rkt` (declaration layout, re-exporting the grammar).

Live execution, the frozen offline binding and the review-proof retention layer
share this one grammar: the retention layer consumes
`verify-declaration-extraction` directly, so the same declaration is never
interpreted two ways, and frozen/live refusals name the same reason vocabulary
and authored span (`review-proof.rkt` owns the operator-facing text).
Backticked bullets, the legacy single-line shell contract and
fenced/unfenced multiline sections are the accepted shapes; stage splitting is
fail-closed. The precise accept/reject contract and its tested examples are in
[the user guide](gsd-guide.md#verify-grammar-verify).

## Scratch

**Module:** `tools/scratch.rkt`

One trusted per-session allocation below the platform temporary directory
(`/tmp/q-agent-scratch/<session-token>/` on POSIX), created owner-only. The
same root is used by the structured `write`/`edit` tools, the trusted worker's
allowed roots, the bash redirection policy and the executor prompt. The
allocation container itself, sibling sessions and every other `/tmp` path are
outside that root, and no cross-session cleanup happens. Direct bash treats a
redirect outside the root as a destructive command: the default `warn` policy
warns but executes, while destructive blocking (the `block` policy or safe
mode) and the sandbox worker refuse it. The worker is stricter because it has
no approval channel.

Scratch is a trusted destination only when safe mode is OFF. Safe mode is
read-only by its existing allowlist (`bash`, `edit`, `write`, `delete-lines`
are blocked), and a session scratch allocation is deliberately NOT a safe-mode
write exception: `write`, `edit`, `delete-lines` and `read` all refuse a
scratch path under safe mode through the ordinary project-root path policy.
The executor prompt therefore never claims a scratch directory is writable in
safe mode.

The trusted scratch root travels only on the local worker's trusted IPC
channel. A remote (mTLS executor) request is built without that trust channel,
and the server rejects a forged `trusted-scratch-root` with
`remote-trusted-root-refused`, so the remote worker receives no scratch root and sanctions no redirection
target: remote scratch is unavailable by omission, with no fallback to the
allocation container or a coordinator-local path. Granting it would require
the remote worker to allocate and return its own root.

A `/go` campaign drives it through one wrapper,
`campaign-request.rkt`'s `call-with-request-scratch-prompt`: the request carries
the opaque token, one root is allocated per executor attempt, that root is
rendered into the attempt's prompt, and it is adopted single-shot around the
session the interface creates — a retry gets a fresh root, and a request
without a token has no scratch at all. The allocation is bookkept under a
temporary attempt key that is released when the attempt returns; the session
that adopted the root keeps its binding under its own session id, and the /go
anchor prompt (an unbound display/legacy fallback) deliberately names no scratch
path at all.

The in-tree `.planning/scratch/...` root is no longer advertised, written by
the guidance, or exempted by the delivery-evidence set. Both evidence owners
agree: `delivery-verifier.rkt` counts untracked in-tree paths as evidence, and
`scripts/gsd-delivery.py` now treats every untracked or modified path inside
the checkout as ordinary working-tree dirt. There is no `.planning/scratch/`
delivery-SYNC tolerance left to rely on, and new work must not assume one.

## Policy and guards

**Module:** `extensions/gsd/policy.rkt` and `core.rkt`

`gsd-decide-action` centralizes guard decisions and returns `policy-decision`
(not hasheq). During `executing` mode the write guard blocks modifications to
`.planning/PLAN.md` and the planning tree; path normalization (`simple-form-path`)
prevents `..` traversal. Tool guards are per-mode. Denials are fail-closed and
name the owner and the legal transition.

## Event telemetry

**Module:** `extensions/gsd/events.rkt`

Stable taxonomy following the `gsd.<category>.<action>` convention:
- `gsd.command.received`, `gsd.command.completed` — command lifecycle
- `gsd.mode.changed` — state transitions
- `gsd.plan.parsed`, `gsd.plan.normalized`, `gsd.plan.validated` — pipeline stages
- `gsd.plan.archived`, `gsd.archive.failed` — archival
- `gsd.transaction.rollback` — transaction failures

Events carry correlation IDs via `current-gsd-correlation-id`; `emit-gsd-event!`
publishes to both the GSD event bus and the session event bus.

## Transaction wrappers

**Module:** `extensions/gsd/core.rkt` (`with-gsd-transaction`)

Snapshot-rollback semantics for multi-step commands: captures `gsm-snapshot`
before execution and rolls back on exception. Durable campaign writes are
separately fenced and journaled in `campaign-repository.rkt`.

## Archive

**Module:** `extensions/gsd/archive.rkt`

`archive-completed-plan!` validates wave completion, moves plan files to the
archive directory and resets GSD mode. Returns `gsd-command-result` (`gsd-ok`
with archive-path/files-archived, or `gsd-err` with a reason). Archive rotation
only cleans its own proven temporary artifacts; unknown `.bak` files are
reported, never guessed away.

Archiving itself is rename-only: it allocates no temporary artifact, so an
interruption leaves already-moved files complete in the archive and the rest
complete in `.planning/`, and re-running completes the same archive.

Post-archive cleanup is allow-list-only. `cleanup-empty-subdirs!` may remove an
empty `.planning/<dir>` only when it is (a) the known `waves` directory owned by
this operation, or (b) a directory an AUTHENTIC ownership marker proves q
created and that now contains nothing except those markers. An unknown empty
directory — an operator's `notes/`, a manual backup, a hidden directory, a
`waves-pre-plan-backup` without our marker, an impostor marker — is preserved
and only logged. Emptiness is not ownership.

## Planning-artifact ownership

**Module:** `extensions/gsd/planning-artifact-ownership.rkt`

The ONE predicate that answers "is this planning artifact ours?". It is a pure
read shell (its only effect is `fs-read`) and every path that may delete or
sweep consumes it instead of re-deciding ownership:

| Consumer | Destructive action gated by the predicate |
|----------|-------------------------------------------|
| `plan-snapshot.rkt` | sweeping an interrupted `plan-snapshot.tmp-*` staging directory |
| `command-handlers.rkt` | rotating/deleting the `waves-pre-plan-backup` on `/plan <text>` |
| `archive.rkt` | removing a post-archive empty planning directory |
| `planning-artifact-validation.rkt` | the read-only doctor's `owned` vs `unowned` classification |

Ownership is proven only by a marker file that is a real (nofollow) regular
file of plausible size whose readable datum is a hash with `schema-version`
exactly `1` and `kind` exactly the creator's kind
(`plan-snapshot-staging` / `.owned-plan-snapshot-staging`,
`waves-pre-plan-backup` / `.q-gsd-plan-supersede-owner`). Marker existence,
a lookalike directory name, a wrong kind, a wrong schema, empty or arbitrary
bytes, a symlinked marker or a symlinked directory all mean UNKNOWN operator
state. The doctor and the destructive consumers therefore cannot disagree: the
doctor never labels `owned` what the sweep would refuse to delete, and no
consumer deletes what it cannot prove.

## Responsibility inventory and dependency rules

**Module:** `extensions/gsd/responsibility-inventory.rkt`, enforced by
`tests/test-gsd-responsibility-inventory.rkt`.

Every module under `extensions/gsd/` is classified (domain, effects,
parameters, dependencies) and the declared effects must match the source. At
v1.00.31 that is 113 modules (W4 added the read-only ownership predicate); the
number is asserted against the directory listing by the inventory test, so it
cannot drift silently. The rules that keep the contract single-owner:

- Only `campaign-state.rkt` / `campaign-repository.rkt` may write durable
  campaign state. UI, tool and prompt modules never do.
- Only the evidence/owner modules may interpret evidence authority
  (`author-scope.rkt`, `supplemental-evidence-authority.rkt`,
  `review-proof.rkt`). Adapters consume decisions; they never invent permission.
- Status, path, and Verify vocabulary live in their one owning module; new
  duplicates are a review finding, not an alternative implementation.
- Compatibility wrappers delegate only. Current wrappers and their removal
  target:
  `wave-docs.rkt`'s `wave-status-markers` accessor and internal
  `status->symbol` alias (`TODO(#v1.00.40)`; removal planned for v1.00.40).
  `extensions/gsd-planning.rkt` remains the extension entry point, not a
  second policy layer.
  Legacy message readers in `stall-policy.rkt` and `review-checkpoint.rkt`
  are boundary-only adapters whose removal is scheduled for v1.00.40 after
  external callers migrate; internal retry/authorization consumes typed data.

## Simplification campaign (v1.00.31): measured before/after

This section records only values that are reproducible in the tree. "Before"
is the branch base for the slice named in the row; every number below is
asserted by a test or by a recorded fixture, not estimated.

| Measure | Before | After | Evidence |
|---------|--------|-------|----------|
| Classified GSD modules in the responsibility inventory | 109 | 113 | `(length inventory)` asserted by `tests/test-gsd-responsibility-inventory.rkt`; the four additions are `campaign-decision.rkt`, `campaign-lease.rkt`, `campaign-request.rkt` (W3/W5) and `planning-artifact-ownership.rkt` (W4: the ONE authentic ownership-marker predicate, read-only) |
| Authorities that reconstruct the next `/go` action (W3) | 3: `delivery-handoff`'s `ready-for-run-checkpoint`, the `run-coordinator-checkpoint` cond, and the orchestrator's `case` on their symbols | 1 pure decision (`decide-campaign-action`) + 1 interpreter (`run-campaign!`) | the three reconstructions were deleted, not wrapped; `tests/test-gsd-campaign-decision.rkt` pins the decision matrix through typed kinds/routes/codes only |
| `go-orchestrator.rkt` size (W3/W5) | 1699 lines / 19 top-level defines | 1689 lines / 11 defines | re-recorded in `tests/fixtures/go-orchestrator-baseline.rktd` (W3 review fix: the finalize branch's post-DONE reload gained the same #f guard as launch/resume, +6 lines and still below the 1700 ceiling; W5 extracted the W2 per-attempt scratch wrapper to `campaign-request.rkt`, then moved the typed no-change retry transition to `verification-repair.rkt` (1696 → 1689)); `tests/test-release-workflow-contract.rkt` enforces the recorded value, so any later growth must re-record the fixture in the same commit |
| Effects the `/go` interpreter owns (W3) | included `mkdir` (campaign-directory creation in the loop) | no `mkdir`; the lease module owns the directory creation and the OS advisory lock | inventory entries for `go-orchestrator.rkt` and `campaign-lease.rkt` |
| Moved code (W3/W5) | - | three extracted modules instead of inlined responsibilities: `campaign-decision.rkt`, `campaign-lease.rkt`, `campaign-request.rkt` (528 lines by newline count at final integration) | file newline counts |
| Scratch allocation owners reachable from the executor prompt (W2/W5) | prompt, bash-redirection and direct-write roots were described separately | one allocation (`tools/scratch.rkt`); `prompts.rkt` renders it and depends on it | inventory entry for `prompts.rkt` (`path-ops` effect plus the `tools/scratch` dependency) and `tests/test-tool-scratch-allocation.rkt` |

What these numbers do **not** say:

- The W3/W5 extraction did not reduce total lines. The three extracted modules
  total 528 lines; the interpreter shrank by 10 lines (1699 → 1689);
  the gain is one routing authority, a pure decision kernel that is testable
  without I/O, and an interpreter that no longer hides a lease.
- Routing never reads operator text. The pre-W3 decisions travelled as symbols
  plus human-readable messages, so a wording change could move behaviour. The
  decision now carries a typed `kind`, a `route` symbol and a stable `code`, and
  `message` is carried for the operator only.
- No gate result is claimed here. The inventory, docs-contract, tier-ownership
  and release-workflow checks are the reproduction apparatus; run them in the
  checkout to obtain their current verdict.

## Compatibility

Historical campaign IDs, manifest hashes, bound body hashes and record formats
remain readable; durable record schemas do not change for reader-contract work
such as the typed status rejection. Existing bound wave paths keep loading
exactly as bound even when newer authoring rules accept more spellings. A
newly accepted spelling never rewrites an old campaign.
