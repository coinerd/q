# Automatic independent `/go` review

Production `/go` requests carry a reviewer runner consumed by the shared TUI/SDK
campaign executor. `extensions/gsd/review-session.rkt` owns the session adapter
and composes runtime and tool APIs from the extension layer. The runner creates
a fresh agent session requesting exact `zai-coding-plan/glm-5.3`, with only
`read` and `bash`, no extensions or memory, and a runtime-owned `q-session:<id>`
identity. The harness label is not assumed to be a q registry alias: the default
builder reads merged q settings and selects the uniquely configured `glm-5.3`
registration (or a literally registered `zai-coding-plan/glm-5.3` alias). It always
constructs the OpenAI-compatible transport with API model ID `glm-5.3`, never the
literal harness prefix. Supported configured provider names are `glm-5.3`,
`zai-coding-plan`, and `openai-compatible`, authenticated through q's normal
credential lookup. The endpoint must be HTTPS `api.z.ai` at
`/api/coding/paas/v4` (optional explicit port 443 and trailing slash only).
Wrong host/path/transport, HTTP, userinfo/query/fragment, Flash/other models,
missing credentials, or multiple exact registrations fail closed, even if a q
default would otherwise resolve one. There is no mock or alternate-model fallback.
Injected provider builders remain available for deterministic fixtures. An
injected/manual request without this runner retains the existing manual-review
behavior.

Tracker facts come only from the hash-verified frozen campaign snapshot.
Numeric `Branch:` issue/milestone metadata remains authoritative. Without a
`Branch:` line, a strict explicit same-wave `W<n> = #<positive issue>` association
may supply the issue; prose links, canary rows, live plans and global tracker
configuration cannot. Duplicate, malformed or Branch-conflicting associations
fail closed. When the numeric milestone is missing, prepare and finalization
share an authenticated cached lookup of that exact frozen issue. The response
must be a non-PR issue with the exact number and canonical same-repository URL,
and its milestone must have a positive integer number, canonical same-repository
API URL and title exactly matching the unique verified `# Plan: <title>` heading.
Missing facts, API errors, null milestones and mismatches stop review. Finalization
also refuses a tracker binding that changed since dispatch; no approval or
publication guard is bypassed by this resolution.

The reviewer uses an **existing clean worktree on the exact receipt branch**.
Only receipt-descendant evidence-only commits under the controller's three
`docs/reports/gsd-wave-{evidence,reviews,validation}` directories are permitted.
Git identity and tracked bytes are checked before and after review. Final JSON
must come from a normally completed final assistant message. The controller,
not the model, owns digest/head binding and strict evidence validation.

Reviewer tool environments scrub credentials, remove credential-agent and shell
startup variables, use an empty private HOME/config directory, and disable the
execution plane. This is not an OS filesystem sandbox: `read` and `bash` still
have the initiating account's filesystem permissions. Deployment requiring
protection against a hostile reviewer reading absolute credential paths needs
an external restricted filesystem/process boundary.

Review timeout defaults to 600 seconds; cancellation, durable fence/receipt
changes, campaign budgets and abnormal termination stop review. Available
review usage is charged to cumulative campaign spend without replacing author
attempt usage. Usage accounting is not a crash-exactly-once per-review ledger.

Explicit premerge REQUEST_CHANGES crosses a lossless JSON checkpoint into the
existing durable rejection resolver. The default persistent rejection allowance
is one. Journal-first crash recovery replays the same rejection idempotently.
If publication succeeded before the checkpoint wrote its marker, prepare fetches
and structurally authenticates the committed REQUEST_CHANGES against the exact
receipt head/branch/attempt/fence and active generation before any reviewer
launch. Every ancestry-tail commit must touch only excluded evidence paths;
malformed or stale rejection evidence refuses dispatch, not a hidden retry.
The ordinary retry creates a new implementation session/attempt/fence, carries
findings into the prompt, and reuses the rejected source checkout. It fetches
and fast-forwards only a receipt-descendant evidence-only rejection tail before
implementation. Fresh full Verify supersedes the old receipt, preserves receipt
history, advances the evidence generation, and requires another fresh review.
Rejected source is not destroyed if that retry fails.

Malformed output, infrastructure failure, cancellation, stale identities,
budget pause or exhausted rejection allowance stop without another rejection
retry. Postmerge REQUEST_CHANGES currently stops explicitly with an unsupported
artifact-repair reason; it never rewinds or reruns already merged source.

F11 is unchanged. Merge still requires recorded operator authorization bound to
the exact wave and implementation receipt, with operator/action/source and
matching content digest. `/go`, the reviewer, and this integration do not invent
that authorization. No trustworthy automatic authorization source was identified
in this slice; an otherwise reviewed campaign may legitimately stop at this
gate rather than claim DONE.

Approval candidates are committed locally and must pass the unchanged strict
trio gate before pinned non-force publication/readback. Malformed remaining
items fail as infrastructure errors without changing the remote; no gate facts
or remaining items are silently discarded.

Focused proof now exercises the default production coordinator and real Python
controller against a local Git remote (`tests/test-gsd-auto-review-production.rkt`).
The actual isolated `/go` loop (`tests/test-gsd-auto-review-loop.rkt`) publishes
rejection, loses the checkpoint outcome/marker, resumes from committed evidence,
and enters exactly one ordinary fenced retry and fresh generation review. Only
model transport responses are deterministic; controller statuses are not mocked.
The real-Git strict controller suite is registered by
`tests/test-gsd-delivery-auto-review.rkt`. These are not live GitHub delivery or
independent model acceptance: preliminary Codex findings are not an acceptance
substitute. `tests/test-gsd-review-runtime.rkt` additionally exercises the actual
default provider builder with real temporary q settings/auth files, a fresh empty
`PLTUSERHOME`, and a network-denying guard; deterministic HTTP-boundary capture
checks the exact API model and Coding Plan path. A safe actual-config construction
probe verified provider availability without network or credential output. This
is not a live reviewer response or canary. The provider-selector patch still
requires independent exact GLM-5.3 diff review and all protected full CI gates on
the new source head before merge/build/restart/resume. Prior-head approval or
pending checks must not authorize installing that head.
