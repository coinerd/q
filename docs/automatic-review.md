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
must come from a normally completed final assistant message: exactly one JSON
object, either bare or inside one whole-text Markdown fence with an optional
`json` tag. Prose, multiple blocks, other language tags and trailing values or
garbage fail closed with a specific final-output diagnostic. A `review-failed`
reason must describe the actual failure, not literal schema placeholder text
such as `infrastructure only`. Fence normalization does not relax owned-proof
or RED acknowledgement binding, confer approval, or supply missing behavioral
RED evidence. The controller, not the model, owns digest/head binding and strict
evidence validation.

Reviewer tool environments scrub credentials, remove credential-agent and shell
startup variables, use an empty private HOME/config directory, and disable the
execution plane. This is not an OS filesystem sandbox: `read` and `bash` still
have the initiating account's filesystem permissions. Deployment requiring
protection against a hostile reviewer reading absolute credential paths needs
an external restricted filesystem/process boundary. The reviewer's `bash` tool
is a compatibility name for bounded direct-argv read-only commands, not an
unrestricted shell (`review-tools.rkt`). It cannot run the owner authorization
CLI or use policy auto-approval to mint supplemental authority.

### Bounded missing-evidence recovery

Before independent review, the evidence readiness boundary authenticates the
retained green proof, frozen Verify binding, receipt identity and clean source
checkout. Existing compatible owned RED remains usable. When meaningful RED is
missing, static discovery may select a supported frozen RackUnit regression
suite; unsupported or ambiguous harnesses pause with `evidence-repair-required`.
This is not an author repair lifecycle: source/test changes require a separate
implementation attempt and full Verify, which this slice does not automate.

The controller reserves at most one operation, with two jobs and a 600-second
probe timeout, and runs the authenticated regression against isolated baseline
and implementation worktrees. `scripts/gsd-evidence-probe-runner.rkt` collects and
validates the supported suite; runner coverage checks read installed owner code
and Git bytes, rather than trusting JSON assertions. Only canonical formatting
and context/spec digests are pure (`evidence-repair-format.rkt`); discovery,
readiness, probe, coverage and ledger boundaries perform file/Git I/O. The
controller delegates those effects rather than becoming a proof authority.

Registration requires owner-generated authorization and revalidated context,
retains the green proof and records a separate evidence revision. Registration
alone does not admit review: readiness must authenticate it again. A durably
captured result can be recovered under the same live guard; ambiguous intent or
start records pause and are not relaunched or silently adopted. Stale identities,
unsupported suites, invalid probe artifacts and exhausted reservation budgets
fail closed. No approval, operator authorization, source receipt replacement or
publication permission is created by this recovery. The production reviewer
remains exact GLM-5.3 with the unchanged 600-second cap. Local deterministic tests
are not live model acceptance or live GitHub delivery evidence.

### Governed supplemental evidence replan (BUG-0088)

An exhausted automatic evidence-author allowance remains permanently spent.
`evidence-author-allowance-exhausted` is an honest blocked state, not permission
to retry. `/replan` changes planning mode only; a new task ID, receipt rotation,
crash, revocation or failure does not automatically reset an allowance or issue
a replacement grant. Supplemental recovery is a separate operator-governed,
one-shot test-evidence task on the retained campaign checkout, not another
implementation dispatch or a new campaign carrying old proofs forward.

The governance contract separates these immutable records and decisions:

- **T / task grant A:** exact source, campaign, snapshot, predecessor receipt and
  generation, baseline/tree, control-plane identity, two model-write paths
  (probe/helper), enumerated owner-generated paths, deadlines and bounds. An
  authentic operator grant binds the exact retained T proposal digest.
- **K / candidate approval:** actual committed descendant chain/tree and exact
  blob/mode/hash manifest. Every intervening commit must stay in scope, including
  changes later reverted. Approval of K permits verification, not delivery.
- **C0 / C1, G1 / R1:** historical frozen Verify C0 stays unchanged. Effective C1
  retains exactly the five approved stages, appending only the approved probe
  to the focused positional list; original focused paths remain an ordered
  prefix and all other normalized stages are unchanged. No sixth stage,
  removed check or shell/flag substitution is allowed. A fresh complete owned
  C1 GREEN G1 is mandatory; the old C0 GREEN cannot certify changed tests.
  The versioned supplemental proof (`schema 2`,
  `supplemental-declared-verify-green/v1`) binds the effective contract and
  approvals; legacy readers must refuse it, not discard authority fields.
- **S / supersession authorization:** exact predecessor receipt/generation,
  K/G1/R1, proposed successor receipt and publication identity. This is a third
  exact-digest operator decision, before publication/certification.
- **P / promotion:** protected-owner attestation linking S and captured G1/R1
  to the exact certified receipt/context/generation and registration operation.
  Public P is an audit descriptor, not a capability or approval secret.

G1 produces the exact proposed receipt r* before probing. A distinct staged
wrapper predicts its successor evidence context E; neither is current evidence
for the old receipt. Identical committed probe/helper bytes overlay the real
ancestor baseline, never candidate production code. R1 must retain a genuine
named assertion RED there and the same assertion GREEN on K. Compilation,
setup errors, timeouts and unrelated failures are not RED. Helper discovery
requires exact authorized literal paths and committed regular blobs, without
executing readers during discovery; the strict probe grammar remains unchanged.
Certification preserves r* exactly, including captured timestamp/proof reference.

Protected publication uses intent/readback and guarded receipt supersession,
preserving historical receipts/proofs/publications, the original completed
author marker and cumulative accounting. Promotion requires actual current
context to equal predicted E, then issues a fresh opaque single-consumption
registration capability from protected owner origin. It admits the captured
operation in the actual generation and registers exactly once. Receipt-certified,
registration-pending recovery replays that same attested operation without any
model/probe relaunch or rewritten R1. A digest, public P or caller-supplied guard
alone is not authority. Offline audit requires an authenticated controller/owner
anchor for the full P → S → K/G1/R1 → receipt/E chain. The required fresh review
request and ACK must pin that chain, effective contract and registration revision;
this is an acceptance requirement, not a claim that all consumers are accepted.

The protected journal consumes launch permission **before** the single test-author
dispatch, using a fresh monotonic owner attempt/fence allocation. Ambiguous launch
stays spent. At full `/go` campaign entry, an `awaiting-delivery` wave with an
authentic, live approved-but-unlaunched T is authenticated before the ordinary
exhausted-allowance coordinator. Under the campaign lease and authority lock,
only that eligible wave's status returns to `pending`; the existing owned lane
then allocates and consumes the single scoped test-author launch. This is not a
legacy allowance reset: the historical marker, receipt/proof, frozen snapshot,
FILES and task/approval records stay unchanged. Stale context/attempt/fence,
revocation, expiry, cancellation, budget pause or an ambiguous reserved allocation
fail closed. A retained supplemental pointer owns spent/revoked retries too;
it never permits ordinary-author fallback or duplicate dispatch. Direct
`run-campaign-wave` tests do not cover this parked full-campaign checkpoint;
full-entry fixtures invoke `run-campaign!` without an advance override and stop
at the next genuine approval boundary, not at live delivery acceptance.

Shared owner locking, revision/CAS and write-ahead intents serialize
admission, revocation and recovery. Revalidate exact source/clean checkout,
ancestry, receipt/generation, approvals/revocation, deadlines, cancellation and
fences at protected boundaries and after waits. Owned jobs must be confirmed
quiescent before releasing admission. No automatic regrant, phase redispatch,
accounting reset or unknown-cost-as-zero shortcut is permitted.

#### Operator runtime and trusted TUI handoff

Authority lives outside project/execution/control repositories in the trusted-user
`~/.q/agent/supplemental-evidence` store. Owner tooling and `/go` share that fixed
default; project JSON, XDG lookup and public campaign pointers cannot select or
create authority. Only the owner frontend provisions validated missing directory
components; `/go` does not create a store. Explicit embedding overrides must bind
both processes to the same trusted absolute store; explicit `#f` denies. The root
must be mode 0700, records mode 0600, without symlink indirection. These checks are
trusted-process protocol controls, **not isolation from arbitrary same-uid code**.
The launcher must supply trusted HOME, owner-process/tool separation and any
external sandbox required by its threat model. Modes and hashes alone do not
prove operator identity.

`scripts/gsd-evidence-replan.rkt` actually parses `--verb` with values
`propose|authorize|inspect|status|revoke`, not positional subcommands. Authorization
uses `--verb authorize --stage task|candidate|supersession`; campaign operations
also take `--campaign-root`, `--plan` and `--wave`. `propose` additionally requires
`--repo`, `--test-path`, `--harness-path`, `--owner-paths`, `--baseline-head` and
`--baseline-tree`; the authenticated receipt context supplies the execution
checkout, not an arbitrary replacement `--repo`. `inspect` uses `--kind` and
`--sha256`. Proposal retention and inspection grant nothing.

A standalone invocation still has no active in-process interactive approval
channel: `authorize`/`revoke` refuse with exit 2. Stdin, TTY consent, booleans,
project files and policy/batch auto-approval are not grant paths.

The trusted q TUI now supplies the owner handoff (fixture-tested, not installed
or live-authorized). Start q in the same campaign-root project directory used by
CLI `--verb propose`, with the same trusted-user authority store. In its keyboard
command input use:

```text
/evidence-replan inspect task-proposal TASK-SHA256
/evidence-replan authorize-task PLAN WAVE TASK-SHA256
/evidence-replan revoke KIND APPROVAL-SHA256
```

`inspect` prints the full retained canonical record and digest in the transcript.
`authorize-task` requires the exact existing campaign pointer and obtains an opaque
broker grant only after human selection in the genuine approval overlay. It pins
the validated task approval but launches nothing. Then invoke `/go` separately:
its governed controller inherits the UI publisher and requests exact retained K
and S approvals through the same digest-correlated reducer and keyboard handler.
To inspect either record first, deny the prompt, run the displayed `inspect`
command, then invoke `/go` again. Denial never grants authority. Each prompt
identifies the stage and full digest; no bounded preview substitutes for inspection.
The owner command is intercepted only on the keyboard-user path, not registered
as a model tool or extension hook. Shared command dispatch reserves its name but
cannot invoke it. Missing UI bus/channel fails closed.

For revocation, `KIND` is `task-approval`, `candidate-approval` or
`supersession-approval`, and `APPROVAL-SHA256` identifies the exact live private
approval (not the proposal). Wait for session quiescence, inspect that approval,
then use the keyboard `revoke` command. It retains an inert one-target revocation
proposal and presents its exact digest through the same genuine approval overlay.
Deny to inspect the displayed `revocation-proposal` bytes; repeat the same revoke
command to request again. Only an explicit exact-digest broker grant consumed by
protected `revoke!` installs durable revocation. Denial, cancellation and wrong
digests do not revoke. Success reports the revocation record digest and launches
nothing: subsequent validation, admission and recovery refuse revoked authority,
without redispatch or resetting the spent allowance. Standalone CLI revoke still
refuses without an active same-process interactive channel; there is no batch,
TTY or policy approval substitute.

Engineering fixtures with real broker grants do not establish live authorization,
installation or `/go` resumption. Until independent frontend acceptance, complete
review/audit binding, final-head gates and protected installation are accepted, recovery remains
blocked. Live T, K and S decisions are still separately required. Only actual
registered readiness `ready` followed by production `/go` reaching a fresh
independent reviewer with zero author redispatch resolves the original admission
blocker. Reviewer rejection/infra remains an honest outcome; approval, F11 and
protected delivery remain separate gates.

### Lost author with a committed delta (BUG-0086)

At the owner-only `/go` checkpoint, the existing owned-session liveness verdict
may prove that a durably running evidence author is gone. This does **not** prove
which model phases ran. Neither phase is redispatched and no author allowance,
review rejection, approval or no-edit result is invented.

For a changed committed tree, recovery requires the exact existing successor
attempt/fence, stable campaign fence and frozen declaration, strict receipt-head
ancestry, and every intervening commit's paths within frozen literal FILES.
The authenticated execution checkout must be clean. Its Git common-directory
identity must match the old receipt: a receipt naming `q/` and a context naming
its retained worktree are different locations, not different repositories.
Clones and arbitrary path substitution do not satisfy that identity.

The owner runs the **complete frozen declared Verify** on the existing successor,
retains a genuine owned green proof, and uses the existing approved-head
publication/readback and author receipt certification protocols. Provenance binds
the approved successor before receipt supersession, so publication crash windows
remain resumable. An interrupted owned publication intent reuses its exact proof;
a repeated successful `/go` proceeds to fresh independent review, not another
Verify or model author phase. Prior review/evidence-generation approvals do not
survive supersession; RED reproduction/readiness and all protected gates remain
mandatory.

The entire owner recovery, including proof capture, Git snapshots, publication
readback/guards and confirmed-publication replay, has an outer deadline: the
configured Verify window plus one hour (five hours by default). Verify keeps its
full four-hour default; this is not a short reviewer timeout. Recovery owns a
dedicated verification registry, closes admission and cancels/reaps jobs before
interrupting its worker, then shuts down its custodian. Interrupted sandbox
processes kill/reap their groups before propagating the break. Real verifier Git
facts (including the non-injectable proof snapshot path) have a 15-second limit
and a 10 MiB capture cap per stream; timeout/truncation refuses the fact rather
than approving partial evidence. Full Verify logs remain file-backed.

Dirty staged, unstaged or untracked files stop recovery with an actionable
preservation message. In particular, unrelated `README.md` metrics edits must be
explicitly accounted for by the operator before resuming. Recovery never discards,
commits, stashes or copies them away, and does not verify an immutable substitute
to evade the clean-source gate. A red Verify, stale identity or publication
failure leaves the running author and old receipt intact. This behavior is
covered with real owned subprocesses and a local bare Git remote, not live
campaign mutation or live reviewer acceptance.

### Frozen Verify execution binding

The frozen document and snapshot digest remain immutable. Declared execution,
owned-proof capture, offline audit and focused RED selection share the pure
`verify-command-format.rkt` stage canonicalizer. Fenced multiline Verify commands
are joined in document order with fail-fast ` && ` before expansion, jobs injection,
identity hashing and execution. A terminal shell exit zero therefore cannot mask an
earlier failed stage. Existing unambiguous single-line and `&&` declarations retain
their command bytes. Legacy single-command shell contracts are not promoted to the
strict five-gate audit grammar.

Ambiguous chains (empty stages, dangling operators, continuations, quoted newlines
or `&&`, shell control/substitution syntax) fail closed rather than being split as
arbitrary shell programs. The strict audit still requires the supported format,
compile, focused whole-file tests, full fast suite and lint shapes. Only the trusted
operational jobs=2 option may be injected; scope flags and deadlines are unchanged.
The manifest schema remains v1: validators bind the exact canonical expanded command
and argv, not successful newline scripts or diagnostic log summaries. Previously
retained newline executions cannot be retroactively certified by this normalization.
Meaningful behavioral RED remains a separate failed registered focused execution,
not an inference from green logs or a new approval.

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

Malformed output, cancellation, stale identities,
budget pause or exhausted rejection allowance stop without another rejection
retry. Postmerge REQUEST_CHANGES currently stops explicitly with an unsupported
artifact-repair reason; it never rewinds or reruns already merged source.

A typed review-lane infrastructure failure (transport, stale session, malformed
or verdict-less reviewer output) no longer dead-ends the wave (BUG-0084): the
coordinator checkpoint relaunches the review from a separate durable per-round
budget in the delivery journal (default two, parameter
`current-gsd-review-infra-retry-limit`). Each retry is itself the durable
journal transition, so the no-spin advance guard holds; any genuine advance
refreshes the next round's budget; exhaustion stops blocked with the honest
bounded reason, and blank blocked reasons are never persisted as wave failure
reasons. Reviewer sessions pin the author-phase parameter unbound so the
pre-tool guard can never deny the reviewer's own read/bash tools.

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
