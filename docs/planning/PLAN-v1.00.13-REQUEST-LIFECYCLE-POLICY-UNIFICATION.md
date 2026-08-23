# PLAN v1.00.13 — Request Lifecycle Policy Unification

**Source plans:** `PLAN-v1.00.12-SSE-STALL-DETECTION-BOUNDS.md` (containment + handoff) and `AUDIT-v1.00.12-PLAN-AND-NETWORKING.md` (NP-1…NP-8).
**Architectural trigger:** the v1.00.12 regression proved that a raw scalar timeout (`sse-read`) can cross semantic boundaries and silently change dead-peer detection, reasoning allowance, content-gap detection, and whole-body reads. The audit additionally found response-header loss, non-streaming cleanup gaps, and adapter-specific timeout behavior. These are treated here as one ownership problem, not unrelated networking bugs.
**Depends on:** v1.00.12 released and its phase bounds/timeout diagnostics green. If v1.00.12 takes the temporary all-adapter parity path, this plan preserves behavior while relocating ownership; if v1.00.12 defers parity, W2 completes it.
**Primary goal:** establish **one mandatory owner for provider-request lifecycle policy**. Raw model/config timeout values are resolved once into semantically named policy fields; every provider adapter consumes that resolved policy; transport/SSE code enforces mechanism only; response metadata, resource cleanup, structured failures, and retry metadata survive the request boundary.
**Execution root:** `/home/user/src/q-agent/q`. All paths below are relative to `q/`.
**GSD waves:** 6 (W0–W5)
**Broad gate:** after W4 and in W5
**Companions:** `STATE-v1.00.13-REQUEST-LIFECYCLE-POLICY-UNIFICATION.md`, `VALIDATION-v1.00.13-REQUEST-LIFECYCLE-POLICY-UNIFICATION.md` (created at execution)

---

## 1. Context: The Deeper Problem

v1.00.12 fixes the immediate DeepSeek 600 s SSE stall and extracts a shared phase-timeout
resolver. That is necessary but not sufficient.

The deeper architectural problem is **distributed ownership of a provider request**:

1. **Policy is distributed.** OpenAI-compatible computes timeout values from per-model config;
   anthropic/azure/gemini historically use stream defaults. An adapter can accidentally widen,
   tighten, or omit a safety bound.
2. **Timeout values are semantically untyped.** `sse-read` has been used as both a
   whole-body budget and a per-phase stream gap. One integer therefore means several different
   things depending on call path.
3. **Mechanism knows policy.** SSE transport code contains constants/config-derived semantics
   that should be resolved before transport begins.
4. **Lifecycle metadata is lost.** HTTP response headers are discarded before retry policy can
   use them; `Retry-After` is therefore reconstructed from exception text and never works for
   real responses.
5. **Resource ownership differs by path.** Streaming paths have explicit custodian/port
   cleanup, while the non-streaming/eager-body twin relies on GC and lacks timeout cleanup.
6. **Failure semantics are split between struct fields and human strings.** Retry code should
   consume structured phase/status/header metadata, not parse presentation text.
7. **Liveness metadata is collected but not authoritative.** Heartbeats are recorded, yet
   held-request classification ignores them.
8. **No architecture-level conformance test prevents recurrence.** A resolver unit test can be
   green while a new adapter bypasses it entirely.

### The semantic-type failure

The v1.00.12 incident can be expressed as a type error:

```text
600 seconds as "slow reasoning allowance"
        accidentally became
600 seconds as "dead-peer/content-stall detector"
```

A raw scalar cannot encode which clock it belongs to.

The architectural target is therefore:

```text
raw config
    │
    ▼
request-policy resolver
    │  (semantic names + invariants + legacy mapping)
    ▼
resolved request-network-policy
    │
    ├──────────────► provider adapter (wire format/auth/endpoints)
    │
    ▼
request lifecycle executor
    │  (HTTP ownership, headers, deadlines, cleanup, failure context)
    ▼
stream/body mechanism
    │  (enforce already-resolved bounds; parse bytes/events only)
    ▼
structured outcome/failure
    │
    ├──────────────► retry policy
    └──────────────► runtime/TUI presentation
```

**Ownership rule:** after resolution, provider adapters and transport code must not reinterpret
raw timeout config.

---

## 2. Defects

| ID | Defect | Severity | Owner Wave |
|----|--------|----------|------------|
| **RL-1** | No single module owns provider-request lifecycle policy; adapters can independently compute or omit timeout semantics | HIGH | W1/W2 |
| **RL-2** | `sse-read` is semantically overloaded (thinking gap + eager whole-body read; historically initial/content gap too) | HIGH | W1 |
| **RL-3** | Adapter APIs do not require a resolved network policy, so bypassing policy is easy and invisible | HIGH | W2 |
| **RL-4** | Connect+TLS+status+headers (TTFB) uses the broad request budget; established-but-silent peers can hold for up to 900 s | MEDIUM | W4 |
| **RL-5** | Response headers are discarded; `Retry-After` is not available as structured retry metadata | MEDIUM | W3 |
| **RL-6** | Non-streaming/eager-body response-port lifecycle is not explicitly owned on success/error/timeout | MEDIUM | W3 |
| **RL-7** | Retry/presentation layers depend partly on exception strings instead of one structured failure context | MEDIUM | W3 |
| **RL-8** | Heartbeat liveness metadata is ignored by held-request classification; heartbeat-only streams can be misclassified | LOW-MEDIUM | W4 |
| **RL-9** | Total stream deadline enforcement is soft and adapter policy for total duration is not centrally defined | LOW-MEDIUM | W1/W4 |
| **RL-10** | No cross-adapter architecture/conformance suite proves every provider consumes the same resolved policy and lifecycle contract | HIGH | W0/W2/W5 |

---

## 3. Target Contracts

### 3.1 Resolved request-network policy

Introduce a pure semantic value in a policy-owned module (candidate:
`llm/request-policy.rkt`; W0 verifies the least-coupled existing location before creation).

Conceptual shape:

```racket
(struct request-network-policy
  (request-budget-secs
   connect-ttfb-secs
   initial-idle-secs
   thinking-idle-secs
   content-idle-secs
   stream-total-secs
   body-read-budget-secs)
  #:transparent)
```

The exact field names may follow repository naming conventions, but the semantic separation is
non-negotiable.

### 3.2 v1.00.13 policy invariants

The resolver owns these invariants:

- **request budget:** preserves the existing per-model `request` meaning.
- **connect/TTFB:** a dedicated bounded value, independent of the broad request budget.
  Initial implementation: `(min request-budget 120)`; W0 records evidence and may tighten the
  default before W4, but W4 must not ship with the old 900 s established-silent bound.
- **initial idle:** `(min request-budget 120)`; never widened by model config.
- **thinking idle:** `(min request-budget (min configured-thinking 300))`; default 120.
- **content idle:** 60 s; never widened by model config.
- **stream total:** centrally derived from the current supported semantics. W0 characterizes
  the existing formulas before W1 freezes one policy contract; adapters may not hardcode their
  own total.
- **body-read budget:** explicit semantic field for eager/non-streaming full-body reads.

The resolver validates every produced duration as positive and fails configuration early rather
than allowing zero/negative values to reach transport code.

### 3.3 Config migration: retire semantic overload without breaking users

Introduce semantically named optional model keys:

```text
timeouts.models.<m>.request
timeouts.models.<m>.thinking-idle
timeouts.models.<m>.body-read
```

`initial-idle` and `content-idle` remain safety invariants, not ordinary widening knobs.
`connect-ttfb` remains policy-owned in v1.00.13 unless W0 finds a proven provider requirement
for a bounded override.

Backward compatibility for legacy `sse-read`:

```text
thinking-idle:
  explicit thinking-idle
  > legacy sse-read
  > 120 default
  then cap at 300

body-read:
  explicit body-read
  > legacy sse-read
  > existing fallback
```

`legacy sse-read` must **never** influence connect/TTFB, initial idle, or content idle.

Documentation marks `sse-read` deprecated in v1.00.13. Removal is not part of this milestone.

### 3.4 Mandatory consumption

A streaming/non-streaming provider request may not start without a resolved
`request-network-policy`.

Adapters own:

- authentication;
- endpoint/provider-specific headers;
- request payload encoding;
- provider wire-format decoding;
- stream-event normalization.

Adapters do **not** own:

- timeout/config interpretation;
- generic HTTP resource ownership;
- generic deadline/liveness policy;
- `Retry-After` semantics;
- retry classification;
- rendering retry text.

### 3.5 Structured failure context

Introduce one machine-readable failure context carried by network/provider exceptions
(exact exception subtype integration follows the current hierarchy):

```text
kind
phase
elapsed
idle-duration
received-data?
received-heartbeats?
content-chars
http-status
response-headers
retry-after
```

Not every failure populates every field.

The existing human-readable exception prefix remains stable where practical. v1.00.12's
`[phase=… data-received=… chars=…]` suffix becomes a **rendering of structured fields**, not
the data path itself.

### 3.6 Resource ownership

The request lifecycle boundary owns every response port it opens.

For both streaming and non-streaming paths:

- normal completion closes/finalizes once;
- status-check failure closes/finalizes once;
- read/phase timeout closes/finalizes once;
- request/connect timeout closes/finalizes once;
- cancellation/abandonment closes/finalizes once;
- GC/will cleanup remains a final safety net, not the normal lifecycle.

---

## 4. Outcome / Definition of Done

v1.00.13 is complete only when:

1. **One policy owner (RL-1/RL-2):** raw timeout/model configuration is interpreted in one
   policy module. `llm/stream.rkt` no longer owns model/config semantics; any v1.00.12
   resolver there is moved behind or reduced to a compatibility re-export.
2. **Semantic config exists (RL-2):** `thinking-idle` and `body-read` are supported;
   legacy `sse-read` maps only to those semantic fields with explicit precedence and caps.
3. **Policy is mandatory (RL-3):** openai-compatible, anthropic, azure-openai, and gemini
   request paths all receive one resolved policy; no adapter computes phase timeout values from
   raw config.
4. **Cross-adapter parity is tested (RL-10):** a shared conformance harness proves all four
   adapters pass the same policy values into common stream/body mechanisms.
5. **Architecture gate exists (RL-10):** CI fails if provider adapters regain direct ownership
   of legacy timeout accessors/magic phase constants outside an explicit allowlist.
6. **Connect/TTFB is bounded (RL-4):** an established connection that produces no status/header
   progress cannot consume the 900 s model request budget; the dedicated policy bound fires and
   carries `phase='connect/ttfb` (or repository-equivalent structured phase).
7. **Headers survive (RL-5):** status + response headers required for retry policy are preserved
   in structured failure metadata. `Retry-After` is read from the header, not exception text.
8. **Retry-After works (RL-5):** delta-seconds is honored; HTTP-date is supported if the current
   retry subsystem already has a clock abstraction, otherwise HTTP-date support is added with
   deterministic clock-injected tests.
9. **Non-streaming cleanup is explicit (RL-6):** `make-provider-http-request`/eager-body paths
   close the response port on success, status error, timeout, and cancellation; timeout cleanup
   is passed to the request-timeout wrapper.
10. **Structured failure path (RL-7):** retry classification consumes structured fields.
    No retry decision parses the human exception message.
11. **Heartbeat-aware liveness (RL-8):** received heartbeats prevent a heartbeat-only live peer
    from being classified as a zero-liveness held request; empty/comment flood protection remains
    bounded.
12. **Total deadline owned centrally (RL-9):** every adapter receives its total stream/body
    budget from policy; deadline checks use remaining budget so blocking reads cannot overshoot
    by a full phase timeout.
13. Focused suites, cross-adapter conformance, fast gate, broad gate, architecture, security,
    lint-all, release-readiness, and release workflow pass on the final SHA.
14. `v1.00.13` is tagged and released with the established release assets; STATE/VALIDATION
    documents record policy matrix, adapter conformance, cleanup evidence, and rollback points.

---

## 5. Non-Negotiable Rules

- **TDD first per wave.** New architectural contracts start red; production changes make the
  same tests green.
- **Behavior preservation before behavior improvement.** W1/W2 first centralize the already
  intended v1.00.12 semantics. NP-2/NP-3/NP-4/NP-6 behavior changes land only in their owning
  waves with dedicated red tests.
- **No provider rewrite.** Wire encoders/decoders/event normalizers stay where they are unless a
  minimal signature change is required to consume policy/context.
- **No adapter-local compatibility logic.** Legacy `sse-read` mapping exists in one resolver
  only.
- **No string-as-protocol.** Human messages may be rendered from structured failures; retry and
  circuit-breaker decisions may not parse them.
- **No hidden cleanup ownership.** Every opened response port has one explicit owner and one
  deterministic cleanup path.
- **No unbounded new knob.** Any new operator-configurable duration has a semantic name and a
  documented safety ceiling or a documented reason it is a total budget rather than a liveness
  detector.
- **No shell mutation of `.rkt` files.** Follow repository editing/format rules from v1.00.12.
- **Explicit `git add <paths>`; protected main; per-PR README metrics sync; established format,
  architecture, security, and release gates remain mandatory.**
- **TCP keepalive remains out of scope.** NP-5 requires an FFI-level socket change and is not
  necessary once application-level lifecycle bounds are correct.
- Provider-specific rate-limit algorithms, retry-count policy, circuit-breaker thresholds, and
  TUI redesign are out of scope except where they consume the new structured metadata.

---

## 6. Waves

### W0 — Characterization + red architecture contracts

**Objective:** prove the current ownership leaks and freeze intended behavior before moving code.

**Tasks**

1. Inventory every consumer/definition of:
   - `effective-sse-read-timeout-for`;
   - model `request` timeout accessors;
   - `held-request-detect-secs`;
   - `http-stream-timeout-default`;
   - `max-total-timeout`;
   - `stream-sse-events` timeout arguments;
   - `make-provider-http-request`;
   - response header values returned by `http-sendrecv`;
   - `parse-retry-after`;
   - `received-heartbeats?`.
   Record adapter-by-adapter ownership in the wave report.
2. Characterize current total-duration semantics on all adapters (including the
   openai-compatible `max(600, 2×request)` behavior and the hardcoded 600 s paths identified by
   the audit). This evidence decides the single W1 `stream-total` formula; no accidental formula
   change is allowed during extraction.
3. Add `tests/test-request-network-policy.rkt` compile-red for:
   - semantic policy struct/value;
   - resolver with request/legacy/new-field precedence;
   - invariant sweep over large/small values;
   - `sse-read` cannot affect connect/initial/content.
4. Add `tests/test-provider-network-policy-conformance.rkt` red harness. For each of
   openai-compatible, anthropic, azure-openai, gemini, intercept the common stream/body boundary
   and assert it receives the expected resolved policy.
5. Add red structured-failure tests:
   - HTTP 429 fixture with `Retry-After: 17`;
   - response headers visible in failure context;
   - retry code consumes structured retry-after without reading error text.
6. Add red cleanup tests for non-streaming/eager body:
   success, status-check raise, read timeout, request timeout, cancellation. Instrument the mock
   port so close count is observable and exactly one.
7. Add red heartbeat-classification test: heartbeat-only stream records liveness and must not be
   classified identically to a zero-byte/zero-heartbeat held request.
8. Add architecture/lint test (red on current code) that restricts raw timeout-config ownership
   to the future policy module plus temporary compatibility allowlist.
9. Register all new tests per repository metadata/tag/inventory conventions and record both
   compile-red and assertion-red failures.

**Files:** tests only + wave report.

**Gate:** all existing v1.00.12 focused suites remain green; new suites are intentionally red
with documented reasons.

**Exit criterion:** current behavior and ownership map are evidence-backed; no production diff.

---

### W1 — Semantic policy module + legacy config migration

**Objective:** introduce one pure owner for timeout semantics without yet redesigning request
execution.

**Tasks**

1. Create the policy module selected in W0 (candidate `llm/request-policy.rkt`) containing:
   - `request-network-policy`;
   - pure resolver;
   - safety constants/caps;
   - validation;
   - legacy `sse-read` compatibility mapping;
   - explicit new-field precedence.
2. Move the v1.00.12 phase resolver semantics out of `llm/stream.rkt`.
   If existing tests/imports require it, retain a thin compatibility re-export with a deprecation
   comment; no semantic logic remains in stream mechanism.
3. Add config accessors for `thinking-idle` and `body-read`.
   The resolver, not adapters, resolves:
   explicit semantic key > legacy `sse-read` > default.
4. Freeze the W0-evidenced total-stream formula in policy. Document why it is a total budget
   rather than an inactivity detector.
5. Add pure property tests:
   - every duration positive;
   - initial ≤120;
   - thinking ≤300;
   - content ≤60;
   - connect/TTFB ≤ policy cap;
   - legacy `sse-read` cannot widen safety detectors;
   - explicit semantic keys override legacy alias;
   - request budget clamps phase values where required.
6. Add one non-fatal config deprecation signal for legacy `sse-read` if the repository already
   has a config-warning mechanism. If not, docs-only deprecation is acceptable; do not invent a
   new global warning subsystem in this wave.

**Files:** policy/config modules, `llm/stream.rkt` compatibility surface, tests, README metrics.

**Gate:** policy suite green; v1.00.12 SSE-bound suites green; fast gate; architecture suite
allows only the temporary adapter consumers needed for W2.

**Exit criterion:** one tested semantic resolver exists; no adapter behavior changed beyond
equivalent values.

---

### W2 — Mandatory policy consumption across all adapters

**Objective:** adapters consume policy; they no longer author generic timeout semantics.

**Tasks**

1. Change the provider request construction path so a resolved `request-network-policy` is
   created once before provider network execution and passed explicitly.
2. Update openai-compatible, anthropic, azure-openai, and gemini streaming paths to consume the
   resolved policy fields when calling `stream-sse-events`.
3. Update eager/non-streaming body-read paths to consume `body-read-budget-secs`.
4. Remove adapter-local `or sse-read-timeout ...`, phase constants, raw timeout accessors, and
   hardcoded 120/60/60/600 policy assembly where those values represent generic lifecycle
   policy.
5. Keep provider-specific wire/protocol behavior untouched.
6. Make `tests/test-provider-network-policy-conformance.rkt` green for all adapters.
7. Tighten the architecture test: provider adapters may not directly import legacy timeout
   config accessors or define generic phase timeout policy.
8. If v1.00.12 already wired all adapters through its transitional resolver, this wave should be
   predominantly relocation/signature work with no observable timeout change. If v1.00.12
   deferred parity, record the intentional parity change separately in the wave report.

**Files:** provider adapter modules, request construction/wiring modules, policy module, tests,
README metrics.

**Gate:** all provider focused suites; policy/conformance; stream suites; fast gate; architecture
suite.

**Exit criterion:** a new provider adapter cannot obtain correct generic timeout behavior by
copying constants; it must consume resolved policy.

---

### W3 — Request lifecycle ownership: headers, cleanup, structured failures

**Objective:** make generic HTTP lifecycle metadata survive the transport boundary and remove
string parsing from retry decisions.

**Tasks**

1. Introduce/extend the common request lifecycle boundary (prefer the existing
   `llm/http-helpers.rkt` if it can own this cleanly; create a dedicated
   `llm/request-executor.rkt` only if W0/W2 show that adding ownership there would otherwise
   couple provider protocol code back into HTTP helpers).
2. Preserve HTTP status and response headers long enough to populate a structured
   network-failure context.
3. Add structured fields/context for:
   failure kind, phase, elapsed/idle duration where available, received-data?,
   received-heartbeats?, chars, HTTP status, headers, parsed retry-after.
4. Keep v1.00.12 human timeout text compatible by rendering it from the structured context.
5. Change auto-retry to consume structured retry metadata. Delete/retire the path that feeds the
   entire exception message to `parse-retry-after`.
6. Parse `Retry-After` from actual response headers:
   - delta-seconds required;
   - HTTP-date required if a deterministic clock seam exists; otherwise add the seam and tests
     in this wave rather than parsing wall clock directly in test code.
7. Repair non-streaming/eager-body port ownership:
   - close on normal body completion;
   - close on status-checker raise;
   - supply timeout cleanup to `call-with-request-timeout`;
   - close on cancellation;
   - preserve existing streaming will/custodian safety net.
8. Make close operations idempotent or structurally single-owner so double-close races are not
   introduced.
9. Make W0 structured-failure/Retry-After/cleanup tests green.

**Files:** HTTP helper/request lifecycle module, network exception definitions if separate,
`runtime/auto-retry.rkt`, eager-body consumers, tests, docs comments, README metrics.

**Gate:** focused HTTP/stream/auto-retry/provider telemetry suites; cleanup tests; fast gate;
security suite (headers/error handling).

**Exit criterion:** retry policy needs no human error-string parsing, and both streaming and
non-streaming paths have deterministic resource ownership.

---

### W4 — Liveness + deadline completion

**Objective:** complete the lifecycle policy so every blocking phase has explicit liveness and
deadline semantics.

**Tasks**

1. Add dedicated connect/TTFB enforcement from `request-network-policy`.
   Required behavioral bound: an established-but-silent request cannot wait the full 900 s model
   request budget. Initial policy is `(min request-budget 120)` unless W0 evidence justified a
   tighter value and documented it.
2. Emit structured `connect/ttfb` failure metadata and verify cleanup fires before retry.
3. Make total-duration enforcement hard enough that a blocking read is bounded by:
   `min(phase-idle-timeout, remaining-total-budget)`.
   This removes NP-7's possible overshoot by a full phase window.
4. Make `held-request?` / equivalent liveness classification consult heartbeat metadata:
   - zero data + zero heartbeat + initial timeout = held/dead peer;
   - heartbeat-only = live-but-no-content, not identical to a dead peer;
   - existing empty/comment flood ceiling remains a separate bounded protocol-safety condition.
5. Verify cancellation remains responsive and cannot be reclassified as timeout.
6. Add matrix tests covering:
   connect/TTFB, initial, thinking, content, total deadline, heartbeat-only, empty/comment flood,
   cancellation.
7. Update the architecture test so **all** generic lifecycle constants and raw config resolution
   are centralized; remove transitional allowlists from W1/W2.

**Files:** request lifecycle/policy modules, stream mechanism, auto-retry classification,
provider wiring only if signatures require it, tests, README metrics.

**Gate:** policy + conformance + stream + auto-retry + provider focused suites; fast gate;
**broad gate**; architecture; security; lint-all.

**Exit criterion:** every generic blocking phase is bounded by one resolved policy and every
retry-relevant liveness fact is structured.

---

### W5 — Documentation, migration proof, integration verification + release

**Objective:** prove the architecture, document operator semantics, and ship v1.00.13.

**Tasks**

1. `docs/provider-retry.md`:
   - replace the transitional v1.00.12 timeout matrix with the resolved-policy matrix;
   - document deadline vs inactivity timeout explicitly;
   - document adapter-independent policy ownership;
   - document `thinking-idle`, `body-read`, and legacy `sse-read` precedence/deprecation;
   - document connect/TTFB and total-duration semantics;
   - document structured Retry-After behavior and heartbeat classification.
2. Add a short architecture note (existing architecture docs if present) with the dependency
   direction:
   config → policy → adapter/request lifecycle → mechanism → structured outcome → retry/TUI.
3. `CHANGELOG.md`: v1.00.13 entries:
   - Changed: centralized request-network policy;
   - Added: semantic timeout config + cross-adapter conformance;
   - Fixed: Retry-After header propagation; eager-body cleanup; connect/TTFB bound;
   - Fixed: heartbeat-aware held-request classification; hard remaining-budget reads;
   - Deprecated: legacy `sse-read`.
4. Migration tests:
   - an old config containing only `request` + `sse-read` produces intended compatible
     thinking/body values;
   - new explicit fields win over legacy aliases;
   - DeepSeek `sse-read:600` still cannot widen initial/content and caps thinking at 300;
   - Kimi legacy `sse-read:300` still receives the intended body-read/thinking allowance.
5. Final architecture scan: no provider adapter contains raw legacy timeout accessors or generic
   phase policy constants outside documented protocol-specific exceptions.
6. Local gates with recorded evidence: focused, fast, tui, arch, workflows, security, lint-all,
   release-readiness; broad/full-regression according to then-current repository release
   practice.
7. Release PR → CI green → squash-merge → annotated `v1.00.13` tag → release workflow; verify
   established release assets.
8. Write STATE/VALIDATION with:
   - before/after ownership graph;
   - final policy matrix;
   - adapter conformance evidence;
   - Retry-After evidence;
   - cleanup close-count evidence;
   - connect/TTFB and remaining-budget evidence;
   - legacy-config migration evidence.
9. Mirror planning/state/validation docs to `q/docs/planning/` using the established docs PR
   process.

**Files:** docs/changelog/version files/README/planning mirrors.

**Gate:** all required suites + release workflow `success` + published release.

**Exit criterion:** every §4 definition-of-done item has test or release evidence.

---

## 7. Architecture / Conformance Gates

The milestone should leave behind durable prevention, not only implementation tests.

### AC-1 — Raw config ownership

Only the request-policy module may translate:

```text
request
thinking-idle
body-read
legacy sse-read
```

into generic network durations.

Provider adapters may receive already-resolved values only.

### AC-2 — Generic timeout constants

Generic values such as 120 s held/initial, 300 s thinking ceiling, 60 s content gap, and the
connect/TTFB bound have one owner. Adapter-local copies fail the architecture check unless a
documented provider protocol requirement proves the value is not generic policy.

### AC-3 — Conformance harness

For each supported streaming adapter, the same policy fixture must produce the same mechanism
arguments:

```text
initial
thinking
content
total
```

Provider-specific parsing output may differ; generic lifecycle bounds may not.

### AC-4 — Failure metadata

Retry tests must assert structured fields, not regexes over human text, except tests explicitly
covering presentation stability.

### AC-5 — Cleanup

Every request implementation must satisfy the shared close-once lifecycle suite or an equivalent
adapter-specific wrapper that proves the same contract.

---

## 8. Risk & Rollback

| Risk | Likelihood | Mitigation |
|------|------------|------------|
| Large architecture change hides behavior drift | Medium | Characterize first in W0; W1 policy extraction is behavior-preserving; later behavior changes are isolated by wave and red tests |
| Moving the v1.00.12 resolver breaks imports/tests | Medium | Temporary compatibility re-export for one release; architecture test prevents new consumers |
| Legacy `sse-read` users lose Kimi/eager-body headroom | Medium | Explicit compatibility mapping to both `thinking-idle` and `body-read`; migration fixtures for Kimi and DeepSeek |
| New connect/TTFB bound is too tight for a legitimately slow provider | Low-Medium | Start at 120 s unless W0 evidence supports tighter; request budget remains separate; add a bounded semantic override only with evidence |
| Standardizing total-duration policy changes long anthropic/azure/gemini streams | Medium | W0 freezes current behavior; W1 records chosen formula; W2 parity change called out separately if v1.00.12 deferred it |
| Preserving response headers leaks sensitive values into logs | Medium | Structured context may retain only retry-relevant/safe headers or redact before presentation; never dump arbitrary authorization/cookie headers |
| Cleanup refactor double-closes ports | Low-Medium | Close-count instrumentation + idempotent/single-owner cleanup tests |
| Retry-After causes unexpectedly long waits | Low | Existing retry ceiling/cancellation rules still apply; tests assert cancellation and cumulative ceiling |
| Heartbeat-aware classification permits infinite heartbeat-only streams | Low | Total deadline + empty/comment ceiling remain authoritative; heartbeat changes classification, not unbounded duration |
| Structured exception changes break consumers | Medium | Preserve public exception hierarchy/message prefix where practical; migrate in-repo consumers in same wave; architecture tests forbid string parsing |
| Scope becomes a provider rewrite | Medium | Non-negotiable provider-boundary rule; each wave lists allowed files/behavior; stop and split if wire-format logic starts moving |

### Rollback strategy

- **W1** is independently revertible because it is a pure extraction plus compatibility mapping.
- **W2** is signature/wiring relocation; revert restores v1.00.12 adapter behavior.
- **W3** can be reverted without changing phase-bound policy if header/cleanup/error changes cause
  compatibility problems.
- **W4** behavior changes (connect/TTFB, heartbeat classification, hard remaining-budget reads)
  are one separately reviewable/revertible wave.
- Do not combine W1–W4 into one PR; rollback granularity is part of the design.

---

## 9. Verification Hooks (for VALIDATION)

- Unit: request policy resolver matrix + property sweep.
- Compatibility: old `request` + `sse-read` configs for DeepSeek/Kimi.
- Cross-adapter: one policy fixture observed at openai/anthropic/azure/gemini mechanism boundary.
- Architecture: only policy module reads raw generic timeout config.
- Connect/TTFB: established mock connection, no headers → bounded failure + cleanup.
- Stream liveness: initial/thinking/content gaps use resolved policy, not raw config.
- Total budget: per-read wait ≤ remaining budget; no phase-sized overshoot.
- Headers: 429 fixture preserves `Retry-After`; retry delay derives from header metadata.
- Cleanup: response port close count exactly one across success/error/timeout/cancel.
- Heartbeats: heartbeat-only stream is live-but-no-content, not dead-held; still bounded by total
  and flood limits.
- Presentation: v1.00.12 phase/liveness suffix still rendered from structured failure context.
- Post-release smoke: DeepSeek and Kimi legacy configs behave as migration tests predict; no
  adapter-specific timeout-policy divergence appears in telemetry.

---

## 10. Architectural End State

After v1.00.13, the intended dependency direction is:

```text
                  raw config
                      │
                      ▼
            request-policy module
                      │
             resolved policy
                      │
                      ▼
             provider adapter
        (provider specifics only)
                      │
                      ▼
         request lifecycle executor
   (HTTP ownership/headers/deadlines)
                      │
            ┌─────────┴─────────┐
            ▼                   ▼
        SSE mechanism       body mechanism
            │                   │
            └─────────┬─────────┘
                      ▼
        structured outcome/failure
                      │
             ┌────────┴────────┐
             ▼                 ▼
          retry              TUI/log
         policy            presentation
```

The architectural invariant is stronger than “the resolver returns the right values”:

> **Every provider request must consume one resolved lifecycle policy, preserve one structured
> lifecycle/failure contract, and leave generic policy decisions outside adapters and transport
> mechanisms.**

That is the regression-prevention boundary v1.00.12 did not have.
