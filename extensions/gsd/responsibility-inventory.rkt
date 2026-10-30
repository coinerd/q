#lang racket
;; GSD Responsibility & Effect Inventory — machine-readable evidence (v0.99.87 W2)
;; Issue #9214. Consumed by tests/test-gsd-responsibility-inventory.rkt.
;;
;; Each module entry: (entry module-file domain effects params deps)
;; domain ∈ (pure-planning campaign-state transition-logic ui-glue persistence
;;            event-projection command-parsing compatibility-facade
;;            external-ports)
;; effects ∈ (fs-write fs-read fs-rename fs-delete mkdir dir-list sha256 git
;;            subprocess parameterize make-param path-ops network github
;;            dynamic-require)
;;
;; W5 (v1.00.31 simplification): effects are the scanner-visible primitives a
;; module's OWN source calls. A delegated effect (journal write, proof capture,
;; scratch allocation) is declared by its owning module and shows up here as a
;; dependency, never as a waiver: `open-output-file` is now measured as fs-write
;; too, so a module that opens its own output port cannot hide behind an
;; atomic-write helper in somebody else's namespace.

(require racket/contract)

(struct entry (module-file domain effects params deps) #:transparent)

(define (make-entry module-file domain effects params deps)
  (entry module-file domain effects params deps))

(define inventory
  (list
   ;; Evidence recovery: only the format module is pure. The controller
   ;; delegates durable writes and execution to the ledger/probe boundaries.
   (make-entry "evidence-repair-format.rkt"
               'transition-logic
               '(sha256)
               '()
               '("json" "racket/list" "racket/string" "util/json/checksum"))
   (make-entry "owned-lane.rkt"
               'transition-logic
               '(parameterize)
               '()
               '("wave-runner-port" "campaign-state"
                                    "campaign-repository"
                                    "campaign-budgets"
                                    "delivery-receipt"
                                    "author-execution"
                                    "author-checkpoint"
                                    "composition-root"
                                    "verification-job"))
   (make-entry "wave-scope.rkt"
               'transition-logic
               '()
               '()
               '("campaign-state" "wave-executor"
                                  "review-worktree"
                                  "author-checkpoint"
                                  "delivery-publication"
                                  "attempt-artifacts"))
   (make-entry
    "evidence-author-journal.rkt"
    'campaign-state
    '()
    '()
    '("delivery-journal" "review-proof" "author-scope" "evidence-repair-format" "composition-root"))
   ;; `dynamic-require` is a REJECTED-FORM marker in the static helper-closure
   ;; walk, never an executed effect; the scanner reads source text.
   (make-entry "evidence-discovery.rkt"
               'persistence
               '(git parameterize path-ops dynamic-require)
               '()
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/port"
                               "racket/string"
                               "racket/system"
                               "review-proof-format"
                               "evidence-repair-format"
                               "evidence-runner-coverage"
                               "util/json/checksum"
                               "scripts/gsd-evidence-probe-runner"))
   (make-entry
    "evidence-probe.rkt"
    'persistence
    '(fs-read fs-write fs-rename fs-delete mkdir sha256 git parameterize path-ops dynamic-require)
    '()
    '("json" "racket/file"
             "racket/path"
             "racket/list"
             "racket/string"
             "racket/system"
             "racket/port"
             "racket/runtime-path"
             "evidence-repair-format"
             "evidence-runner-coverage"
             "review-proof"
             "verification-job"
             "composition-root"
             "util/json/checksum"))
   (make-entry "evidence-readiness.rkt"
               'campaign-state
               '(fs-read git parameterize path-ops)
               '()
               '("json" "racket/list"
                        "racket/path"
                        "racket/string"
                        "racket/system"
                        "delivery-journal"
                        "review-proof"
                        "evidence-repair-format"
                        "evidence-repair-ledger"
                        "evidence-probe"
                        "evidence-discovery"))
   (make-entry "evidence-repair-ledger.rkt"
               'persistence
               '(fs-read mkdir path-ops)
               '()
               '("json" "racket/file"
                        "racket/path"
                        "racket/list"
                        "review-proof"
                        "evidence-repair-format"
                        "evidence-probe"))
   ;; Atomic ledger output/locking delegates fs-write/fs-rename to racket/file;
   ;; these are invisible to the legacy marker scanner, as above.
   (make-entry
    "evidence-repair-controller.rkt"
    'campaign-state
    '()
    '()
    '("evidence-readiness" "evidence-repair-format" "evidence-repair-ledger" "evidence-probe"))
   (make-entry "evidence-runner-coverage.rkt"
               'persistence
               '(fs-read git parameterize path-ops dynamic-require)
               '()
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/port"
                               "racket/promise"
                               "racket/runtime-path"
                               "racket/string"
                               "racket/system"
                               "review-proof-format"))
   ;; Effects below are source-marker effects, not a transitive purity claim.
   ;; Admission reads through readiness/scope and reserves durable permission
   ;; through delivery-journal; it neither dispatches nor fabricates rejection.
   (make-entry
    "evidence-author-repair.rkt"
    'campaign-state
    '()
    '()
    '("racket/list" "racket/string" "author-scope" "evidence-readiness" "delivery-journal"))
   ;; Authenticate literal FILES against the frozen snapshot; actual file reads.
   (make-entry
    "author-scope.rkt"
    'persistence
    '(fs-read)
    '()
    '("racket/file" "racket/list" "racket/string" "plan-snapshot" "plan-types" "wave-docs"))
   ;; Authenticated task data and prompt/tool projections; construction reads
   ;; authority through its owner and reads frozen snapshot intent. No dispatch,
   ;; budget or approval authority.
   (make-entry "supplemental-author-contract.rkt"
               'transition-logic
               '(fs-read make-param)
               '(current-supplemental-author-contract)
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/string"
                               "plan-snapshot"
                               "wave-docs"
                               "supplemental-evidence-authority"
                               "supplemental-evidence-format"
                               "util/worker-tool-policy"
                               "../gsd-planning/execution-policy"))
   ;; Owner-controlled phase dispatch and Git commits. Snapshot/journal reads,
   ;; durable launch intent and registered checks delegate to their owners.
   ;; Path normalization uses path->complete-path/simplify-path, outside the
   ;; legacy path-ops marker (string->path/path->string).
   (make-entry "author-execution.rkt"
               'external-ports
               '(git parameterize)
               '()
               '("racket/list" "racket/path"
                               "racket/port"
                               "racket/string"
                               "racket/system"
                               "red-checkpoint"
                               "delivery-receipt"
                               "delivery-journal"
                               "wave-runner-port"
                               "policy"
                               "supplemental-author-contract"
                               "../gsd-planning/execution-policy"))
   ;; Resume policy mutates campaign state and delegates persistence to the
   ;; repository. Approved parked-entry admission serializes with revocation
   ;; under the authority lock; the controller authenticates the exact context.
   ;; Retained checkout authentication delegates Git to receipts.
   (make-entry "author-checkpoint.rkt"
               'campaign-state
               '(path-ops)
               '()
               '("racket/path" "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "delivery-receipt"
                               "supplemental-evidence-authority"
                               "supplemental-evidence-controller"
                               "wave-executor"))
   ;; Lost-session completion is an owner effect, not a model phase. The
   ;; deadline/registry and Git runner own cancellation and process cleanup;
   ;; receipt, proof and publication persistence delegates to their owners.
   (make-entry "author-session-recovery.rkt"
               'campaign-state
               '(git subprocess parameterize make-param path-ops)
               '(current-gsd-author-recovery-timeout-sec)
               '("racket/list" "racket/path"
                               "racket/string"
                               "author-scope"
                               "owner-correction"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "delivery-receipt"
                               "delivery-verifier"
                               "delivery-publication"
                               "evidence-author-journal"
                               "review-proof"
                               "wave-runner-port"
                               "system-adapters"
                               "composition-root"
                               "verification-job"
                               "wave-executor"
                               "attempt-artifacts"
                               "sandbox/subprocess"))
   ;; Separate owner metadata authority. The deterministic interpreter has no
   ;; I/O; the owner protocol owns private persistence and exact Git effects.
   (make-entry "generated-readme.rkt"
               'pure-planning
               '()
               '()
               '("racket/list" "racket/port" "racket/string"))
   (make-entry "owner-correction.rkt"
               'persistence
               '(fs-write fs-read
                          fs-rename
                          fs-delete
                          mkdir
                          sha256
                          git
                          subprocess
                          parameterize
                          make-param
                          path-ops)
               '(current-owner-correction-root current-owner-correction-cut)
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/port"
                               "racket/string"
                               "generated-readme"
                               "author-scope"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "delivery-receipt"
                               "evidence-author-journal"
                               "review-proof"
                               "../gsd-planning/execution-policy"
                               "util/json/checksum"
                               "sandbox/subprocess"))
   ;; Durable, locked preimplementation RED and candidate assertion capture,
   ;; never a full Verify receipt. Atomic output/locking delegates writes and
   ;; renames to racket/file; registered child execution to verification-job.
   (make-entry "red-checkpoint.rkt"
               'persistence
               '(fs-read mkdir sha256 git parameterize path-ops)
               '()
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/port"
                        "racket/runtime-path"
                        "racket/string"
                        "racket/system"
                        "review-proof"
                        "delivery-receipt"
                        "delivery-verifier"
                        "composition-root"
                        "verification-job"
                        "evidence-repair-format"
                        "evidence-runner-coverage"
                        "author-scope"
                        "util/json/checksum"
                        "scripts/gsd-evidence-probe-runner"))
   ;; pure planning
   (make-entry "shared.rkt" 'pure-planning '() '() '("racket/string" "racket/list"))
   (make-entry "wave-status.rkt" 'pure-planning '() '() '("racket/string"))
   ;; Campaign terminal values, structured diagnostics, and pure stop text.
   (make-entry "campaign-result.rkt" 'pure-planning '() '() '("racket/string"))
   (make-entry "stall-policy.rkt"
               'transition-logic
               '(parameterize make-param
                  )
               '()
               '("racket/string" "policy"))
   (make-entry "infra-retry-policy.rkt"
               'transition-logic
               '(path-ops)
               '()
               '("racket/string" "../../util/loop-result" "policy"))
   (make-entry "freshness.rkt" 'transition-logic '(fs-read make-param) '() '("racket/string"))
   (make-entry "command-types.rkt" 'compatibility-facade '() '() '())
   (make-entry "command-parser.rkt"
               'command-parsing
               '()
               '()
               '("util/command-helpers" "util/command-types"))
   (make-entry "plan-types-parser.rkt" 'pure-planning '() '() '("racket/string" "racket/list"))
   (make-entry "plan-types.rkt"
               'pure-planning
               '()
               '()
               '("racket/string" "racket/list" "racket/format"))
   (make-entry "planning-paths.rkt" 'pure-planning '() '() '("racket/path" "racket/string"))
   (make-entry "planning-artifact-validation.rkt"
               'command-parsing
               '(fs-read dir-list path-ops)
               '()
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/port"
                               "racket/string"
                               "plan-types"
                               "plan-validator"
                               "wave-docs"
                               "plan-snapshot"
                               "planning-artifact-ownership"
                               "planning-paths"
                               "verify-command-format"
                               "campaign-state"
                               "campaign-repository"))
   (make-entry "plan-validator.rkt"
               'pure-planning
               '()
               '()
               '("racket/format" "racket/string" "plan-types"))
   (make-entry "context-bundle.rkt"
               'pure-planning
               '()
               '()
               '("racket/contract" "racket/string" "util/error"))
   ;; W5: the authoring template is GENERATED from wave-docs' required
   ;; header/section constants (R-003/R-015) and the executor scratch guidance
   ;; renders the ONE trusted per-session root. `path->string` is a real
   ;; marker effect of the rendered guidance; the scratch allocation ITSELF is a
   ;; delegated effect owned by tools/scratch.rkt (session-scratch-root /
   ;; ensure-scratch-root!), so it is a dependency here and never a second
   ;; allocator. The module now imports racket/path, so it is no longer in the
   ;; no-I/O pure sweep.
   (make-entry "prompts.rkt"
               'pure-planning
               '(path-ops)
               '()
               '("racket/format" "racket/path"
                                 "racket/string"
                                 "supplemental-author-contract"
                                 "plan-types"
                                 "wave-executor"
                                 "wave-docs"
                                 "tools/scratch"))
   ;; R76 (BUG-0076): shared leaf delivery-root resolver — the root/layout
   ;; decision logic sits below every call site (plan-context-builder,
   ;; wave-executor, delivery-verifier, ... all import it). Like its caller
   ;; plan-context-builder it probes Git's own answers, so the same effect
   ;; family applies; the run-git seam parameter is the only state.
   (make-entry "git-root-layout.rkt"
               'pure-planning
               '(git make-param path-ops parameterize)
               '(current-git-root-layout-run-git)
               '("racket/file" "racket/path" "racket/port" "racket/string" "racket/system"))
   (make-entry "plan-context-builder.rkt"
               'pure-planning
               '(git make-param path-ops fs-read parameterize)
               '()
               '("racket/string" "racket/port"
                                 "racket/list"
                                 "racket/path"
                                 "racket/system"
                                 "plan-types"
                                 "effect-ports"
                                 "composition-root"))
   ;; campaign state (5)
   (make-entry "runtime-state-types.rkt" 'campaign-state '() '() '("racket/set"))
   (make-entry "session-state.rkt"
               'campaign-state
               '(make-param)
               '("current-gsd-ctx" "current-gsd-session-id")
               '("racket/contract" "racket/set" "runtime-state-types"))
   (make-entry "campaign-state.rkt"
               'campaign-state
               '(fs-read sha256)
               '()
               '("racket/file" "racket/string"
                               "racket/port"
                               "racket/path"
                               "racket/list"
                               "racket/format"
                               "racket/contract"
                               "wave-docs"
                               "planning-paths"
                               "util/json/checksum"))
   ;; v0.99.90 W1: .rktd storage boundary owns persistence effects
   (make-entry "campaign-repository.rkt"
               'persistence
               '(fs-read fs-write fs-rename fs-delete mkdir dir-list path-ops)
               '()
               '("racket/file" "racket/path"
                               "racket/match"
                               "racket/format"
                               "racket/string"
                               "racket/contract"
                               "campaign-state"
                               "planning-paths"
                               "plan-snapshot"))
   ;; W3: the ONE /go effect interpreter. It performs what the pure
   ;; campaign-decision decides and never reconstructs a route. The process-safe
   ;; lease (OS advisory lock + mkdir of the campaign directory) moved OUT to
   ;; campaign-lease.rkt, so this module no longer declares the mkdir effect and
   ;; no longer imports sandbox/gateway-bridge. Routing reads typed kinds,
   ;; routes and codes; operator text is carried, never parsed.
   (make-entry "go-orchestrator.rkt"
               'campaign-state
               '(make-param parameterize)
               '()
               '("racket/format" "racket/file"
                                 "racket/match"
                                 "racket/os"
                                 "racket/string"
                                 "racket/system"
                                 "util/version"
                                 "campaign-state"
                                 "campaign-repository"
                                 "campaign-result"
                                 "campaign-lease"
                                 "campaign-request"
                                 "campaign-decision"
                                 "campaign-budgets"
                                 "stall-policy"
                                 "infra-retry-policy"
                                 "freshness"
                                 "attempt-artifacts"
                                 "wave-completion"
                                 "wave-runner-port"
                                 "wave-docs"
                                 "wave-status"
                                 "wave-scope"
                                 "wave-executor"
                                 "wave-outcome-reporting"
                                 "projection-effects"
                                 "delivery-verifier"
                                 "delivery-handoff"
                                 "delivery-receipt"
                                 "delivery-publication"
                                 "delivery-coordinator"
                                 "delivery-finalize"
                                 "tracker-resume"
                                 "util/loop-result"
                                 "system-adapters"
                                 "plan-context-builder"
                                 "policy"
                                 "prompts"
                                 "verification-repair"
                                 "owned-lane"
                                 "author-checkpoint"
                                 "review-dispatch"
                                 "review-worktree"
                                 "notify"
                                 "runtime/iteration/step-executor"
                                 "util/iteration/decision"
                                 "runtime/provider-retry"
                                 "agent/state"))
   ;; W3: ONE pure campaign decision per /go pass — complete / launch / resume /
   ;; finalize / block / cancel, with the lane route table. No I/O, no
   ;; parameters, no journal writes, and no text matching: only typed kinds,
   ;; statuses, wave indices and the route symbol choose behaviour.
   (make-entry "campaign-decision.rkt" 'transition-logic '() '() '("campaign-state"))
   ;; W3: the ONE process-safe campaign lease (OS advisory lock on a stable
   ;; lock file, owner/pid diagnostics, busy-stop explanation). It only
   ;; acquires/releases/describes; run-campaign! still owns the dynamic-wind that
   ;; releases on every exit path. mkdir parent directories, opens its own
   ;; output port (fs-write) and renders the lock path.
   (make-entry
    "campaign-lease.rkt"
    'persistence
    '(fs-write fs-read mkdir path-ops)
    '()
    '("racket/file" "racket/format" "racket/os" "racket/string" "campaign-result" "planning-paths"))
   ;; W3: the interface-safe /go request VALUE plus its process-local token
   ;; registry (one locked hash) and the ONE attempt-prompt wrapper. The struct
   ;; carries durable campaign identity and the callbacks the interpreter needs,
   ;; the registry maps an opaque token to a value so TUI/GUI/SDK hook payloads
   ;; never carry higher-order values, and call-with-request-scratch-prompt turns
   ;; the request's scratch token into THIS attempt's advertised root (its
   ;; temporary registry entry is released when the attempt ends, leaving the
   ;; adopted session's own binding intact). No durable
   ;; write; the scratch allocation itself is delegated to tools/scratch.rkt.
   (make-entry "campaign-request.rkt"
               'campaign-state
               '()
               '()
               '("racket/format" "racket/string" "campaign-state" "delivery-handoff" "tools/scratch"))
   ;; v1.00.30 (delivery runtime): the coordinator-owned authenticated
   ;; delivery handoff — loop decision, delivered-proof validation,
   ;; pending→delivered ledger, and the predecessor resolver. Factored
   ;; OUT of go-orchestrator so the wave-advance gate and the final
   ;; full-loop delivery check share ONE proof source (BUG-0064/#9620).
   ;; Effects reflect the scanner-visible markers; the durable handoff
   ;; ledger is additionally written atomically via
   ;; call-with-atomic-output-file / make-parent-directory* (fs-write /
   ;; fs-rename / mkdir, invisible to the marker scanner).
   (make-entry "delivery-handoff.rkt"
               'campaign-state
               '(fs-read path-ops parameterize subprocess)
               '()
               '("json" "racket/file"
                        "racket/format"
                        "racket/path"
                        "racket/runtime-path"
                        "racket/string"
                        "campaign-state"
                        "campaign-repository"
                        "campaign-decision"
                        "git-root-layout"
                        "delivery-journal"
                        "delivery-receipt"
                        "planning-paths"
                        "sandbox/subprocess"))
   ;; JSON journal writes atomically; fs-write/fs-rename/mkdir are not visible
   ;; to the legacy effect marker scanner, as with delivery-handoff above.
   ;; W3 register F5: the remote-pending marker's clear deletes the sibling
   ;; file, so fs-delete is a declared effect of this module.
   ;; BUG-0088: guarded supplemental supersession/promotion writes (atomic
   ;; save!, write-ahead intents, fsync) are owner-side persistence here.
   (make-entry "delivery-journal.rkt"
               'persistence
               '(fs-read fs-write fs-rename fs-delete)
               '()
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "review-proof"
                        "supplemental-evidence-authority"))
   ;; Owned execution retention is effectful; formats stay pure.
   (make-entry
    "review-proof.rkt"
    'persistence
    '(fs-read fs-write fs-rename fs-delete mkdir dir-list path-ops make-param sha256 parameterize)
    '(current-owned-proof-root)
    '("json" "racket/file"
             "racket/list"
             "racket/path"
             "racket/port"
             "racket/string"
             "review-proof-format"
             "supplemental-evidence-proof"
             "verify-command-format"
             "delivery-verifier"
             "verification-job"
             "composition-root"
             "plan-snapshot"
             "plan-types"
             "wave-docs"
             "util/json/checksum"))
   (make-entry "review-proof-format.rkt"
               'transition-logic
               '(sha256)
               '()
               '("racket/list" "racket/string" "util/json/checksum"))
   (make-entry "verify-command-grammar.rkt" 'pure-planning '() '() '("racket/list" "racket/string"))
   (make-entry "verify-command-format.rkt"
               'transition-logic
               '()
               '()
               '("racket/string" "verify-command-grammar"))
   ;; Public promotion audit validation delegates to pure format modules.
   (make-entry "review-audit-promotion.rkt"
               'transition-logic
               '()
               '()
               '("supplemental-evidence-format" "evidence-repair-format"))
   (make-entry "review-audit-format.rkt"
               'transition-logic
               '()
               '()
               '("racket/list" "racket/string" "review-proof-format"))
   ;; Automatic independent review slice A state port: guarded durable
   ;; review-rejection authorization (recorded through delivery-journal's
   ;; declared atomic writes), superseded-attempt marking and the ordinary
   ;; failed retry transition on the in-memory campaign record. Pure with
   ;; respect to effects — every durable write is delegated to
   ;; delivery-journal; the caller persists the record.
   (make-entry "review-dispatch.rkt"
               'campaign-state
               '(fs-write subprocess make-param path-ops)
               '(current-gsd-auto-review-runner)
               '("json" "racket/file"
                        "racket/format"
                        "racket/list"
                        "racket/path"
                        "racket/runtime-path"
                        "racket/string"
                        "delivery-handoff"
                        "delivery-journal"
                        "sandbox/subprocess"))
   (make-entry "review-checkpoint.rkt"
               'campaign-state
               '()
               '()
               '("json" "racket/string"
                        "campaign-state"
                        "campaign-repository"
                        "delivery-journal"
                        "review-rejection"))
   (make-entry "review-worktree.rkt"
               'campaign-state
               '(git subprocess path-ops)
               '()
               '("racket/path" "racket/string"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "review-rejection"
                               "delivery-handoff"
                               "planning-paths"
                               "wave-executor"
                               "sandbox/subprocess"))
   ;; Creates isolated runtime sessions; the extension owns runtime/tools
   ;; composition, never an upward dependency from runtime into GSD.
   (make-entry "review-session.rkt"
               'external-ports
               '(mkdir subprocess parameterize)
               '()
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/string"
                        "racket/set"
                        "runtime/agent-session"
                        "runtime/settings"
                        "runtime/provider/provider-factory"
                        "util/event/event-bus"
                        "util/cancellation"
                        "util/loop-result"
                        "util/message/message"
                        "util/content/content-parts"
                        "review-tools"
                        "tools/permission-gate"
                        "sandbox/subprocess"
                        "sandbox/gateway-bridge"
                        "campaign-state"
                        "campaign-repository"
                        "campaign-budgets"
                        "delivery-journal"
                        "review-usage"
                        "review-proof"
                        "review-audit-format"
                        "review-audit-promotion"
                        "../gsd-planning/execution-policy"
                        "runtime/provider/model-registry"
                        "runtime/auth/auth-store"
                        "util/hash-helpers"
                        "review-worktree"
                        "planning-paths"))
   (make-entry "review-usage.rkt"
               'campaign-state
               '()
               '()
               '("campaign-state" "campaign-repository" "campaign-budgets"))
   (make-entry "review-rejection.rkt"
               'campaign-state
               '()
               '()
               '("racket/format" "racket/string" "campaign-state" "delivery-journal" "policy"))
   (make-entry "delivery-receipt.rkt"
               'campaign-state
               '(git subprocess path-ops parameterize make-param sha256)
               '()
               '("racket/path" "racket/string"
                               "delivery-journal"
                               "delivery-verifier"
                               "review-proof"
                               "campaign-state"
                               "plan-context-builder"
                               "sandbox/subprocess"
                               "util/credential-redaction"))
   ;; BUG-0079: coordinator-owned approved-head publication. Durable
   ;; approval-bound publication intent/confirmation (atomic sibling records,
   ;; fail-closed loader), strict-descendant repair-tail generations,
   ;; publication-only replay, and the durable-spare-branches union. The only
   ;; external effect is the trusted gsd-delivery.py publish-head action
   ;; through the single controller-environment credential boundary.
   (make-entry "delivery-publication.rkt"
               'campaign-state
               '(fs-read dir-list sha256 git subprocess make-param path-ops)
               '()
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/string"
                        "racket/runtime-path"
                        "campaign-state"
                        "attempt-artifacts"
                        "delivery-handoff"
                        "delivery-receipt"
                        "sandbox/subprocess"
                        "util/json/checksum"))
   ;; B2a (coordinator-delivery-execution-gap): deterministic journal-driven
   ;; delivery stage machine. Pure decision kernel: reloads journal + durable
   ;; record, re-runs delivery-receipt-blocker per effect, rejects
   ;; cancellation/fence/takeover, records separate delivery usage, advances
   ;; one journal stage per success. Receipt-bound artifact preflight uses
   ;; read-only Git worktree/status queries through the controller boundary.
   (make-entry "delivery-coordinator.rkt"
               'campaign-state
               '(subprocess path-ops git)
               '()
               '("racket/string" "racket/runtime-path"
                                 "json"
                                 "campaign-state"
                                 "campaign-repository"
                                 "delivery-receipt"
                                 "delivery-journal"
                                 "delivery-handoff"
                                 "planning-paths"
                                 "sandbox/subprocess"))
   ;; v1.00.33 recovery continuation: supported same-attempt repair-tail
   ;; recovery entry. Strict precheck + journal restore + independent clone
   ;; candidate + production verify; effects are delegated to the trusted
   ;; modules (delivery-journal reconcile, attempt-artifacts provenance,
   ;; delivery-handoff persist) — the module's own scanner-visible markers are
   ;; the git subprocess resolver, path operations, and the origin-policy
   ;; parameter. W3 review fix: the advisory lock now comes from
   ;; campaign-lease.rkt (stable lock path, can-update open, OS lock, owner
   ;; diagnostics), so this module no longer opens/writes the lock file. Its
   ;; remaining atomic writes go through call-with-atomic-output-file (not
   ;; visible to the legacy marker scanner, as with delivery-handoff).
   (make-entry "delivery-recovery.rkt"
               'campaign-state
               '(fs-read fs-rename fs-delete mkdir dir-list git path-ops parameterize make-param)
               '()
               '("racket/path" "racket/string"
                               "racket/system"
                               "json"
                               "campaign-state"
                               "campaign-lease"
                               "campaign-repository"
                               "delivery-journal"
                               "delivery-verifier"
                               "delivery-handoff"
                               "planning-paths"
                               "attempt-artifacts"))
   ;; BUG-0077: honest-completion state machine extracted from
   ;; go-orchestrator (attempt delivery provenance, park/finalize
   ;; checkpoint drivers, coordinator checkpoint decision). All durable
   ;; effects are delegated to the required modules (campaign-repository,
   ;; delivery-journal, wave-completion, attempt-artifacts); this module's
   ;; own source carries no marker-visible effects.
   ;; BUG-0084: declares the current-gsd-review-infra-retry-limit parameter
   ;; (audited in docs/architecture/parameter-inventory.rktd).
   ;;
   ;; W3: the campaign loop's checkpoints are now thin interpreters of the ONE
   ;; pure decision (campaign-decision.rkt) and the authenticated readback
   ;; adapter (delivery-handoff campaign-delivery-view-for-pass).
   (make-entry "delivery-finalize.rkt"
               'campaign-state
               '(make-param path-ops)
               '()
               '("racket/list" "racket/string"
                               "campaign-state"
                               "campaign-repository"
                               "campaign-result"
                               "campaign-decision"
                               "delivery-journal"
                               "delivery-handoff"
                               "wave-completion"
                               "attempt-artifacts"
                               "delivery-verifier"
                               "delivery-coordinator"
                               "reconciliation-checkpoint"
                               "tracker-production-wiring"
                               "tracker-reconciliation"
                               "review-checkpoint"
                               "author-checkpoint"
                               "author-session-recovery"
                               "supplemental-evidence-controller"))
   ;; BUG-0074 wiring: the post-DONE checkpoint policy. Decides WHEN the
   ;; receipt-authoritative pass is attempted and swallows its failures into a
   ;; typed result, so a tracker outage can never move a delivered wave off
   ;; `done`. Holds no credentials, no adapter and no subprocess: the only way
   ;; a tracker effect can happen is a reconciler a caller injected explicitly.
   (make-entry "reconciliation-checkpoint.rkt"
               'transition-logic
               '()
               '()
               '("racket/base" "racket/format" "delivery-handoff" "tracker-reconciliation"))
   ;; v1.00.33 W1: retry previously delivered DONE waves after process
   ;; restart. The explicit live plan/wave binding filters historical waves;
   ;; the receipt-authoritative reconciler rechecks proof on each retry.
   (make-entry "tracker-resume.rkt"
               'transition-logic
               '()
               '()
               '("campaign-state" "campaign-repository"
                                  "delivery-handoff"
                                  "reconciliation-checkpoint"
                                  "tracker-production-wiring"
                                  "tracker-reconciliation"))
   ;; BUG-0074: receipt-authoritative tracker reconciliation after durable
   ;; delivery. Reads durable campaign/handoff/journal state locally and
   ;; mirrors issue/board status through the injected GitHub effect port;
   ;; idempotent and restart-safe. No local durable writes.
   (make-entry "tracker-reconciliation.rkt"
               'external-ports
               '(fs-read parameterize)
               '()
               '("racket/file" "racket/list"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-handoff"
                               "delivery-journal"
                               "effect-ports"))
   ;; v1.00.33 W1: opt-in settings composition for the in-repo Racket adapter.
   (make-entry
    "tracker-production-wiring.rkt"
    'external-ports
    '()
    '()
    '("../../runtime/settings" "gh-cli-tracker-adapter" "github-port" "tracker-reconciliation"))
   ;; v1.00.22 W7 (BUG-0042): extracted from go-orchestrator verbatim
   (make-entry "attempt-artifacts.rkt"
               'persistence
               '(path-ops)
               '()
               '("racket/string" "campaign-state"
                                 "campaign-repository"
                                 "wave-executor"
                                 "delivery-verifier"
                                 "stall-policy"))
   (make-entry "campaign-budgets.rkt"
               'campaign-state
               '(make-param)
               '("current-campaign-usage-observation")
               '("util/loop-result" "campaign-state"
                                    "campaign-repository"
                                    "notify"
                                    "runtime/settings"
                                    "runtime/settings-query"))
   (make-entry "wave-completion.rkt"
               'campaign-state
               ;; W3 review fix: the completion outbox now publishes through the
               ;; shared projection atomic writer (unique temp + rename + owned
               ;; cleanup), so this module no longer carries its own
               ;; rename/mkdir markers; those effects live in
               ;; projection-effects.rkt (a declared dependency).
               '(fs-read fs-write fs-delete)
               '()
               '("racket/file" "racket/path"
                               "racket/format"
                               "racket/match"
                               "racket/port"
                               "racket/string"
                               "campaign-state"
                               "wave-docs"
                               "wave-status"
                               "planning-paths"
                               "projection-effects"))
   (make-entry "delivery-verifier.rkt"
               'campaign-state
               '(git subprocess fs-read make-param path-ops parameterize)
               '(current-gsd-delivery-verify-command current-gsd-delivery-verify-timeout-sec
                                                     current-owned-verify-observer)
               '("racket/format" "racket/path"
                                 "racket/port"
                                 "racket/set"
                                 "racket/string"
                                 "racket/system"
                                 "composition-root"
                                 "events"
                                 "plan-types"
                                 "verification-job"
                                 "verify-command-format"
                                 "supplemental-evidence-proof"
                                 "review-proof-format"
                                 "git-root-layout"
                                 "wave-docs"
                                 "planning-paths"
                                 "plan-context-builder"
                                 "sandbox/subprocess"))
   ;; BUG-0040 W6: terminal-transition notification sink adapter
   ;; (tmux/bell default + opt-in desktop-command/webhook sinks).
   (make-entry "notify.rkt"
               'external-ports
               '(subprocess parameterize make-param)
               '(current-gsd-notify-sinks)
               '("racket/contract" "racket/format"
                                   "json"
                                   "racket/match"
                                   "racket/string"
                                   "racket/system"
                                   "settings-query"
                                   "events"))
   ;; transition logic (7)
   (make-entry "policy.rkt"
               'transition-logic
               '(path-ops make-param)
               '()
               '("racket/match" "racket/string" "racket/path"))
   (make-entry "transition-kernel.rkt" 'transition-logic '() '() '("racket/match" "racket/set"))
   (make-entry "projection-kernel.rkt"
               'event-projection
               '()
               '()
               '("racket/base" "racket/string" "wave-status"))
   ;; W4: temp names are unique/self-identifying and the shell deletes ONLY its
   ;; own temp paths on failure, so fs-delete is a real effect of this module;
   ;; path construction is build-path/format only (no path<->string marker).
   (make-entry "projection-effects.rkt"
               'event-projection
               '(fs-read fs-write fs-rename fs-delete mkdir)
               '()
               '("racket/file" "racket/path"
                               "racket/format"
                               "racket/string"
                               "racket/port"
                               "projection-kernel"
                               "planning-paths"))
   (make-entry "transition-logic.rkt"
               'transition-logic
               '()
               '()
               '("racket/match" "racket/set" "runtime-state-types" "transition-kernel"))
   (make-entry "state-machine.rkt"
               'transition-logic
               '(make-param)
               '()
               '("racket/contract" "racket/set"
                                   "runtime-state-types"
                                   "transition-logic"
                                   "policy"
                                   "events"
                                   "event-structs"))
   (make-entry "wave-executor.rkt"
               'campaign-state
               ;; W8/v1.00.17: worktree isolation (W1-W5) added git + fs-delete
               ;; + parameter/make-parameter + path ops to this module.
               '(fs-delete git make-param parameterize path-ops)
               '()
               '("racket/format" "racket/string"
                                 "racket/file"
                                 "racket/port"
                                 "plan-types"
                                 "wave-docs"
                                 "planning-artifact-validation"
                                 "state-machine"
                                 "campaign-state"))
   ;; UI/extension glue (3)
   (make-entry "core.rkt"
               'ui-glue
               '(fs-read fs-write make-param parameterize path-ops)
               '()
               '("racket/contract" "racket/string"
                                   "racket/format"
                                   "racket/path"
                                   "racket/port"
                                   "state-machine"
                                   "plan-types"
                                   "context-bundle"
                                   "archive"
                                   "command-types"
                                   "wave-docs"
                                   "planning-paths"
                                   "session-state"
                                   "runtime-state-types"
                                   "events"
                                   "event-structs"
                                   "policy"))
   (make-entry "tool-handlers.rkt"
               'ui-glue
               '(fs-read fs-write mkdir path-ops)
               '()
               '("racket/contract" "racket/match"
                                   "racket/port"
                                   "racket/string"
                                   "racket/file"
                                   "racket/path"
                                   "json"
                                   "define-extension"
                                   "context"
                                   "tool-api"
                                   "state-machine"
                                   "session-state"
                                   "gsd-planning/command-normalization"
                                   "core"
                                   "policy"
                                   "events"))
   (make-entry "command-handlers.rkt"
               'ui-glue
               ;; W8/v1.00.17: empty-response re-anchor (#9514) added parameterize.
               ;; BUG-0032 fix: waves-dir backup rotation added fs-rename + path-ops.
               ;; W4: supersession keeps ONE owned, PROVABLY created backup and
               ;; preserves unknown backups; the pre-plan rotation stays an
               ;; fs-delete/mkdir effect owned here, and the ownership marker is
               ;; written with call-with-output-file (fs-write).
               '(fs-delete fs-write mkdir parameterize fs-rename path-ops)
               '()
               '("racket/contract" "racket/match"
                                   "racket/string"
                                   "racket/format"
                                   "racket/set"
                                   "racket/os"
                                   "racket/path"
                                   "json"
                                   "define-extension"
                                   "ext-commands"
                                   "hooks"
                                   "tool-api"
                                   "gsd-planning/command-normalization"
                                   "command-parser"
                                   "gsd-planning/plan-diff"
                                   "state-machine"
                                   "core"
                                   "plan-types"
                                   "plan-validator"
                                   "wave-executor"
                                   "prompts"
                                   "context-bundle"
                                   "wave-docs"
                                   "planning-paths"
                                   "planning-artifact-ownership"
                                   "tools/scratch"
                                   "tool-handlers"
                                   "archive"
                                   "events"
                                   "event-structs"
                                   "session-state"
                                   "plan-context-builder"
                                   "agent/verification/verifier-gate"
                                   "agent/verification/verifier-core"
                                   "racket/file"
                                   "campaign-state"
                                   "campaign-repository"
                                   "go-orchestrator"
                                   "delivery-verifier"
                                   "delivery-coordinator"
                                   "delivery-handoff"
                                   "delivery-journal"
                                   "runtime/settings-core"
                                   "policy"
                                   "review-session"
                                   "agent/state"
                                   "supplemental-author-contract"))
   ;; persistence (2)
   (make-entry "archive.rkt"
               'persistence
               '(fs-read fs-write fs-rename fs-delete mkdir dir-list path-ops)
               '()
               '("racket/contract" "racket/file"
                                   "racket/path"
                                   "racket/port"
                                   "racket/set"
                                   "racket/string"
                                   "racket/format"
                                   "plan-types"
                                   "wave-docs"
                                   "state-machine"
                                   "command-types"
                                   "shared"
                                   "wave-status"
                                   "planning-paths"
                                   "planning-artifact-ownership"
                                   "session-state"))
   (make-entry "wave-docs.rkt"
               'persistence
               '(fs-read fs-write mkdir dir-list path-ops)
               '()
               '("racket/contract" "racket/file"
                                   "racket/format"
                                   "racket/list"
                                   "racket/path"
                                   "racket/port"
                                   "racket/string"
                                   "util/version"
                                   "shared"
                                   "planning-paths"
                                   "wave-status"))
   ;; BUG-0060: bounded same-wave verification repair policy + durable
   ;; retry transition (classification, no-progress guard, ledger/persist).
   ;; W5: also owns the bounded zero-EDIT retry transition (authorized by the
   ;; typed 'no-change verdict); it reads the shared attempt-identity predicate
   ;; from delivery-receipt.
   (make-entry "verification-repair.rkt"
               'transition-logic
               '(parameterize)
               '()
               '("racket/match" "attempt-artifacts"
                                "campaign-repository"
                                "campaign-state"
                                "delivery-receipt"
                                "delivery-verifier"
                                "events"
                                "policy"
                                "prompts"
                                "wave-executor"))
   ;; BUG-0042 size discipline: terminal runner outcome reporting extracted
   ;; from go-orchestrator (typed error-surface event + honest reason).
   (make-entry "wave-outcome-reporting.rkt"
               'event-projection
               '()
               '()
               '("racket/string" "events" "wave-runner-port"))
   ;; event projection (2)
   (make-entry
    "events.rkt"
    'event-projection
    '(make-param)
    '("current-gsd-correlation-id")
    '("racket/match" "util/error" "agent/event-structs/base" "agent/event-emitter" "session-state"))
   (make-entry "event-structs.rkt" 'event-projection '() '() '("util/event/event-macro"))
   ;; v0.99.90 W0 external-domain ports (3) + W3 executor port (1) + W4 github port (1)
   (make-entry "effect-ports.rkt" 'external-ports '() '() '("racket/contract"))
   (make-entry "wave-runner-port.rkt" 'external-ports '() '() '("racket/contract"))
   ;; v1.00.24 W3 (BUG-0052): immutable plan snapshots
   (make-entry "plan-snapshot.rkt"
               'persistence
               '(fs-read fs-write fs-rename fs-delete mkdir dir-list sha256 path-ops)
               '()
               '("racket/file" "racket/format"
                               "racket/list"
                               "racket/match"
                               "racket/os"
                               "racket/path"
                               "racket/port"
                               "racket/string"
                               "planning-artifact-ownership"
                               "planning-paths"
                               "../../util/json/checksum"))
   ;; W4 (GSD-SIMPLIFICATION-PLAN): the ONE authentic ownership-marker
   ;; predicate. A pure READ shell by contract: it opens no output port, renames
   ;; nothing and deletes nothing — every destructive consumer asks it first.
   (make-entry "planning-artifact-ownership.rkt"
               'persistence
               '(fs-read)
               '()
               '("racket/file" "racket/path"))
   ;; v1.00.24 W3 (BUG-0053): owned singleton verification jobs
   (make-entry "verification-job.rkt"
               'external-ports
               '(fs-read fs-write fs-delete dir-list sha256 subprocess parameterize path-ops)
               '()
               '("racket/contract" "racket/file"
                                   "racket/format"
                                   "racket/match"
                                   "racket/path"
                                   "racket/string"
                                   "racket/system"
                                   "../../util/json/checksum"))
   (make-entry "github-port.rkt"
               'external-ports
               '()
               '()
               '("racket/contract" "racket/string" "effect-ports"))
   ;; W0 canary (cd3814e7): the board mutation refuses an operator-supplied
   ;; option-id that is not a member of the bound Status field's option
   ;; list. The option list is read by the same authenticated GraphQL
   ;; readback that already gates the mutation, so the refusal happens
   ;; before `gh project item-edit` — no new effect class is introduced.
   (make-entry "gh-cli-tracker-adapter.rkt"
               'external-ports
               '(subprocess)
               '()
               '("json" "racket/string" "github-port" "../../sandbox/subprocess"))
   (make-entry
    "system-adapters.rkt"
    'external-ports
    '(fs-read fs-write fs-rename fs-delete mkdir dir-list git parameterize make-param subprocess)
    '()
    '("racket/file" "racket/list"
                    "racket/path"
                    "racket/port"
                    "racket/string"
                    "racket/system"
                    "effect-ports"
                    "wave-runner-port"
                    "sandbox/gateway-bridge"))
   (make-entry "composition-root.rkt"
               'external-ports
               '(make-param)
               '(current-gsd-verification-registry)
               '("racket/contract" "effect-ports" "github-port" "system-adapters" "verification-job"))
   ;; BUG-0088 governed supplemental evidence: pure contract lenses and the
   ;; strict five-stage C0->C1 focus-only superset validator.
   (make-entry "effective-verify-binding.rkt"
               'pure-planning
               '()
               '()
               '("racket/list" "racket/string" "review-proof-format"))
   ;; Canonical sealed supplemental records. Pure digest/shape validation only.
   (make-entry "supplemental-evidence-format.rkt"
               'pure-planning
               '()
               '()
               '("racket/list" "racket/string" "evidence-repair-format" "effective-verify-binding"))
   ;; Private immutable proposal/approval retention. The broker grant is an
   ;; in-process capability; retained bytes live outside the project root.
   (make-entry "supplemental-evidence-authority.rkt"
               'persistence
               '(fs-write fs-read fs-rename fs-delete mkdir dir-list sha256 make-param path-ops)
               '(current-supplemental-authority-root)
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/port"
                        "racket/runtime-path"
                        "racket/string"
                        "../../runtime/approval/broker"
                        "../../util/json/checksum"
                        "evidence-repair-format"
                        "../gsd-planning/execution-policy"))
   ;; Fixed amendment identity. The private fixture parameter carries trusted
   ;; test data only, under an isolated private authority root, never a grant.
   (make-entry
    "supplemental-disposition-policy.rkt"
    'transition-logic
    '(make-param parameterize)
    '(fixture-amendment)
    '("racket/file" "racket/path" "supplemental-evidence-format" "supplemental-evidence-authority"))
   ;; Canonical private filesystem primitives and fsync FFI extracted from journal.
   (make-entry "supplemental-evidence-store.rkt"
               'persistence
               '(fs-write fs-read fs-rename fs-delete dir-list sha256 path-ops)
               '()
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/runtime-path"
                        "ffi/unsafe"
                        "ffi/unsafe/port"
                        "../../util/json/checksum"
                        "evidence-repair-format"
                        "supplemental-evidence-authority"))
   ;; BUG-0090: storage/validation core moved verbatim from the journal so the
   ;; module graph stays journal -> claim -> core, a static acyclic DAG with no
   ;; lazy first-use dynamic-require. Refusals keep the journal's source symbol
   ;; (like supplemental-evidence-store.rkt); the crash-cut parameter moved here.
   (make-entry "supplemental-evidence-store-core.rkt"
               'persistence
               '(fs-read mkdir make-param path-ops)
               '(current-supplemental-journal-crash-cut)
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "supplemental-evidence-store"
                        "supplemental-evidence-format"
                        "supplemental-evidence-authority"
                        "../gsd-planning/execution-policy"))
   ;; Known-path journal helper; requires the shared store core instead of the
   ;; journal back-edge, and re-provides its immutable inspection surface.
   (make-entry "supplemental-disposition-claim.rkt"
               'persistence
               '(sha256 path-ops)
               '()
               '("racket/file" "racket/path"
                               "supplemental-evidence-store"
                               "supplemental-evidence-format"
                               "supplemental-evidence-authority"
                               "supplemental-disposition-policy"
                               "supplemental-preserved-candidate"
                               "../../util/json/checksum"
                               "evidence-repair-format"
                               "supplemental-evidence-store-core"))
   ;; Candidate-preserving metadata settlement validator leaf. Static policy/
   ;; format/authority imports are pure/known-source; the fixed-a3 claim owner
   ;; and the strict retained-receipt loader are reached only through their
   ;; existing known-source lazy loads (cycle-breaking, never caller auth).
   (make-entry "supplemental-preserved-candidate.rkt"
               'transition-logic
               '(git parameterize path-ops sha256 dynamic-require)
               '()
               '("racket/list" "racket/path"
                               "racket/runtime-path"
                               "racket/string"
                               "racket/system"
                               "supplemental-evidence-format"
                               "supplemental-disposition-policy"
                               "supplemental-evidence-authority"
                               "../../util/json/checksum"
                               "supplemental-disposition-claim"
                               "evidence-readiness"))
   (make-entry "supplemental-evidence-disposition.rkt"
               'campaign-state
               '(git parameterize sha256)
               '()
               '("racket/list" "campaign-lease"
                               "racket/string"
                               "racket/system"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "evidence-repair-format"
                               "../../util/json/checksum"
                               "delivery-receipt"
                               "review-proof"
                               "evidence-readiness"
                               "supplemental-evidence-authority"
                               "supplemental-evidence-format"
                               "supplemental-evidence-journal"
                               "supplemental-disposition-policy"
                               "supplemental-evidence-controller"
                               "../gsd-planning/execution-policy"))
   ;; Protected one-shot operation journal: OS-locked CAS + write-ahead intents.
   ;; BUG-0090: storage/validation effects (fs-read/mkdir/make-param) and the
   ;; crash-cut parameter moved to supplemental-evidence-store-core; the lazy
   ;; dynamic-require seam is gone (static journal -> claim -> core requires).
   (make-entry "supplemental-evidence-journal.rkt"
               'persistence
               '(fs-write fs-delete dir-list path-ops)
               '()
               '("racket/file" "racket/path"
                               "supplemental-evidence-store"
                               "supplemental-evidence-store-core"
                               "supplemental-disposition-claim"
                               "supplemental-evidence-format"
                               "supplemental-evidence-authority"))
   ;; Effective C1 owned-Verify capture and private authority reload. Git,
   ;; verification capture and proof retention delegate to their owners.
   (make-entry "supplemental-evidence-proof.rkt"
               'persistence
               '(sha256 git subprocess path-ops dynamic-require)
               '()
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/runtime-path"
                               "racket/string"
                               "review-proof-format"
                               "supplemental-evidence-format"
                               "supplemental-evidence-authority"
                               "supplemental-evidence-journal"
                               "campaign-state"
                               "campaign-repository"
                               "evidence-repair-format"
                               "sandbox/subprocess"
                               "../../util/json/checksum"))
   ;; Staged wrappers, guarded supersession publication and replay. Effect
   ;; bodies delegate to delivery-publication/authority/journal owners.
   (make-entry "supplemental-evidence-publication.rkt"
               'persistence
               '(fs-write fs-read fs-rename fs-delete sha256 dynamic-require path-ops)
               '()
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/runtime-path"
                        "racket/string"
                        "../../util/json/checksum"
                        "review-proof-format"
                        "evidence-repair-format"
                        "supplemental-evidence-format"
                        "supplemental-evidence-authority"
                        "supplemental-evidence-journal"
                        "supplemental-evidence-proof"
                        "delivery-journal"
                        "delivery-publication"))
   ;; Immutable promotion record P, protected capability minting and idempotent
   ;; reconciliation. Provenance bytes delegate to proof/journal owners.
   (make-entry "supplemental-evidence-promotion.rkt"
               'persistence
               '(sha256)
               '()
               '("racket/file" "racket/list"
                               "racket/path"
                               "racket/runtime-path"
                               "racket/string"
                               "../../util/json/checksum"
                               "evidence-repair-format"
                               "supplemental-evidence-format"
                               "supplemental-evidence-authority"
                               "supplemental-evidence-journal"
                               "supplemental-evidence-proof"
                               "evidence-probe"
                               "evidence-repair-ledger"
                               "review-proof-format"))
   ;; Owned controller: typed admission seams, allocation and advance loop.
   ;; Model dispatch, commits and Verify delegate to the existing owned lanes.
   (make-entry
    "supplemental-evidence-controller.rkt"
    'campaign-state
    '(fs-write fs-read fs-rename fs-delete mkdir sha256 git parameterize path-ops dynamic-require)
    '()
    '("json" "racket/file"
             "racket/list"
             "racket/match"
             "racket/path"
             "racket/port"
             "racket/runtime-path"
             "racket/string"
             "racket/system"
             "campaign-state"
             "campaign-result"
             "campaign-repository"
             "delivery-journal"
             "delivery-receipt"
             "delivery-verifier"
             "delivery-publication"
             "review-proof"
             "review-proof-format"
             "apply-verify-jobs"
             "evidence-readiness"
             "evidence-discovery"
             "evidence-probe"
             "evidence-repair-format"
             "supplemental-evidence-format"
             "supplemental-evidence-authority"
             "supplemental-evidence-journal"
             "supplemental-evidence-operator"
             "supplemental-evidence-proof"
             "supplemental-evidence-publication"
             "author-execution"
             "composition-root"
             "verification-job"
             "wave-executor"
             "owned-lane"
             "wave-scope"
             "author-checkpoint"
             "../gsd-planning/execution-policy"
             "../../util/json/checksum"))
   ;; Explicit single-child recovery: owner lease, immutable parent inspection,
   ;; permanent private claim and exact pointer CAS; never a model retry.
   (make-entry "supplemental-evidence-recovery.rkt"
               'campaign-state
               '(git parameterize dynamic-require)
               '()
               '("racket/list" "racket/path"
                               "racket/runtime-path"
                               "racket/string"
                               "racket/system"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "delivery-receipt"
                               "review-proof"
                               "evidence-readiness"
                               "supplemental-evidence-authority"
                               "supplemental-evidence-format"
                               "supplemental-evidence-journal"
                               "supplemental-evidence-controller"
                               "../gsd-planning/execution-policy"))
   ;; Trusted-frontend approval requests: broker registration and owner UI
   ;; event emission. No grant is ever created here.
   (make-entry "supplemental-evidence-operator.rkt"
               'ui-glue
               '(sha256 make-param)
               '(current-supplemental-operator-publisher)
               '("racket/file" "racket/string"
                               "../../util/credential-redaction"
                               "../../util/json/checksum"
                               "../../runtime/approval/broker"
                               "evidence-repair-format"
                               "supplemental-evidence-authority"))
   ;; Hardened reviewer tool registry: bounded direct-argv read-only commands.
   (make-entry "review-tools.rkt"
               'external-ports
               '(git subprocess parameterize path-ops)
               '()
               '("racket/file" "racket/list"
                               "racket/match"
                               "racket/path"
                               "racket/string"
                               "../../tools/tool"
                               "../../tools/registry-defaults"
                               "../../sandbox/subprocess"
                               "../../sandbox/limits"))))

(provide inventory
         (struct-out entry)
         entry-domain
         entry-effects
         entry-params
         entry-deps)
