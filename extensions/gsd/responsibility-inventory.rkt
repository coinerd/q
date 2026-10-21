#lang racket
;; GSD Responsibility & Effect Inventory — machine-readable evidence (v0.99.87 W2)
;; Issue #9214. Consumed by tests/test-gsd-responsibility-inventory.rkt.
;;
;; Each module entry: (entry module-file domain effects params deps)
;; domain ∈ (pure-planning campaign-state transition-logic ui-glue persistence
;;            event-projection command-parsing compatibility-facade)
;; effects ∈ (fs-write fs-read fs-rename fs-delete mkdir dir-list sha256 git
;;            subprocess parameterize make-param path-ops network github
;;            dynamic-require)

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
   (make-entry "wave-status.rkt" 'pure-planning '() '() '())
   ;; BUG-0077: the campaign terminal result struct, extracted from
   ;; go-orchestrator so the delivery checkpoint modules can construct
   ;; results without depending on the orchestrator (size pin).
   (make-entry "campaign-result.rkt" 'pure-planning '() '() '())
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
   (make-entry "prompts.rkt"
               'pure-planning
               '()
               '()
               '("racket/format" "racket/string" "plan-types" "wave-executor"))
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
                               "plan-snapshot"))
   (make-entry "go-orchestrator.rkt"
               'campaign-state
               '(make-param mkdir parameterize)
               '()
               '("racket/format" "racket/file"
                                 "racket/match"
                                 "campaign-state"
                                 "campaign-repository"
                                 "stall-policy"
                                 "infra-retry-policy"
                                 "freshness"
                                 "attempt-artifacts"
                                 "campaign-budgets"
                                 "wave-completion"
                                 "wave-runner-port"
                                 "wave-docs"
                                 "wave-status"
                                 "projection-effects"
                                 "delivery-verifier"
                                 "delivery-handoff"
                                 "tracker-resume"
                                 "util/loop-result"
                                 "system-adapters"
                                 "sandbox/gateway-bridge"
                                 "plan-context-builder"
                                 "review-dispatch"
                                 "review-worktree"))
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
             "delivery-verifier"
             "verification-job"
             "plan-snapshot"
             "plan-types"
             "wave-docs"
             "util/json/checksum"))
   (make-entry "review-proof-format.rkt"
               'transition-logic
               '(sha256)
               '()
               '("racket/list" "racket/string" "util/json/checksum"))
   (make-entry "verify-command-format.rkt" 'transition-logic '() '() '("racket/list" "racket/string"))
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
                        "review-worktree"))
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
                                 "sandbox/subprocess"))
   ;; v1.00.33 recovery continuation: supported same-attempt repair-tail
   ;; recovery entry. Strict precheck + journal restore + independent clone
   ;; candidate + production verify; effects are delegated to the trusted
   ;; modules (delivery-journal reconcile, attempt-artifacts provenance,
   ;; delivery-handoff persist) — the module's own markers are the git
   ;; subprocess resolver, path operations and the origin-policy parameter.
   (make-entry "delivery-recovery.rkt"
               'campaign-state
               '(git path-ops parameterize make-param fs-read fs-rename fs-delete mkdir dir-list)
               '()
               '("racket/path" "racket/string"
                               "racket/system"
                               "json"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "delivery-verifier"
                               "delivery-handoff"
                               "attempt-artifacts"))
   ;; BUG-0077: honest-completion state machine extracted from
   ;; go-orchestrator (attempt delivery provenance, park/finalize
   ;; checkpoint drivers, coordinator checkpoint decision). All durable
   ;; effects are delegated to the required modules (campaign-repository,
   ;; delivery-journal, wave-completion, attempt-artifacts); this module's
   ;; own source carries no marker-visible effects.
   ;; BUG-0084: declares the current-gsd-review-infra-retry-limit parameter
   ;; (audited in docs/architecture/parameter-inventory.rktd).
   (make-entry "delivery-finalize.rkt"
               'campaign-state
               '(make-param)
               '()
               '("racket/list" "racket/string"
                               "campaign-state"
                               "campaign-repository"
                               "delivery-journal"
                               "wave-completion"
                               "attempt-artifacts"
                               "delivery-verifier"
                               "delivery-coordinator"
                               "campaign-result"
                               "reconciliation-checkpoint"
                               "tracker-production-wiring"
                               "review-checkpoint"
                               "author-checkpoint"
                               "author-session-recovery"))
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
               '(fs-read fs-write fs-rename mkdir fs-delete)
               '()
               '("racket/file" "racket/path"
                               "racket/format"
                               "racket/match"
                               "racket/port"
                               "racket/string"
                               "campaign-state"
                               "wave-docs"
                               "wave-status"
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
                                 "wave-docs"
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
   (make-entry "projection-kernel.rkt" 'event-projection '() '() '("racket/base" "racket/string"))
   (make-entry
    "projection-effects.rkt"
    'event-projection
    '(fs-read fs-write fs-rename mkdir path-ops)
    '()
    '("racket/file" "racket/path" "racket/format" "racket/string" "racket/port" "projection-kernel"))
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
               '(fs-read fs-delete git make-param parameterize path-ops)
               '()
               '("racket/format" "racket/string"
                                 "racket/file"
                                 "racket/port"
                                 "plan-types"
                                 "wave-docs"
                                 "shared"
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
               '(fs-delete mkdir parameterize fs-rename path-ops)
               '()
               '("racket/contract" "racket/match"
                                   "racket/string"
                                   "racket/set"
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
                                   "tool-handlers"
                                   "archive"
                                   "events"
                                   "event-structs"
                                   "session-state"
                                   "plan-context-builder"
                                   "verifier-gate"
                                   "verifier-core"
                                   "racket/file"
                                   "campaign-state"
                                   "go-orchestrator"
                                   "review-session"))
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
                                   "session-state"))
   (make-entry "wave-docs.rkt"
               'persistence
               '(fs-read fs-write mkdir dir-list path-ops)
               '()
               '("racket/file" "racket/path"
                               "racket/format"
                               "racket/string"
                               "racket/match"
                               "shared"
                               "wave-status"))
   ;; BUG-0060: bounded same-wave verification repair policy + durable
   ;; retry transition (classification, no-progress guard, ledger/persist).
   (make-entry "verification-repair.rkt"
               'transition-logic
               '(parameterize)
               '()
               '("racket/match" "attempt-artifacts"
                                "campaign-repository"
                                "campaign-state"
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
                               "racket/path"
                               "racket/port"
                               "racket/string"
                               "../../util/json/checksum"))
   ;; v1.00.24 W3 (BUG-0053): owned singleton verification jobs
   (make-entry "verification-job.rkt"
               'external-ports
               '(fs-read fs-delete dir-list sha256 subprocess parameterize path-ops)
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
   ;; Protected one-shot operation journal: OS-locked CAS + write-ahead intents.
   (make-entry "supplemental-evidence-journal.rkt"
               'persistence
               '(fs-write fs-read fs-rename fs-delete mkdir sha256 make-param path-ops)
               '(current-supplemental-journal-crash-cut)
               '("json" "racket/file"
                        "racket/list"
                        "racket/path"
                        "racket/port"
                        "racket/runtime-path"
                        "../../util/json/checksum"
                        "evidence-repair-format"
                        "supplemental-evidence-format"
                        "supplemental-evidence-authority"
                        "../gsd-planning/execution-policy"))
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
