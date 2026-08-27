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
   ;; pure planning (8)
   (make-entry "shared.rkt" 'pure-planning '() '() '("racket/string" "racket/list"))
   (make-entry "wave-status.rkt" 'pure-planning '() '() '())
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
   (make-entry
    "plan-context-builder.rkt"
    'pure-planning
    '(make-param path-ops fs-read)
    '()
    '("racket/string" "racket/port" "racket/list" "plan-types" "effect-ports" "composition-root"))
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
               '(fs-read fs-write fs-rename fs-delete mkdir)
               '()
               '("racket/file" "racket/path"
                               "racket/match"
                               "racket/format"
                               "racket/string"
                               "racket/contract"
                               "campaign-state"))
   (make-entry "go-orchestrator.rkt"
               'campaign-state
               '(git make-param mkdir path-ops parameterize fs-read)
               '()
               '("racket/format" "racket/file"
                                 "racket/match"
                                 "campaign-state"
                                 "campaign-repository"
                                 "wave-completion"
                                 "wave-runner-port"
                                 "wave-docs"
                                 "wave-status"
                                 "projection-effects"
                                 "delivery-verifier"
                                 "util/loop-result"
                                 "system-adapters"
                                 "sandbox/gateway-bridge"
                                 "plan-context-builder"))
   (make-entry "wave-completion.rkt"
               'campaign-state
               '(fs-read fs-write fs-rename mkdir)
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
               '(current-gsd-delivery-verify-command current-gsd-delivery-verify-timeout-sec)
               '("racket/format" "racket/path"
                                 "racket/port"
                                 "racket/set"
                                 "racket/string"
                                 "racket/system"
                                 "plan-types"
                                 "wave-docs"
                                 "plan-context-builder"))
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
                                   "go-orchestrator"))
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
   (make-entry "github-port.rkt"
               'external-ports
               '()
               '()
               '("racket/contract" "racket/string" "effect-ports"))
   (make-entry "system-adapters.rkt"
               'external-ports
               '(fs-read fs-write fs-rename fs-delete mkdir dir-list git parameterize subprocess)
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
               '()
               '("racket/contract" "effect-ports" "github-port" "system-adapters"))))

(provide inventory
         (struct-out entry)
         entry-domain
         entry-effects
         entry-params
         entry-deps)
