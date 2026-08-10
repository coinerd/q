#lang racket
;; GSD Responsibility & Effect Inventory — machine-readable evidence (v0.99.87 W2)
;; Issue #9214. Consumed by tests/test-gsd-responsibility-inventory.rkt.
;;
;; Each module entry: (entry module-file domain effects params deps)
;; domain ∈ (pure-planning campaign-state transition-logic ui-glue persistence
;;            event-projection command-parsing compatibility-facade)
;; effects ∈ (fs-write fs-rename fs-delete mkdir dir-list sha256 git subprocess
;;            parameterize make-param path-ops network github dynamic-require)

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
   (make-entry "plan-context-builder.rkt"
               'pure-planning
               '(git subprocess parameterize make-param path-ops)
               '()
               '("racket/string" "racket/port" "racket/system" "racket/list" "plan-types"))
   ;; campaign state (5)
   (make-entry "runtime-state-types.rkt" 'campaign-state '() '() '("racket/set"))
   (make-entry "session-state.rkt"
               'campaign-state
               '(make-param)
               '("current-gsd-ctx" "current-gsd-session-id")
               '("racket/contract" "racket/set" "runtime-state-types"))
   (make-entry "campaign-state.rkt"
               'campaign-state
               '(fs-write fs-rename mkdir sha256)
               '()
               '("racket/file" "racket/string"
                               "racket/port"
                               "racket/path"
                               "racket/list"
                               "racket/match"
                               "racket/format"
                               "racket/contract"
                               "wave-docs"
                               "util/json/checksum"))
   (make-entry "go-orchestrator.rkt"
               'campaign-state
               '(mkdir path-ops)
               '()
               '("racket/format" "racket/file"
                                 "racket/match"
                                 "campaign-state"
                                 "wave-completion"
                                 "wave-docs"
                                 "wave-status"
                                 "util/loop-result"
                                 "sandbox/gateway-bridge"
                                 "plan-context-builder"))
   (make-entry "wave-completion.rkt"
               'campaign-state
               '(fs-write fs-rename mkdir)
               '()
               '("racket/file" "racket/path"
                               "racket/format"
                               "racket/match"
                               "racket/port"
                               "racket/string"
                               "campaign-state"
                               "wave-docs"
                               "wave-status"))
   ;; transition logic (5)
   (make-entry "policy.rkt" 'transition-logic '() '() '("racket/match" "racket/string" "racket/path"))
   (make-entry "transition-kernel.rkt" 'transition-logic '() '() '("racket/match" "racket/set"))
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
               'transition-logic
               '()
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
               '(fs-write parameterize path-ops)
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
               '(fs-write mkdir path-ops)
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
               '(fs-delete mkdir)
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
               '(fs-write fs-rename fs-delete mkdir dir-list path-ops)
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
               '(fs-write mkdir path-ops)
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
   (make-entry "event-structs.rkt" 'event-projection '() '() '("util/event/event-macro"))))

(provide inventory
         (struct-out entry)
         entry-domain
         entry-effects
         entry-params
         entry-deps)
