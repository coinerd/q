#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w6-session.rkt — Session subsystem real-world audit
;;
;; Audit of the Session subsystem covering:
;;   1. Lifecycle state struct (defaults, mutations)
;;   2. Session types (agent-session struct, facets, accessors)
;;   3. Session FSM (derived state computation, transitions)
;;   4. Session config (typed accessors, gen:dict, defaults)
;;   5. Session controls (model switching, thinking levels, shutdown)
;;   6. Session mutation guards (invariant enforcement)
;;   7. Session lifecycle transitions (pure message builders)
;;   8. Session store integrity (hash chain, write-ahead markers)
;;   9. Session migration (versioning, migration chain)
;;
;; All tests use synthetic data — no real API keys or filesystem needed.

(require rackunit
         racket/file
         racket/list
         racket/string
         racket/dict
         json
         ;; Lifecycle state
         "../runtime/session/lifecycle-state.rkt"
         ;; Session types
         "../runtime/session/session-types.rkt"
         ;; Session FSM
         "../runtime/session/session-fsm.rkt"
         ;; FSM types
         (only-in "../util/fsm/fsm.rkt" fsm-state? fsm-state fsm-state-name)
         ;; Session config
         "../runtime/session/session-config.rkt"
         ;; Session controls
         "../runtime/session/session-controls.rkt"
         ;; Session mutation guards
         "../runtime/session/session-mutation.rkt"
         ;; Session lifecycle transitions (pure)
         "../runtime/session/session-lifecycle-transitions.rkt"
         ;; Session store integrity
         "../runtime/session/session-store-integrity.rkt"
         ;; Session context
         "../runtime/session/session-context.rkt"
         ;; Session metadata
         "../runtime/session/session-metadata.rkt"
         ;; Message helpers for creating test entries
         (only-in "../util/message/message.rkt"
                  make-message
                  message?
                  message-id
                  message-kind
                  message-parent-id
                  message-meta-safe
                  message-content
                  message-role
                  message->jsexpr)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../util/ids.rkt" generate-id))

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(define (make-test-lifecycle-state)
  (make-lifecycle-state))

(define (make-test-session #:active? [active? #f])
  ;; Build a minimal agent-session with synthetic data
  (agent-session "test-session-id"
                 (string->path "/tmp/test-session")
                 #f ; provider
                 #f ; tool-registry
                 #f ; event-bus
                 #f ; extension-registry
                 "test-model" ; model-name
                 '("test instructions") ; system-instructions
                 #f ; index
                 #f ; queue
                 (hash) ; config
                 active? ; active?
                 (current-seconds) ; start-time
                 '() ; pending-entries
                 'medium ; thinking-level
                 (make-test-lifecycle-state))) ; lifecycle

(define (make-test-config [overrides '()])
  (define base
    '((provider . #f) (model-name . "claude-test")
                      (max-iterations . 50)
                      (max-context-tokens . 128000)
                      (thinking-level . medium)
                      (max-tokens . 8192)
                      (system-instructions . ())
                      (verbose? . #f)
                      (tier-b-count . 20)
                      (tier-c-count . 4)))
  (define merged
    (for/fold ([h (hash)]) ([pair (in-list base)])
      (hash-set h (car pair) (cdr pair))))
  (for/fold ([h merged]) ([pair (in-list overrides)])
    (hash-set h (car pair) (cdr pair))))

;; ---------------------------------------------------------------------------
;; 1. Lifecycle State Struct
;; ---------------------------------------------------------------------------

(test-case "audit-ls-defaults"
  (define ls (make-lifecycle-state))
  (check-true (lifecycle-state? ls))
  (check-false (lifecycle-state-compacting? ls))
  (check-false (lifecycle-state-last-compaction-time ls))
  (check-false (lifecycle-state-persisted? ls))
  (check-false (lifecycle-state-shutdown-requested? ls))
  (check-false (lifecycle-state-force-shutdown? ls))
  (check-false (lifecycle-state-prompt-running? ls) "prompt-running? should default to #f"))

(test-case "audit-ls-default-task-fsm-state"
  (define ls (make-lifecycle-state))
  (check-equal? (lifecycle-state-task-fsm-state ls) 'idle))

(test-case "audit-ls-default-collections"
  (define ls (make-lifecycle-state))
  (check-equal? (lifecycle-state-task-conclusions ls) '())
  (check-equal? (lifecycle-state-recent-tool-calls ls) '()))

(test-case "audit-ls-mutations"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-compacting?! ls #t)
  (check-true (lifecycle-state-compacting? ls))
  (set-lifecycle-state-compacting?! ls #f)
  (check-false (lifecycle-state-compacting? ls))
  (set-lifecycle-state-prompt-running?! ls #t)
  (check-true (lifecycle-state-prompt-running? ls))
  (set-lifecycle-state-task-fsm-state! ls 'executing)
  (check-equal? (lifecycle-state-task-fsm-state ls) 'executing))

(test-case "audit-ls-transparent"
  (define ls (make-lifecycle-state))
  ;; lifecycle-state is transparent — verify printable
  (define s (format "~a" ls))
  (check-true (string? s))
  (check-true (string-contains? s "lifecycle-state")))

;; ---------------------------------------------------------------------------
;; 2. Session Types (agent-session struct)
;; ---------------------------------------------------------------------------

(test-case "audit-st-struct-fields"
  (define sess (make-test-session #:active? #t))
  (check-true (agent-session? sess))
  (check-equal? (agent-session-session-id sess) "test-session-id")
  (check-equal? (agent-session-model-name sess) "test-model")
  (check-equal? (agent-session-system-instructions sess) '("test instructions"))
  (check-true (agent-session-active? sess))
  (check-equal? (agent-session-thinking-level sess) 'medium))

(test-case "audit-st-lifecycle-accessors"
  (define sess (make-test-session))
  (check-false (agent-session-compacting? sess))
  (check-false (agent-session-prompt-running? sess))
  (check-false (agent-session-shutdown-requested? sess))
  (check-false (agent-session-persisted? sess) "New session should not be persisted by default")
  (check-equal? (agent-session-task-fsm-state sess) 'idle))

(test-case "audit-st-path-helpers"
  (define sess (make-test-session))
  (define log-path (session-log-path-for sess))
  (check-true (path? log-path))
  (check-true (string-contains? (path->string log-path) "session.jsonl")))

(test-case "audit-st-facets"
  (define sess (make-test-session))
  (define pf (session->provider-facet sess))
  (check-true (session-provider-facet? pf))
  (check-equal? (session-provider-facet-model-name pf) "test-model")
  (define tf (session->tool-facet sess))
  (check-true (session-tool-facet? tf))
  (define idf (session->identity-facet sess))
  (check-true (session-identity-facet? idf))
  (check-equal? (session-identity-facet-session-id idf) "test-session-id"))

;; ---------------------------------------------------------------------------
;; 3. Session FSM
;; ---------------------------------------------------------------------------

(test-case "audit-fsm-states-defined"
  (check-true (fsm-state? session-lifecycle-terminated))
  (check-true (fsm-state? session-lifecycle-active))
  (check-true (fsm-state? session-lifecycle-streaming))
  (check-true (fsm-state? session-lifecycle-compacting))
  (check-true (fsm-state? session-lifecycle-idle))
  (check-true (fsm-state? session-lifecycle-created)))

(test-case "audit-fsm-derived-state-active"
  (define sess (make-test-session #:active? #t))
  (check-equal? (session-current-state-name sess) 'active))

(test-case "audit-fsm-derived-state-streaming"
  (define sess (make-test-session #:active? #t))
  (set-lifecycle-state-prompt-running?! (agent-session-lifecycle sess) #t)
  (check-equal? (session-current-state-name sess) 'streaming))

(test-case "audit-fsm-derived-state-compacting"
  (define sess (make-test-session #:active? #t))
  (set-lifecycle-state-compacting?! (agent-session-lifecycle sess) #t)
  (check-equal? (session-current-state-name sess) 'compacting))

(test-case "audit-fsm-derived-state-terminated"
  (define sess (make-test-session #:active? #f))
  (check-equal? (session-current-state-name sess) 'terminated))

(test-case "audit-fsm-valid-lifecycle"
  (define sess (make-test-session #:active? #t))
  (check-true (session-valid-lifecycle? sess)))

(test-case "audit-fsm-can-transition"
  (define sess (make-test-session #:active? #t))
  (check-true (session-can-transition? sess 'stream-requested))
  (check-true (session-can-transition? sess 'turn-complete))
  (check-false (session-can-transition? sess 'new-turn) "Cannot accept new-turn from active state"))

;; ---------------------------------------------------------------------------
;; 4. Session Config
;; ---------------------------------------------------------------------------

(test-case "audit-sc-create"
  (define c (hash->session-config (make-test-config)))
  (check-true (session-config? c)))

(test-case "audit-sc-typed-accessors"
  (define c (hash->session-config (make-test-config)))
  (check-equal? (config-model-name c) "claude-test")
  (check-equal? (config-max-iterations c) 50)
  (check-equal? (config-max-context-tokens c) 128000)
  (check-equal? (config-thinking-level c) 'medium)
  (check-equal? (config-max-tokens c) 8192))

(test-case "audit-sc-defaults"
  (define c (hash->session-config (hash)))
  (check-false (config-model-name c))
  (check-equal? (config-max-iterations c) 50 "Default max-iterations")
  (check-equal? (config-max-context-tokens c) 128000 "Default max-context-tokens")
  (check-equal? (config-thinking-level c) 'medium "Default thinking-level")
  (check-equal? (config-max-tokens c) 8192 "Default max-tokens")
  (check-equal? (config-tier-b-count c) 20 "Default tier-b-count")
  (check-equal? (config-tier-c-count c) 4 "Default tier-c-count"))

(test-case "audit-sc-gen-dict"
  (define c (hash->session-config (hash 'model-name "test" 'verbose? #t)))
  (check-equal? (dict-ref c 'model-name) "test")
  (check-true (dict-has-key? c 'verbose?))
  (check-false (dict-has-key? c 'nonexistent))
  (define c2 (dict-set c 'new-key "val"))
  (check-equal? (dict-ref c2 'new-key) "val"))

(test-case "audit-sc-round-trip"
  (define h (make-test-config))
  (define c (hash->session-config h))
  (define h2 (session-config->hash c))
  (check-equal? (hash-ref h2 'model-name) "claude-test")
  (check-equal? (hash-ref h2 'max-iterations) 50))

(test-case "audit-sc-resolve-max-iterations-hard"
  (define c (hash->session-config (hash)))
  ;; When max-iterations-hard is not set, resolves to max(iter*8/5, 80)
  (define resolved (resolve-max-iterations-hard c 50))
  (check-true (exact-positive-integer? resolved))
  (check-true (>= resolved 80)))

(test-case "audit-sc-resolve-max-iterations-hard-explicit"
  (define c (hash->session-config (hash 'max-iterations-hard 200)))
  (define resolved (resolve-max-iterations-hard c 50))
  (check-equal? resolved 200 "Explicit value should be used"))

(test-case "audit-sc-thinking-level-string-coercion"
  ;; normalize-session-config-hash should coerce string thinking-level to symbol
  (define h (hash 'thinking-level "high"))
  (define normalized (normalize-session-config-hash h))
  (check-equal? (hash-ref normalized 'thinking-level) 'high))

(test-case "audit-sc-memory-defaults"
  (define c (hash->session-config (hash)))
  (check-false (config-memory-enabled? c) "Memory disabled by default")
  (check-false (config-memory-auto-extraction-enabled? c) "Auto-extraction off by default")
  (check-equal? (config-memory-auto-extraction-min-confidence c) 0.5))

(test-case "audit-sc-profile-predicate"
  (check-true (context-assembly-profile? 'off))
  (check-true (context-assembly-profile? 'full))
  (check-true (context-assembly-profile? 'bounded))
  (check-false (context-assembly-profile? 'bogus)))

;; ---------------------------------------------------------------------------
;; 5. Session Controls
;; ---------------------------------------------------------------------------

(test-case "audit-ctrl-thinking-levels"
  (check-equal? (length thinking-levels) 6)
  (check-not-false (member 'off thinking-levels) "off is a valid thinking level")
  (check-not-false (member 'xhigh thinking-levels) "xhigh is a valid thinking level")
  (check-true (thinking-level? 'medium))
  (check-false (thinking-level? 'bogus)))

(test-case "audit-ctrl-thinking-level-budget"
  (check-equal? (thinking-level->budget 'off) 0)
  (check-equal? (thinking-level->budget 'minimal) 1024)
  (check-equal? (thinking-level->budget 'low) 4096)
  (check-equal? (thinking-level->budget 'medium) 8192)
  (check-equal? (thinking-level->budget 'high) 16384)
  (check-equal? (thinking-level->budget 'xhigh) 32768))

(test-case "audit-ctrl-set-thinking-level"
  (define sess (make-test-session))
  (set-thinking-level! sess 'high)
  (check-equal? (agent-session-thinking-level sess) 'high))

(test-case "audit-ctrl-set-thinking-level-invalid"
  (define sess (make-test-session))
  (check-exn exn:fail? (lambda () (set-thinking-level! sess 'bogus))))

(test-case "audit-ctrl-set-model"
  (define sess (make-test-session))
  (set-model! sess "new-model")
  (check-equal? (agent-session-model-name sess) "new-model"))

(test-case "audit-ctrl-set-model-invalid"
  (define sess (make-test-session))
  (check-exn exn:fail? (lambda () (set-model! sess 42))))

(test-case "audit-ctrl-shutdown"
  (define sess (make-test-session))
  (check-false (shutdown-requested? sess))
  (request-shutdown! sess)
  (check-true (shutdown-requested? sess))
  (force-shutdown! sess)
  (check-true (force-shutdown-requested? sess))
  (reset-shutdown-flags! sess)
  (check-false (shutdown-requested? sess))
  (check-false (force-shutdown-requested? sess)))

;; ---------------------------------------------------------------------------
;; 6. Session Mutation Guards
;; ---------------------------------------------------------------------------

(test-case "audit-mut-guard-prompt-running-true-then-true"
  (define sess (make-test-session))
  (guarded-set-prompt-running! sess #t)
  (check-exn exn:fail?
             (lambda () (guarded-set-prompt-running! sess #t))
             "Should reject #t→#t invariant violation"))

(test-case "audit-mut-guard-prompt-running-true-then-false"
  (define sess (make-test-session))
  (guarded-set-prompt-running! sess #t)
  (guarded-set-prompt-running! sess #f)
  (check-false (agent-session-prompt-running? sess)))

(test-case "audit-mut-guard-compacting-true-then-true"
  (define sess (make-test-session))
  (guarded-set-compacting! sess #t)
  (check-exn exn:fail?
             (lambda () (guarded-set-compacting! sess #t))
             "Should reject #t→#t invariant violation"))

(test-case "audit-mut-guard-persisted-only-true"
  (define sess (make-test-session))
  (guarded-set-persisted! sess #t)
  (check-true (agent-session-persisted? sess))
  (check-exn exn:fail?
             (lambda () (guarded-set-persisted! sess #f))
             "Should reject #t→#f persisted transition"))

(test-case "audit-mut-guard-shutdown-idempotent"
  (define sess (make-test-session))
  (guarded-set-shutdown-requested! sess #t)
  (guarded-set-shutdown-requested! sess #t) ; should not raise
  (check-true (agent-session-shutdown-requested? sess)))

(test-case "audit-mut-guard-phase"
  (define sess (make-test-session))
  (check-equal? (session-phase sess) 'idle)
  (guarded-set-prompt-running! sess #t)
  (check-equal? (session-phase sess) 'running)
  (guarded-set-prompt-running! sess #f)
  (guarded-set-compacting! sess #t)
  (check-equal? (session-phase sess) 'compacting))

(test-case "audit-mut-guard-valid-phase"
  (check-true (valid-session-phase? 'idle))
  (check-true (valid-session-phase? 'running))
  (check-true (valid-session-phase? 'compacting))
  (check-true (valid-session-phase? 'shutting-down))
  (check-false (valid-session-phase? 'bogus)))

(test-case "audit-mut-guard-recent-tool-calls"
  (define sess (make-test-session))
  (guarded-set-recent-tool-calls! sess '(read write bash))
  (check-equal? (agent-session-recent-tool-calls sess) '(read write bash)))

(test-case "audit-mut-guard-recent-tool-calls-invalid"
  (define sess (make-test-session))
  (check-exn exn:fail? (lambda () (guarded-set-recent-tool-calls! sess "not-a-list"))))

;; ---------------------------------------------------------------------------
;; 7. Session Lifecycle Transitions (Pure)
;; ---------------------------------------------------------------------------

(test-case "audit-lt-build-user-message"
  (define msg (build-user-message "Hello world" #f))
  (check-true (message? msg))
  (check-equal? (message-role msg) 'user)
  (check-false (message-parent-id msg)))

(test-case "audit-lt-build-user-message-with-parent"
  (define msg (build-user-message "Reply" "parent-123"))
  (check-true (message? msg))
  (check-equal? (message-parent-id msg) "parent-123"))

(test-case "audit-lt-compute-parent-id-empty"
  (check-false (compute-parent-id '())))

(test-case "audit-lt-compute-parent-id-from-entries"
  (define m1 (make-message "msg1" #f 'user 'message (list (make-text-part "first")) 1000 (hasheq)))
  (define m2
    (make-message "msg2" "msg1" 'assistant 'message (list (make-text-part "second")) 1001 (hasheq)))
  (define entries (list m1 m2))
  (define parent (compute-parent-id entries))
  (check-equal? parent "msg2" "Should return last message's ID"))

(test-case "audit-lt-compute-parent-id-skips-session-info"
  (define info-msg (make-message "info1" #f 'system 'session-info '() 1000 (hasheq 'version 2)))
  (define m1 (make-message "msg1" #f 'user 'message (list (make-text-part "first")) 1001 (hasheq)))
  (define entries (list info-msg m1))
  (define parent (compute-parent-id entries))
  (check-equal? parent "msg1" "Should skip session-info entries"))

(test-case "audit-lt-inject-system-instructions"
  (define context '())
  (define instructions '("You are helpful." "Be concise."))
  (define result (inject-system-instructions context instructions))
  (check-equal? (length result) 1)
  (check-equal? (message-role (car result)) 'system)
  (check-true (string-contains? (format "~a" (message-content (car result))) "helpful")))

(test-case "audit-lt-inject-system-empty"
  (define result (inject-system-instructions '() '()))
  (check-equal? (length result) 1)
  (check-equal? (message-role (car result)) 'system))

(test-case "audit-lt-inject-system-existing-system"
  (define existing
    (list (make-message "sys1"
                        #f
                        'system
                        'system-instruction
                        (list (make-text-part "existing"))
                        1000
                        (hasheq))))
  (define result (inject-system-instructions existing '()))
  (check-equal? (length result) 1 "Should not prepend when system message already present"))

;; ---------------------------------------------------------------------------
;; 8. Session Store Integrity (Hash Chain)
;; ---------------------------------------------------------------------------

(test-case "audit-int-genesis-hash"
  (check-equal? GENESIS-HASH "genesis"))

(test-case "audit-int-canonical-jsexpr-string"
  (define result (canonical-jsexpr->string (hash 'b "x" 'a "y")))
  (check-true (string? result))
  ;; Keys should be sorted alphabetically: find position of \"a\" and \"b\"
  (define a-pos
    (for/first ([i (in-range (string-length result))]
                #:when (string=? (substring result i (min (string-length result) (+ i 3))) "\"a\""))
      i))
  (define b-pos
    (for/first ([i (in-range (string-length result))]
                #:when (string=? (substring result i (min (string-length result) (+ i 3))) "\"b\""))
      i))
  (when (and a-pos b-pos)
    (check-true (< a-pos b-pos) "Keys should be sorted: a before b")))

(test-case "audit-int-canonical-jsexpr-list"
  (define result (canonical-jsexpr->string '("x" "y")))
  (check-true (string-contains? result "x"))
  (check-true (string-contains? result "y")))

(test-case "audit-int-canonical-jsexpr-bool"
  (check-equal? (canonical-jsexpr->string #t) "true")
  (check-equal? (canonical-jsexpr->string #f) "false"))

(test-case "audit-int-canonical-jsexpr-number"
  (check-equal? (canonical-jsexpr->string 42) "42"))

(test-case "audit-int-compute-event-hash"
  (define entry (hasheq 'type "test" 'data "hello"))
  (define h (compute-event-hash entry GENESIS-HASH))
  (check-true (string? h))
  (check-true (> (string-length h) 0)))

(test-case "audit-int-compute-event-hash-deterministic"
  (define entry (hasheq 'type "test" 'data "hello"))
  (define h1 (compute-event-hash entry GENESIS-HASH))
  (define h2 (compute-event-hash entry GENESIS-HASH))
  (check-equal? h1 h2 "Same input should produce same hash"))

(test-case "audit-int-pending-marker"
  (define tmp-dir (make-temporary-file "session-test-~a" 'directory))
  (define log-path (build-path tmp-dir "test.jsonl"))
  (define marker-path (pending-marker-path log-path))
  ;; Initially no marker
  (check-false (has-pending-marker? log-path))
  ;; Write marker
  (write-pending-marker! log-path 5)
  (check-true (has-pending-marker? log-path))
  ;; Remove marker
  (remove-pending-marker! log-path)
  (check-false (has-pending-marker? log-path))
  ;; Cleanup
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-int-verify-hash-chain-valid"
  (define tmp-dir (make-temporary-file "session-chain-test-~a" 'directory))
  (define log-path (build-path tmp-dir "test.jsonl"))
  ;; Write two entries forming a hash chain
  (define e1 (hasheq 'id "e1" 'data "entry1"))
  (define h1 (compute-event-hash e1 GENESIS-HASH))
  (define e1-with-hash (hash-set* e1 'prev_hash GENESIS-HASH 'hash h1))
  (define e2 (hasheq 'id "e2" 'data "entry2"))
  (define h2 (compute-event-hash e2 h1))
  (define e2-with-hash (hash-set* e2 'prev_hash h1 'hash h2))
  (call-with-output-file log-path
                         (lambda (out)
                           (write-json e1-with-hash out)
                           (newline out)
                           (write-json e2-with-hash out)
                           (newline out))
                         #:mode 'text)
  (define result (verify-hash-chain log-path))
  (check-true (hash? result))
  (check-true (hash-ref result 'valid? #f) "Hash chain should be valid")
  (delete-directory/files tmp-dir #:must-exist? #f))

;; ---------------------------------------------------------------------------
;; 9. Session Context
;; ---------------------------------------------------------------------------

(test-case "audit-ctx-extract-path-settings-empty"
  (define result (extract-path-settings '()))
  (check-true (hash? result))
  (check-false (hash-has-key? result 'model)))

(test-case "audit-ctx-extract-path-settings-model-change"
  (define msg (make-message "m1" #f 'system 'model-change '() 1000 (hash 'model "claude-v2")))
  (define result (extract-path-settings (list msg)))
  (check-equal? (hash-ref result 'model) "claude-v2"))

(test-case "audit-ctx-extract-path-settings-thinking-change"
  (define msg (make-message "m1" #f 'system 'thinking-level-change '() 1000 (hash 'level 'high)))
  (define result (extract-path-settings (list msg)))
  (check-equal? (hash-ref result 'thinking-level) 'high))

;; ---------------------------------------------------------------------------
;; 10. Session Metadata
;; ---------------------------------------------------------------------------

(test-case "audit-meta-label-types"
  (check-true (label-type? 'checkpoint))
  (check-true (label-type? 'milestone))
  (check-true (label-type? 'branch-point))
  (check-false (label-type? 'bogus)))

(test-case "audit-meta-label-type-values"
  (check-equal? LABEL-CHECKPOINT 'checkpoint)
  (check-equal? LABEL-MILESTONE 'milestone)
  (check-equal? LABEL-BRANCH-POINT 'branch-point))

(test-case "audit-meta-session-name-entry-predicate"
  (define name-msg (make-message "n1" #f 'system 'session-info '() 1000 (hash 'name "My Session")))
  (define other-msg (make-message "o1" #f 'user 'message (list (make-text-part "hi")) 1000 (hasheq)))
  (check-true (session-name-entry? name-msg))
  (check-false (session-name-entry? other-msg)))

(test-case "audit-meta-entry-label-predicate"
  (define label-msg
    (make-message "l1" "target1" 'system 'entry-label '() 1000 (hash 'label-type "checkpoint")))
  (define other-msg (make-message "o1" #f 'user 'message (list (make-text-part "hi")) 1000 (hasheq)))
  (check-true (entry-label? label-msg))
  (check-false (entry-label? other-msg)))
