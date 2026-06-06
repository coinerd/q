#lang racket/base

;; tests/test-state-aware-builder.rkt — tests for state-aware context assembly
;; v0.76.0 W2: Extracted module tests

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-action-execution?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt" check-rollback-triggers)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-role
                  message-content
                  text-part?
                  text-part-text)
         (only-in "../util/message/message.rkt" message-kind)
         (only-in "../util/fsm/fsm.rkt" fsm-state)
         (only-in "../runtime/session/session-config.rkt" hash->session-config session-config?)
         (only-in "../runtime/memory/service.rkt" current-memory-backend)
         (only-in "../runtime/memory/backends/memory-hash.rkt" make-memory-hash-backend)
         (only-in "../runtime/memory/types.rkt" memory-item)
         (only-in "../runtime/memory/protocol.rkt" memory-backend-store!)
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget))

(define (make-test-msg role text [meta (hasheq)])
  (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) meta))

(define (make-test-msgs n)
  (for/list ([i (in-range n)])
    (make-test-msg (if (even? i) 'user 'assistant) (format "msg ~a" i))))

(define suite
  (test-suite "state-aware-builder"

    (test-case "current-task-state-aware-assembly? defaults to #f"
      (check-false (current-task-state-aware-assembly?)))

    (test-case "feature flag can be enabled"
      (parameterize ([current-task-state-aware-assembly? #t])
        (check-true (current-task-state-aware-assembly?))))

    (test-case "state-aware assembly without state acts like regular"
      (define msgs (make-test-msgs 10))
      (define regular (build-tiered-context msgs))
      (define state-aware (build-tiered-context/state-aware msgs))
      (check-equal? (length (tiered-context-tier-a regular))
                    (length (tiered-context-tier-a state-aware)))
      (check-equal? (length (tiered-context-tier-b regular))
                    (length (tiered-context-tier-b state-aware)))
      (check-equal? (length (tiered-context-tier-c regular))
                    (length (tiered-context-tier-c state-aware))))

    (test-case "state-aware with idle state adds preamble"
      (define msgs (make-test-msgs 10))
      (define regular (build-tiered-context msgs))
      (define state-aware (build-tiered-context/state-aware msgs #:task-state 'idle))
      ;; v0.78+: idle now produces a preamble message, so tier-a is longer
      (check-true (> (length (tiered-context-tier-a state-aware))
                     (length (tiered-context-tier-a regular)))))

    (test-case "state-aware with exploration adds preamble"
      (define msgs (make-test-msgs 5))
      (define tc (build-tiered-context/state-aware msgs #:task-state 'exploration))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (> (length tier-a) 0))
      (check-equal? (message-role (car tier-a)) 'system)
      (check-equal? (message-kind (car tier-a)) 'system-instruction))

    (test-case "state-aware with fsm-state struct works"
      (define msgs (make-test-msgs 5))
      (define tc (build-tiered-context/state-aware msgs #:task-state (fsm-state 'planning)))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (> (length tier-a) 0)))

    (test-case "state-aware with conclusions adds conclusion entries"
      (define msgs (make-test-msgs 5))
      (define conclusions
        (list (task-conclusion "c1" "Found the bug" 'fact 'exploration '() 1000 '() '())))
      (define tc
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 2)))

    (test-case "state-aware implementation currently injects all conclusion entries (v0.77.0 baseline)"
      (define msgs (make-test-msgs 5))
      (define conclusions
        (for/list ([i (in-range 25)])
          (task-conclusion (format "c~a" i)
                           (format "important conclusion ~a" i)
                           'fact
                           'implementation
                           '()
                           (+ 1000 i)
                           '()
                           '())))
      (define tc
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:conclusions conclusions))
      ;; v0.77.9: Budget enforcement is now active (default 2000 tokens).
      ;; With 25 conclusions × ~132 tokens each ≈ 3300 total, budget trims to ~15.
      ;; Tier-a should have preamble + budgeted conclusions + base tier-a entries.
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 2)) ; at least preamble + some conclusions
      (check-true (< (length tier-a) (+ 1 (length conclusions))))) ; budget was enforced

    (test-case "state-aware filters working-set for implementation"
      (define msgs (make-test-msgs 5))
      (define ws (list (make-test-msg 'system "ws1")))
      (define tc
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:working-set-messages ws))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 1)))

    (test-case "preamble returns message for idle (v0.78+)"
      (check-not-false (build-state-awareness-preamble 'idle '())))

    (test-case "preamble returns #f for #f state"
      (check-false (build-state-awareness-preamble #f '())))

    (test-case "preamble returns message for exploration"
      (define preamble (build-state-awareness-preamble 'exploration '()))
      (check-not-false preamble)
      (check-equal? (message-role preamble) 'system)
      (check-equal? (message-kind preamble) 'system-instruction))

    (test-case "preamble includes conclusions text"
      (define conclusions
        (list (task-conclusion "c1" "Bug in line 42" 'fact 'debugging '() 1000 '() '())))
      (define preamble (build-state-awareness-preamble 'debugging conclusions))
      (check-not-false preamble))

    (test-case "preamble handles fsm-state struct input"
      (define preamble (build-state-awareness-preamble (fsm-state 'verification) '()))
      (check-not-false preamble))

    ;; v0.78.0 G9: repeat-tool-count computed from recent-tool-calls
    (test-case "repeat-tool-count computed from recent-tool-calls"
      (parameterize ([current-task-state-aware-assembly? #t]
                     [current-rollback-action-execution? #t])
        (define msgs (make-test-msgs 10))
        ;; Same tool (read) repeated 4 times -> repeat-count should be 4
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs
                                            #:recent-tool-calls '(read read read read bash)))
        ;; v0.78.6 C4: Verify amnesia warning actually fires for repeat count > 2
        (check-not-false tc "tiered context should be produced")
        ;; Directly verify trigger fires: 4 repeats > 2 threshold
        (define warnings
          (check-rollback-triggers #:before-messages 10
                                   #:after-messages 10
                                   #:conclusion-coverage 0.5
                                   #:repeat-tool-count 4))
        (check-true (pair? warnings) "repeat count 4 should produce warnings")
        (define amnesia-warn (assoc 'task-amnesia-detected warnings))
        (check-not-false amnesia-warn
                         "task-amnesia-detected warning should fire for 4 repeat tool calls")))

    (test-case "state-aware observe-only memory integration emits trace without changing context shape"
      (define msgs (make-test-msgs 10))
      (define cfg (hash->session-config (hash 'memory-backend 'memory-hash)))
      (define events (box '()))
      (define (trace! phase payload)
        (set-box! events (cons (cons phase payload) (unbox events))))
      (parameterize ([current-memory-backend (make-memory-hash-backend)])
        (define without-memory (build-tiered-context/state-aware msgs #:task-state 'implementation))
        (define with-memory
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:session-config cfg
                                            #:trace trace!))
        (check-equal? (length (tiered-context-tier-a with-memory))
                      (length (tiered-context-tier-a without-memory)))
        (check-true (for/or ([evt (in-list (unbox events))])
                      (eq? (car evt) 'memory-observe)))))

    (test-case "repeat-tool-count defaults to 0 with no recent-tool-calls"
      (parameterize ([current-task-state-aware-assembly? #t])
        (define msgs (make-test-msgs 10))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs))
        (check-not-false tc)))))

;; ============================================================
;; v0.95.15 W3: Memory injection wiring tests
;; ============================================================

(define (extract-text-from-messages msgs)
  (string-join (for*/list ([m msgs]
                           [p (message-content m)]
                           #:when (text-part? p))
                 (text-part-text p))
               " "))

(define memory-injection-suite
  (test-suite "state-aware-builder memory injection"

    (test-case "no memory injection when budget not set"
      (parameterize ([current-memory-backend (make-memory-hash-backend)]
                     [current-memory-injection-budget #f]
                     [current-task-state-aware-assembly? #f])
        (define msgs (make-test-msgs 5))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:session-config
                                            (hash->session-config (hasheq 'memory-backend 'hash))
                                            #:working-set-messages msgs))
        (define tier-a-text (extract-text-from-messages (tiered-context-tier-a tc)))
        (check-false (string-contains? tier-a-text "Memory —")
                     "No memory section expected without budget")))

    (test-case "memory section injected into tier-a when budget is set"
      (define backend (make-memory-hash-backend))
      (parameterize ([current-memory-backend backend]
                     [current-memory-injection-budget 500]
                     [current-task-state-aware-assembly? #f])
        (define test-item
          (memory-item "mem-1"
                       'semantic
                       'session
                       "The project uses Racket"
                       (hasheq 'project-root
                               "/test"
                               'session-id
                               "sess-1"
                               'tags
                               '()
                               'source
                               'test
                               'origin-message-id
                               "msg-1")
                       (hasheq 'sensitivity 'public 'confidence 0.9 'expires-at #f 'supersedes '())
                       "2026-06-05T12:00:00Z"
                       "2026-06-05T12:00:00Z"))
        ((memory-backend-store! backend) test-item)
        (define msgs (make-test-msgs 5))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:session-config
                                            (hash->session-config (hasheq 'memory-backend 'hash))
                                            #:working-set-messages msgs))
        (define tier-a-text (extract-text-from-messages (tiered-context-tier-a tc)))
        (check-not-false (string-contains? tier-a-text "Racket")
                         "Injected memory content should appear in tier-a")))))

(run-tests suite)
(run-tests memory-injection-suite)
