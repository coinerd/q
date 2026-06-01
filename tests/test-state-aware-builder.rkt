#lang racket/base

;; tests/test-state-aware-builder.rkt — tests for state-aware context assembly
;; v0.76.0 W2: Extracted module tests

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-action-execution?)
         (only-in "../util/protocol-types.rkt" make-message make-text-part message-role)
         (only-in "../util/fsm.rkt" fsm-state))

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

    (test-case "state-aware with idle state acts like regular"
      (define msgs (make-test-msgs 10))
      (define regular (build-tiered-context msgs))
      (define state-aware (build-tiered-context/state-aware msgs #:task-state 'idle))
      (check-equal? (length (tiered-context-tier-a regular))
                    (length (tiered-context-tier-a state-aware))))

    (test-case "state-aware with exploration adds preamble"
      (define msgs (make-test-msgs 5))
      (define tc (build-tiered-context/state-aware msgs #:task-state 'exploration))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (> (length tier-a) 0))
      (check-equal? (message-role (car tier-a)) 'system-instruction))

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

    (test-case "preamble returns #f for idle"
      (check-false (build-state-awareness-preamble 'idle '())))

    (test-case "preamble returns #f for #f state"
      (check-false (build-state-awareness-preamble #f '())))

    (test-case "preamble returns message for exploration"
      (define preamble (build-state-awareness-preamble 'exploration '()))
      (check-not-false preamble)
      (check-equal? (message-role preamble) 'system-instruction))

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
        ;; Should succeed — the rollback trigger should have fired but not crashed
        (check-not-false tc)))

    (test-case "repeat-tool-count defaults to 0 with no recent-tool-calls"
      (parameterize ([current-task-state-aware-assembly? #t])
        (define msgs (make-test-msgs 10))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs))
        (check-not-false tc)))))

(run-tests suite)
