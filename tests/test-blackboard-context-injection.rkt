#lang racket/base

;; tests/test-blackboard-context-injection.rkt
;; v0.99.21 W3 (§4.3): Tests for blackboard context injection into subagent system prompt.

;; @speed fast
(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         "../agent/blackboard.rkt"
         "../tools/builtins/spawn-subagent.rkt")

(define suite
  (test-suite "Blackboard Context Injection for Subagents (v0.99.21 §4.3)"

    (test-case "build-subagent-blackboard-context returns empty when no blackboard"
      (parameterize ([current-blackboard #f])
        (define ctx (build-subagent-blackboard-context))
        (check-equal? ctx "")))

    (test-case "build-subagent-blackboard-context returns empty for empty blackboard"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (define ctx (build-subagent-blackboard-context))
        (check-equal? ctx "")))

    (test-case "build-subagent-blackboard-context returns content when blackboard has activity"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (update-blackboard! (lambda (state)
                              (struct-copy blackboard-state
                                           state
                                           [active-plan
                                            (hasheq 'wave-count 3 'summary "test plan")]))))
      (parameterize ([current-blackboard bb])
        (define ctx (build-subagent-blackboard-context))
        (check-true (> (string-length ctx) 0) "should have content")
        (check-true (string-contains? ctx "Parent Session Context") "should have header")
        (check-true (string-contains? ctx "test plan") "should include plan summary")))

    (test-case "build-subagent-blackboard-context includes wave status"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (update-blackboard!
         (lambda (state)
           (struct-copy blackboard-state state [wave-status (hasheq "w0" 'done "w1" 'in-progress)]))))
      (parameterize ([current-blackboard bb])
        (define ctx (build-subagent-blackboard-context))
        (check-true (string-contains? ctx "Parent Session Context"))
        (check-true (string-contains? ctx "w0") "should include wave status")))

    (test-case "build-subagent-blackboard-context includes verifier decisions"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (update-blackboard! (lambda (state)
                              (struct-copy blackboard-state
                                           state
                                           [verifier-decisions
                                            (list (hasheq 'verdict 'approved 'risk-level 'low))]))))
      (parameterize ([current-blackboard bb])
        (define ctx (build-subagent-blackboard-context))
        (check-true (> (string-length ctx) 0))
        (check-true (string-contains? ctx "Verifier") "should include verifier info")))

    (test-case "build-subagent-blackboard-context caps token budget"
      ;; Add many activities to verify token budget guard
      (define bb (make-blackboard))
      (define many-waves
        (for/hasheq ([i (in-range 100)])
          (values (string->symbol (format "wave-~a" i)) 'done)))
      (parameterize ([current-blackboard bb])
        (update-blackboard! (lambda (state)
                              (struct-copy blackboard-state state [wave-status many-waves]))))
      (parameterize ([current-blackboard bb])
        (define ctx (build-subagent-blackboard-context))
        ;; MAX-BLACKBOARD-SNIPPET-LEN is 500, plus our header should keep it bounded
        (check-true (<= (string-length ctx) 600)
                    (format "context too long: ~a chars" (string-length ctx)))))))

(run-tests suite)
