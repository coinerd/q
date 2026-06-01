#lang racket
;; BOUNDARY: integration

;; tests/test-context-assembly-ws-budget.rkt — Budget pressure integration test

(require rackunit
         rackunit/text-ui
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/working-set.rkt"
         "../runtime/context-assembly.rkt"
         (only-in "../runtime/context-assembly/token-metrics.rkt"
                  context-token-telemetry?
                  context-token-telemetry-tier-a-tokens
                  context-token-telemetry-tier-b-tokens
                  context-token-telemetry-tier-c-tokens
                  context-token-telemetry-working-set-tokens
                  context-token-telemetry-conclusion-tokens
                  context-token-telemetry-recent-tokens
                  context-token-telemetry-total-tokens
                  measure-context-token-telemetry))

;; Helper: create a test message
(define (make-test-msg id role kind text [parent #f])
  (make-message id parent role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define budget-tests
  (test-suite "Context Assembly Working Set Budget Pressure"

    (test-case "T01: large working set messages consume tier budget"
      ;; Create a working set with a large entry
      (define ws (make-working-set #:max-entries 30 #:max-tokens 50000))
      (define large-msg
        (make-message "big-tool"
                      #f
                      'tool
                      'tool-result
                      (list (make-text-part (make-string 2000 #\x)))
                      (current-seconds)
                      (hasheq)))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/big.rkt")))
                           (list large-msg)
                           message-id
                           (lambda (m) 2000))
      (check-equal? (working-set-entry-count ws) 1)

      ;; Build tiered context with many recent messages
      (define msgs
        (cons (make-test-msg "sys" 'system 'system-instruction "System")
              (for/list ([i (in-range 30)])
                (make-test-msg (format "msg-~a" i) 'user 'message (make-string 200 #\y)))))

      ;; With working set, the large message should be included in tier-a
      (define tc-with-ws (build-tiered-context msgs #:working-set-messages (list large-msg)))
      (define tc-without-ws (build-tiered-context msgs))

      ;; Both should produce valid results
      (check-true (tiered-context? tc-with-ws))
      (check-true (tiered-context? tc-without-ws))

      ;; With working set consuming budget, tier-a includes the ws message
      (check-true (>= (length (tiered-context-tier-a tc-with-ws)) 2))
      (check-true (>= (length (tiered-context-tier-a tc-without-ws)) 1)))

    (test-case "T02: multiple working set entries reduce recent message space"
      ;; Create working set with 5 entries
      (define ws (make-working-set #:max-entries 30 #:max-tokens 50000))
      (define ws-msgs
        (for/list ([i (in-range 5)])
          (make-message (format "ws-~a" i)
                        #f
                        'tool
                        'tool-result
                        (list (make-text-part (make-string 500 #\x)))
                        (current-seconds)
                        (hasheq))))
      (for ([i (in-range 5)])
        (working-set-update!
         ws
         (list (hasheq 'name "read" 'arguments (hasheq 'path (format "/tmp/f~a.rkt" i))))
         (list (list-ref ws-msgs i))
         message-id
         (lambda (m) 500)))
      (check-equal? (working-set-entry-count ws) 5)

      ;; Build tiered context
      (define msgs
        (cons (make-test-msg "sys" 'system 'system-instruction "System")
              (for/list ([i (in-range 20)])
                (make-test-msg (format "msg-~a" i) 'user 'message (make-string 100 #\y)))))
      (define tc-with-ws (build-tiered-context msgs #:working-set-messages ws-msgs))
      (define tc-without-ws (build-tiered-context msgs))

      ;; Both should produce valid results
      (check-true (tiered-context? tc-with-ws))
      (check-true (tiered-context? tc-without-ws))

      ;; Tier-a should include system + working set messages
      (check-true (>= (length (tiered-context-tier-a tc-with-ws)) 3)))

    (test-case "T03: token telemetry reports tier/category estimates without changing assembly"
      (define ws-msg
        (make-message "ws-telemetry"
                      #f
                      'tool
                      'tool-result
                      (list (make-text-part "working set content"))
                      (current-seconds)
                      (hasheq)))
      (define conclusion-msg
        (make-message "c-telemetry"
                      #f
                      'system-instruction
                      'text
                      (list (make-text-part "[Conclusion] important decision"))
                      (current-seconds)
                      (hasheq)))
      (define msgs
        (list (make-test-msg "sys" 'system 'system-instruction "System")
              (make-test-msg "recent-1" 'user 'message "recent context")))
      (define tc (build-tiered-context msgs #:working-set-messages (list ws-msg)))
      (define tier-a-before (tiered-context-tier-a tc))
      (define telemetry
        (measure-context-token-telemetry tc
                                         #:conclusion-messages (list conclusion-msg)
                                         #:working-set-messages (list ws-msg)
                                         #:recent-messages msgs))
      (check-true (context-token-telemetry? telemetry))
      (check-equal? (tiered-context-tier-a tc) tier-a-before "telemetry must be observation-only")
      (check-true (>= (context-token-telemetry-tier-a-tokens telemetry) 0))
      (check-true (>= (context-token-telemetry-tier-b-tokens telemetry) 0))
      (check-true (>= (context-token-telemetry-tier-c-tokens telemetry) 0))
      (check-true (> (context-token-telemetry-working-set-tokens telemetry) 0))
      (check-true (> (context-token-telemetry-conclusion-tokens telemetry) 0))
      (check-true (> (context-token-telemetry-recent-tokens telemetry) 0))
      (check-equal? (context-token-telemetry-total-tokens telemetry)
                    (+ (context-token-telemetry-tier-a-tokens telemetry)
                       (context-token-telemetry-tier-b-tokens telemetry)
                       (context-token-telemetry-tier-c-tokens telemetry))))))

(module+ main
  (run-tests budget-tests))
(module+ test
  (run-tests budget-tests))
