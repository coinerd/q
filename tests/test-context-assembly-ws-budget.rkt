#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: integration

;; tests/test-context-assembly-ws-budget.rkt — Budget pressure integration test

(require rackunit
         rackunit/text-ui
         racket/list
         "../util/message/protocol-types.rkt"
         "../runtime/working-set.rkt"
         "../runtime/context/context-assembly.rkt"
         (rename-in (only-in "../runtime/turn-orchestrator.rkt" build-assembled-context)
                    [build-assembled-context build-turn-context])
         (only-in "../runtime/session/session-config.rkt" hash->session-config)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
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

    (test-case ">100-message provider context enforces actual 30 percent WS share"
      (define ws (make-working-set #:max-entries 20 #:max-tokens 8192))
      (define assistant-parent
        (make-message "large-parent"
                      #f
                      'assistant
                      'message
                      (for/list ([i (in-range 8)])
                        (make-tool-call-part (format "large-call-~a" i)
                                             "read"
                                             (hasheq 'path (format "/tmp/large-~a.rkt" i))))
                      (current-seconds)
                      (hasheq)))
      (define ws-msgs
        (for/list ([i (in-range 8)])
          (make-message
           (format "large-ws-~a" i)
           "large-parent"
           'tool
           'tool-result
           (list (make-tool-result-part (format "large-call-~a" i) (make-string 2000 #\w) #f))
           (current-seconds)
           (hasheq 'toolCallId (format "large-call-~a" i) 'isError #f))))
      (for ([m (in-list ws-msgs)]
            [i (in-naturals)])
        (working-set-add! ws (format "/tmp/large-~a.rkt" i) (message-id m) 500))
      (define history
        (append (list assistant-parent)
                ws-msgs
                (for/list ([i (in-range 105)])
                  (make-test-msg (format "history-~a" i) 'assistant 'message (make-string 500 #\h)))))
      (define config
        (hash->session-config (hasheq 'working-set
                                      ws
                                      'project-dir
                                      "/tmp/project"
                                      'tier-b-count
                                      20
                                      'tier-c-count
                                      4
                                      'max-tokens
                                      8192)))
      (define assembled (build-turn-context history config #f (make-event-bus) "share-test" 1))
      (define assembled-tokens (estimate-context-tokens assembled))
      (define original-ws-ids (map message-id ws-msgs))
      (define provider-ws-ids
        (filter (lambda (id) (member id original-ws-ids)) (map message-id assembled)))
      (define active-ws-ids (map ws-entry-message-id (working-set-entries ws)))
      (check-true (> (length history) 100))
      (check-equal? (sort provider-ws-ids string<?) (sort active-ws-ids string<?))
      (define retained-parent (findf (lambda (m) (equal? (message-id m) "large-parent")) assembled))
      (check-not-false retained-parent (format "assembled IDs: ~a" (map message-id assembled)))
      (define retained-call-ids
        (for/list ([part (in-list (message-content retained-parent))]
                   #:when (tool-call-part? part))
          (tool-call-part-id part)))
      (define retained-result-call-ids
        (apply append
               (for/list ([m (in-list assembled)]
                          #:when (and (eq? (message-role m) 'tool)
                                      (equal? (message-parent-id m) "large-parent")))
                 (for/list ([part (in-list (message-content m))]
                            #:when (tool-result-part? part))
                   (tool-result-part-tool-call-id part)))))
      (check-equal? (sort retained-call-ids string<?) (sort retained-result-call-ids string<?))
      (check-true (<= (working-set-token-count ws) (compute-working-set-budget assembled-tokens))))

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

(module+ test
  (run-tests budget-tests))

(module+ main
  (run-tests budget-tests))
