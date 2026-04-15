#lang racket

;; tests/test-compaction-hooks.rkt — tests for Enriched Compaction Hook and Events (#697-#699)
;;
;; Covers:
;;   - #697: Enriched session-before-compact hook payload
;;   - #698: Compaction-start and compaction-end events
;;   - #699: Parent feature integration

(require rackunit
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/compactor.rkt"
         "../runtime/compaction-hooks.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define msg-counter 0)
(define (next-id!)
  (set! msg-counter (add1 msg-counter))
  (format "msg-~a" msg-counter))

(define (make-user-msg text)
  (make-message (next-id!) #f 'user 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-assistant-msg text)
  (make-message (next-id!) #f 'assistant 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

;; ============================================================
;; #697: Enriched hook payload
;; ============================================================

(test-case "build-enriched-compact-payload: includes message counts"
  (define msgs (append (for/list ([i (in-range 10)]) (make-user-msg (format "q~a" i)))
                       (for/list ([i (in-range 5)]) (make-assistant-msg (format "a~a" i)))))
  (define strategy (compaction-strategy 5 3))
  (define payload (build-enriched-compact-payload msgs strategy))
  (check-equal? (hash-ref payload 'message-count) 15)
  (check-true (>= (hash-ref payload 'messages-to-summarize) 0))
  (check-true (positive? (hash-ref payload 'tokens-before))))

(test-case "build-enriched-compact-payload: includes strategy"
  (define msgs (list (make-user-msg "hello")))
  (define strategy (compaction-strategy 5 3))
  (define payload (build-enriched-compact-payload msgs strategy))
  (check-equal? (hash-ref payload 'strategy) strategy))

(test-case "build-enriched-compact-payload: includes previous-summary"
  (define msgs (list (make-user-msg "hello")))
  (define strategy (compaction-strategy 5 3))
  (define payload (build-enriched-compact-payload msgs strategy
                                                    #:previous-summary "old summary"))
  (check-equal? (hash-ref payload 'previous-summary) "old summary"))

(test-case "build-enriched-compact-payload: includes session-id"
  (define msgs (list (make-user-msg "hello")))
  (define strategy (compaction-strategy 5 3))
  (define payload (build-enriched-compact-payload msgs strategy
                                                    #:session-id "sess-123"))
  (check-equal? (hash-ref payload 'session-id) "sess-123"))

(test-case "dispatch-enriched-before-compact: returns payload and hook result"
  (define msgs (for/list ([i (in-range 8)]) (make-user-msg (format "q~a" i))))
  (define strategy (compaction-strategy 5 3))
  (define-values (hook-res payload)
    (dispatch-enriched-before-compact #f msgs strategy))
  (check-false hook-res)
  (check-true (hash? payload)))

(test-case "dispatch-enriched-before-compact: hook receives enriched payload"
  (define msgs (for/list ([i (in-range 8)]) (make-user-msg (format "q~a" i))))
  (define strategy (compaction-strategy 5 3))
  (define received-payload #f)
  (define (mock-hook topic payload)
    (set! received-payload payload)
    (hook-result 'pass (hasheq)))
  (define-values (hook-res payload)
    (dispatch-enriched-before-compact mock-hook msgs strategy))
  (check-true (hash? received-payload))
  (check-true (hash-has-key? received-payload 'message-count))
  (check-true (hook-result? hook-res)))

(test-case "maybe-use-custom-summary: extracts custom summary from replace action"
  (define res (hook-result 'replace (hasheq 'summary "custom summary text")))
  (check-equal? (maybe-use-custom-summary res) "custom summary text"))

(test-case "maybe-use-custom-summary: returns #f for pass action"
  (define res (hook-result 'pass (hasheq 'summary "ignored")))
  (check-false (maybe-use-custom-summary res)))

(test-case "maybe-use-custom-summary: returns #f for #f input"
  (check-false (maybe-use-custom-summary #f)))

;; ============================================================
;; #698: Compaction events
;; ============================================================

(test-case "make-compaction-start-event: creates event with correct topic"
  (define evt (make-compaction-start-event 'threshold 100 5000 "sess-1" "turn-1"))
  (check-equal? (event-ev evt) "compaction.start")
  (define payload (event-payload evt))
  (check-equal? (hash-ref payload 'reason) 'threshold)
  (check-equal? (hash-ref payload 'message-count) 100)
  (check-equal? (hash-ref payload 'tokens-before) 5000))

(test-case "make-compaction-end-event: creates event with correct topic"
  (define evt (make-compaction-end-event 'overflow 50 5000 2000 "sess-1" "turn-1"))
  (check-equal? (event-ev evt) "compaction.end")
  (define payload (event-payload evt))
  (check-equal? (hash-ref payload 'reason) 'overflow)
  (check-equal? (hash-ref payload 'removed-count) 50)
  (check-equal? (hash-ref payload 'tokens-before) 5000)
  (check-equal? (hash-ref payload 'tokens-after) 2000)
  (check-true (hash-ref payload 'summary-generated?)))

(test-case "make-compaction-end-event: respects summary-generated flag"
  (define evt (make-compaction-end-event 'threshold 5 3000 2500 "sess-1" "turn-1"
                                          #:summary-generated? #f))
  (check-false (hash-ref (event-payload evt) 'summary-generated?)))

(test-case "publish-compaction-start!: publishes to event bus"
  (define bus (make-event-bus))
  (define received #f)
  (subscribe! bus (lambda (evt) (set! received evt)))
  (publish-compaction-start! bus 'threshold 50 3000 "sess-1" "turn-1")
  (check-true (event? received))
  (check-equal? (event-ev received) "compaction.start"))

(test-case "publish-compaction-end!: publishes to event bus"
  (define bus (make-event-bus))
  (define received #f)
  (subscribe! bus (lambda (evt) (set! received evt)))
  (publish-compaction-end! bus 'overflow 30 5000 2000 "sess-1" "turn-1")
  (check-true (event? received))
  (check-equal? (event-ev received) "compaction.end"))

(test-case "publish-compaction-start!: handles #f bus gracefully"
  ;; Should not crash when bus is #f
  (publish-compaction-start! #f 'threshold 50 3000 "sess-1" "turn-1")
  (check-true #t))

(test-case "publish-compaction-end!: handles #f bus gracefully"
  (publish-compaction-end! #f 'overflow 30 5000 2000 "sess-1" "turn-1")
  (check-true #t))

(test-case "compaction events: topic constants are strings"
  (check-true (string? compaction-start-topic))
  (check-true (string? compaction-end-topic))
  (check-equal? compaction-start-topic "compaction.start")
  (check-equal? compaction-end-topic "compaction.end"))

;; ============================================================
;; #699: Integration
;; ============================================================

(test-case "integration: enriched hook with events"
  ;; Simulate full enriched compaction flow
  (define bus (make-event-bus))
  (define events-received '())
  (subscribe! bus (lambda (evt) (set! events-received (cons evt events-received))))

  (define msgs (for/list ([i (in-range 20)])
                 (if (even? i)
                     (make-user-msg (format "question ~a" i))
                     (make-assistant-msg (format "answer ~a" i)))))
  (define strategy (compaction-strategy 10 5))

  ;; Dispatch enriched hook
  (define custom-summary #f)
  (define (mock-hook topic payload)
    ;; Hook sees enriched data
    (check-true (hash-has-key? payload 'message-count))
    (check-true (hash-has-key? payload 'tokens-before))
    ;; Return replace with custom summary
    (hook-result 'replace (hasheq 'summary "extension-provided summary")))

  (define-values (hook-res enriched-payload)
    (dispatch-enriched-before-compact mock-hook msgs strategy
                                       #:session-id "sess-integration"))

  ;; Publish start event
  (publish-compaction-start! bus 'threshold (length msgs)
                              (hash-ref enriched-payload 'tokens-before)
                              "sess-integration" "turn-1")

  ;; Check hook result
  (check-true (hook-result? hook-res))

  ;; Check custom summary extraction
  (set! custom-summary (maybe-use-custom-summary hook-res))
  (check-equal? custom-summary "extension-provided summary")

  ;; Publish end event
  (publish-compaction-end! bus 'threshold 10
                           (hash-ref enriched-payload 'tokens-before)
                           2000 "sess-integration" "turn-1")

  ;; Verify events received
  (check-equal? (length events-received) 2)
  (check-equal? (event-ev (second events-received)) "compaction.start")
  (check-equal? (event-ev (first events-received)) "compaction.end"))
