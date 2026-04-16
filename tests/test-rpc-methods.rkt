#lang racket

;; tests/test-rpc-methods.rkt — FEAT-75: Core RPC methods

(require rackunit
         rackunit/text-ui
         "../interfaces/rpc-mode.rkt"
         "../wiring/rpc-methods.rkt")

(define rpc-method-tests
  (test-suite "core RPC methods"

    ;; ============================================================
    ;; Method registry structure
    ;; ============================================================
    (test-case "make-core-rpc-handlers returns hash with required methods"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (check-not-false (hash-ref handlers 'abort #f) "abort method missing")
      (check-not-false (hash-ref handlers 'subscribe #f) "subscribe method missing")
      (check-not-false (hash-ref handlers 'session_info #f) "session_info method missing")
      (check-not-false (hash-ref handlers 'compact #f) "compact method missing")
      (check-not-false (hash-ref handlers 'fork #f) "fork method missing"))

    ;; ============================================================
    ;; abort method
    ;; ============================================================
    (test-case "abort returns acknowledged status"
      (define cancelled? (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'cancel-token (lambda () (set-box! cancelled? #t)))))
      (define handler (hash-ref handlers 'abort))
      (define result (handler (hasheq)))
      (check-equal? (hash-ref result 'status) "aborted")
      (check-true (unbox cancelled?)))

    ;; ============================================================
    ;; session_info method
    ;; ============================================================
    (test-case "session_info returns session metadata"
      (define mock-session-info (hasheq 'session-id "test-123" 'active? #t 'history-length 5))
      (define handlers
        (make-core-rpc-handlers (hasheq 'session-info-fn (lambda () mock-session-info))))
      (define handler (hash-ref handlers 'session_info))
      (define result (handler (hasheq 'sessionId "test-123")))
      (check-equal? (hash-ref result 'session-id) "test-123")
      (check-equal? (hash-ref result 'active?) #t)
      (check-equal? (hash-ref result 'history-length) 5))

    (test-case "session_info returns error for missing session"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'session_info))
      (define result (handler (hasheq 'sessionId "nonexistent")))
      (check-equal? (hash-ref result 'status) "error")
      (check-not-false (hash-ref result 'message #f)))

    ;; ============================================================
    ;; compact method
    ;; ============================================================
    (test-case "compact returns compaction result"
      (define compact-called? (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'compact-fn
                                        (lambda ()
                                          (set-box! compact-called? #t)
                                          (hasheq 'removed 10 'kept 5)))))
      (define handler (hash-ref handlers 'compact))
      (define result (handler (hasheq 'persist #f)))
      (check-true (unbox compact-called?))
      (check-equal? (hash-ref result 'removed) 10))

    ;; ============================================================
    ;; fork method
    ;; ============================================================
    (test-case "fork returns new session id"
      (define handlers
        (make-core-rpc-handlers
         (hasheq 'fork-fn (lambda (entry-id) (hasheq 'newSessionId "fork-123" 'status "ok")))))
      (define handler (hash-ref handlers 'fork))
      (define result (handler (hasheq 'entryId "entry-5")))
      (check-equal? (hash-ref result 'newSessionId) "fork-123")
      (check-equal? (hash-ref result 'status) "ok"))

    (test-case "fork without entry-id passes #f"
      (define received-entry-id (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'fork-fn
                                        (lambda (entry-id)
                                          (set-box! received-entry-id entry-id)
                                          (hasheq 'newSessionId "fork-456" 'status "ok")))))
      (define handler (hash-ref handlers 'fork))
      (define result (handler (hasheq)))
      (check-false (unbox received-entry-id))
      (check-equal? (hash-ref result 'status) "ok"))

    ;; ============================================================
    ;; subscribe method
    ;; ============================================================
    (test-case "subscribe returns subscription id"
      (define handlers (make-core-rpc-handlers (hasheq 'subscribe-fn (lambda (filter) 42))))
      (define handler (hash-ref handlers 'subscribe))
      (define result (handler (hasheq)))
      (check-equal? (hash-ref result 'subscriptionId) 42))

    (test-case "subscribe with filter passes it through"
      (define received-filter (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'subscribe-fn
                                        (lambda (filter)
                                          (set-box! received-filter filter)
                                          99))))
      (define handler (hash-ref handlers 'subscribe))
      (define result (handler (hasheq 'filter '("model.stream.text" "tool.execution.start"))))
      (check-equal? (hash-ref result 'subscriptionId) 99)
      (check-not-false (unbox received-filter)))

    ;; ============================================================
    ;; dispatch integration
    ;; ============================================================
    (test-case "dispatch-rpc-request routes to registered method"
      (define handlers
        (make-core-rpc-handlers (hasheq 'session-info-fn (lambda () (hasheq 'session-id "s1")))))
      (define req (rpc-request "req-1" 'session_info (hasheq 'sessionId "s1")))
      (define resp (dispatch-rpc-request req handlers))
      (check-false (rpc-response-error resp))
      (check-not-false (hash-ref (rpc-response-result resp) 'session-id #f)))))

(run-tests rpc-method-tests)
