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
      (check-not-false (hash-ref (rpc-response-result resp) 'session-id #f)))

    ;; ============================================================
    ;; Default implementations (no deps)
    ;; ============================================================
    (test-case "abort with default cancel-token returns status"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'abort))
      (define result (handler (hasheq)))
      (check-equal? (hash-ref result 'status) "aborted"))

    (test-case "fork default returns error"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'fork))
      (define result (handler (hasheq 'entryId "entry-1")))
      (check-equal? (hash-ref result 'status) "error"))

    ;; ============================================================
    ;; GC-17: New RPC methods
    ;; ============================================================

    (test-case "registry includes new GC-17 methods"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (for ([method (in-list '(prompt steer follow_up navigate send_message))])
        (check-not-false (hash-ref handlers method #f) (format "method ~a missing" method))))

    (test-case "prompt with message delegates to prompt-fn"
      (define received (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'prompt-fn
                                        (lambda (msg)
                                          (set-box! received msg)
                                          (hasheq 'status "ok" 'turn-id "t1")))))
      (define handler (hash-ref handlers 'prompt))
      (define result (handler (hasheq 'message "Hello")))
      (check-equal? (hash-ref result 'status) "ok")
      (check-equal? (unbox received) "Hello"))

    (test-case "prompt without message returns error"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'prompt))
      (define result (handler (hasheq)))
      (check-equal? (hash-ref result 'status) "error"))

    (test-case "steer with message delegates to steer-fn"
      (define received (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'steer-fn
                                        (lambda (msg)
                                          (set-box! received msg)
                                          (hasheq 'status "steered")))))
      (define handler (hash-ref handlers 'steer))
      (define result (handler (hasheq 'message "Change approach")))
      (check-equal? (hash-ref result 'status) "steered")
      (check-equal? (unbox received) "Change approach"))

    (test-case "steer without message returns error"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'steer))
      (define result (handler (hasheq)))
      (check-equal? (hash-ref result 'status) "error"))

    (test-case "follow_up with message delegates to follow-up-fn"
      (define received (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'follow-up-fn
                                        (lambda (msg)
                                          (set-box! received msg)
                                          (hasheq 'status "queued")))))
      (define handler (hash-ref handlers 'follow_up))
      (define result (handler (hasheq 'message "Next question")))
      (check-equal? (hash-ref result 'status) "queued")
      (check-equal? (unbox received) "Next question"))

    (test-case "navigate with target delegates to navigate-fn"
      (define received (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'navigate-fn
                                        (lambda (target)
                                          (set-box! received target)
                                          (hasheq 'status "ok" 'target target)))))
      (define handler (hash-ref handlers 'navigate))
      (define result (handler (hasheq 'target "entry-5")))
      (check-equal? (hash-ref result 'status) "ok")
      (check-equal? (unbox received) "entry-5"))

    (test-case "navigate without target returns error"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'navigate))
      (define result (handler (hasheq)))
      (check-equal? (hash-ref result 'status) "error"))

    (test-case "send_message with role and text delegates"
      (define received-role (box #f))
      (define received-text (box #f))
      (define handlers
        (make-core-rpc-handlers (hasheq 'send-message-fn
                                        (lambda (role text)
                                          (set-box! received-role role)
                                          (set-box! received-text text)
                                          (hasheq 'status "ok")))))
      (define handler (hash-ref handlers 'send_message))
      (define result (handler (hasheq 'role "user" 'text "Injected")))
      (check-equal? (hash-ref result 'status) "ok")
      (check-equal? (unbox received-role) "user")
      (check-equal? (unbox received-text) "Injected"))

    (test-case "send_message without required params returns error"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (define handler (hash-ref handlers 'send_message))
      (define result1 (handler (hasheq 'role "user")))
      (check-equal? (hash-ref result1 'status) "error")
      (define result2 (handler (hasheq 'text "hello")))
      (check-equal? (hash-ref result2 'status) "error"))

    (test-case "new methods default to error when no deps provided"
      (define handlers (make-core-rpc-handlers (hasheq)))
      (for ([method (in-list '(prompt steer follow_up navigate send_message))])
        (define handler (hash-ref handlers method))
        (define params
          (case method
            [(prompt steer follow_up) (hasheq 'message "test")]
            [(navigate) (hasheq 'target "t")]
            [(send_message) (hasheq 'role "user" 'text "test")]))
        (define result (handler params))
        (check-equal? (hash-ref result 'status)
                      "error"
                      (format "~a should error without handler" method))))))

;; ============================================================
;; Wired handler integration tests (Issue #1107)
;; ============================================================

(define (make-wired-handlers)
  ;; Simulate the wiring done in run-json-rpc.rkt
  (define core-deps
    (hasheq 'cancel-token
            (lambda () (void))
            'session-info-fn
            (lambda () (hasheq 'session-id "s-default" 'active? #t))
            'compact-fn
            (lambda () (hasheq 'status 'not-implemented))
            'fork-fn
            (lambda (entry-id) (hasheq 'newSessionId "forked-1" 'status "ok"))
            'subscribe-fn
            (lambda (filter) 1)
            'prompt-fn
            (lambda (msg) (hasheq 'status "ok" 'message msg))
            'steer-fn
            (lambda (msg) (hasheq 'status "steered"))
            'follow-up-fn
            (lambda (msg) (hasheq 'status "queued"))
            'navigate-fn
            (lambda (target) (hasheq 'status 'not-implemented))
            'send-message-fn
            (lambda (role text) (hasheq 'status 'not-implemented))))
  (define core (make-core-rpc-handlers core-deps))
  ;; Add session management + utility handlers
  (define extra
    (make-hash
     (list (cons 'ping (lambda (params) (hasheq 'pong #t)))
           (cons 'shutdown (lambda (params) (hasheq 'status "shutting-down")))
           (cons 'session.open (lambda (params) (hasheq 'sessionId "new-1")))
           (cons 'session.list (lambda (params) (hasheq 'sessions '())))
           (cons 'session.resume (lambda (params) (hasheq 'sessionId "s1" 'status "resumed")))
           (cons 'session.close (lambda (params) (hasheq 'sessionId "s1" 'status "closed"))))))
  (for/fold ([h core]) ([(k v) (in-hash extra)])
    (hash-set! h k v)
    h))

(define wired-handler-tests
  (test-suite "wired handler integration (Issue #1107)"

    (test-case "wired handlers contain all expected method keys"
      (define handlers (make-wired-handlers))
      (define expected-methods
        '(abort subscribe
                session_info
                compact
                fork
                prompt
                steer
                follow_up
                navigate
                send_message
                ping
                shutdown
                session.open
                session.list
                session.resume
                session.close))
      (for ([method (in-list expected-methods)])
        (check-not-false (hash-ref handlers method #f)
                         (format "wired handlers missing method: ~a" method))))

    (test-case "prompt via wired handler invokes deps function"
      (define received (box #f))
      (define deps
        (hasheq 'prompt-fn
                (lambda (msg)
                  (set-box! received msg)
                  (hasheq 'status "ok" 'message msg))))
      (define core (make-core-rpc-handlers deps))
      (define handler (hash-ref core 'prompt))
      (define result (handler (hasheq 'message "hello world")))
      (check-equal? (hash-ref result 'status) "ok")
      (check-equal? (unbox received) "hello world"))

    (test-case "steer via wired handler invokes deps function"
      (define received (box #f))
      (define deps
        (hasheq 'steer-fn
                (lambda (msg)
                  (set-box! received msg)
                  (hasheq 'status "steered"))))
      (define core (make-core-rpc-handlers deps))
      (define handler (hash-ref core 'steer))
      (define result (handler (hasheq 'message "redirect")))
      (check-equal? (hash-ref result 'status) "steered")
      (check-equal? (unbox received) "redirect"))

    (test-case "follow_up via wired handler invokes deps function"
      (define received (box #f))
      (define deps
        (hasheq 'follow-up-fn
                (lambda (msg)
                  (set-box! received msg)
                  (hasheq 'status "queued"))))
      (define core (make-core-rpc-handlers deps))
      (define handler (hash-ref core 'follow_up))
      (define result (handler (hasheq 'message "next")))
      (check-equal? (hash-ref result 'status) "queued")
      (check-equal? (unbox received) "next"))))

(run-tests rpc-method-tests)
(run-tests wired-handler-tests)
