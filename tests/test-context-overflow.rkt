#lang racket

;; tests/test-context-overflow.rkt — tests for context overflow recovery (GC-27)
;;
;; Tests:
;; 1. context-overflow-error? — pattern detection
;; 2. call-with-overflow-recovery — compact and retry on overflow
;; 3. Event emission during overflow recovery

(require rackunit
         rackunit/text-ui
         racket/exn
         "../runtime/auto-retry.rkt"
         "../runtime/iteration.rkt"
         "../runtime/compactor.rkt"
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt")

;; Helper: create a simple message for testing
(define (make-test-msg [role 'user] [content "test"])
  (define id (symbol->string (gensym 'msg)))
  (message id #f role 'text (list (hasheq 'type 'text 'text content)) (current-seconds) (hasheq)))

(define context-overflow-tests
  (test-suite "Context Overflow Recovery (GC-27)"

    ;; -------------------------------------------------------
    ;; context-overflow-error? — pattern matching
    ;; -------------------------------------------------------
    (test-case "context-overflow-error?: detects context_length"
      (check-true (context-overflow-error? (exn:fail "context_length exceeded"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects context length (with space)"
      (check-true (context-overflow-error? (exn:fail "This model has a maximum context length of 4096"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects maximum context"
      (check-true (context-overflow-error? (exn:fail "Request exceeds maximum context size"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects too many tokens"
      (check-true (context-overflow-error? (exn:fail "too many tokens in request"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects token limit"
      (check-true (context-overflow-error? (exn:fail "token limit reached"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects max_tokens"
      (check-true (context-overflow-error? (exn:fail "input exceeds max_tokens"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects input is too long"
      (check-true (context-overflow-error? (exn:fail "input is too long for this model"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects request too large"
      (check-true (context-overflow-error? (exn:fail "request too large"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects reduce the length"
      (check-true (context-overflow-error? (exn:fail "Please reduce the length of your input"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: detects exceeds the maximum"
      (check-true (context-overflow-error? (exn:fail "content exceeds the maximum allowed"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: case insensitive"
      (check-true (context-overflow-error? (exn:fail "CONTEXT_LENGTH EXCEEDED"
                                                     (current-continuation-marks)))))

    (test-case "context-overflow-error?: returns #f for non-overflow errors"
      (check-false (context-overflow-error? (exn:fail "connection refused"
                                                      (current-continuation-marks)))))

    (test-case "context-overflow-error?: returns #f for unrelated errors"
      (check-false (context-overflow-error? (exn:fail "file not found"
                                                      (current-continuation-marks)))))

    ;; -------------------------------------------------------
    ;; call-with-overflow-recovery — compact and retry
    ;; -------------------------------------------------------
    (test-case "call-with-overflow-recovery: succeeds when no overflow"
      (define bus (make-event-bus))
      (define ctx (list (make-test-msg)))
      (define result (call-with-overflow-recovery (lambda () 'success) ctx bus "test-session"))
      (check-equal? result 'success))

    (test-case "call-with-overflow-recovery: recovers from overflow error"
      (define bus (make-event-bus))
      (define ctx
        (for/list ([i (in-range 20)])
          (make-test-msg 'user (format "msg ~a" i))))
      (define attempt-box (box 0))
      (define result
        (call-with-overflow-recovery (lambda ()
                                       (set-box! attempt-box (add1 (unbox attempt-box)))
                                       (if (= (unbox attempt-box) 1)
                                           (raise (exn:fail "context_length exceeded"
                                                            (current-continuation-marks)))
                                           'recovered))
                                     ctx
                                     bus
                                     "test-session"))
      (check-equal? result 'recovered)
      (check-equal? (unbox attempt-box) 2))

    (test-case "call-with-overflow-recovery: re-raises non-overflow errors"
      (define bus (make-event-bus))
      (define ctx (list (make-test-msg)))
      (check-exn (lambda (e) (and (exn:fail? e) (string-contains? (exn-message e) "unrelated error")))
                 (lambda ()
                   (call-with-overflow-recovery
                    (lambda () (raise (exn:fail "unrelated error" (current-continuation-marks))))
                    ctx
                    bus
                    "test-session"))))

    ;; -------------------------------------------------------
    ;; Event emission during overflow recovery
    ;; -------------------------------------------------------
    (test-case "call-with-overflow-recovery: emits overflow detected event"
      (define bus (make-event-bus))
      (define recorded-events '())
      (define sub-id
        (subscribe! bus
                    (lambda (evt) (set! recorded-events (append recorded-events (list evt))))
))
      (define ctx
        (for/list ([i (in-range 20)])
          (make-test-msg 'user (format "msg ~a" i))))
      (define attempt-box (box 0))
      (call-with-overflow-recovery (lambda ()
                                     (set-box! attempt-box (add1 (unbox attempt-box)))
                                     (when (= (unbox attempt-box) 1)
                                       (raise (exn:fail "too many tokens"
                                                        (current-continuation-marks)))))
                                   ctx
                                   bus
                                   "test-session")
      (unsubscribe! bus sub-id)
      ;; Should have emitted detected + compacted events
      (define event-names (map event-ev recorded-events))
      (check-not-false (member "context.overflow.detected" event-names))
      (check-not-false (member "context.overflow.compacted" event-names)))

    (test-case "call-with-overflow-recovery: no events when no overflow"
      (define bus (make-event-bus))
      (define recorded-events '())
      (define sub-id
        (subscribe! bus
                    (lambda (evt) (set! recorded-events (append recorded-events (list evt))))
))
      (define ctx (list (make-test-msg)))
      (call-with-overflow-recovery (lambda () 'ok) ctx bus "test-session")
      (unsubscribe! bus sub-id)
      ;; No overflow events
      (define overflow-events
        (filter (lambda (e) (string-contains? (event-ev e) "overflow")) recorded-events))
      (check-equal? overflow-events '()))))

(run-tests context-overflow-tests)
