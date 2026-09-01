#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;; tests/test-partial-result-preservation.rkt
;; W3 NR-4: Partial result preservation tests.
;;
;; Verifies:
;; 1. Partial text is captured from streaming errors via exn:fail:stream-error.
;; 2. Opt-in partial recovery feeds partial text as continuation context.
;; 3. Min-chars threshold prevents using tiny fragments.
;; 4. Partial text always visible in transcript regardless of setting.

(require rackunit
         racket/list
         racket/string
         "../util/exn.rkt"
         (only-in "../llm/stream.rkt"
                  exn:fail:network:timeout:stream
                  exn:fail:network:timeout:stream?)
         "../runtime/auto-retry.rkt"
         "../runtime/provider-retry.rkt"
         (only-in "../util/message/message.rkt" message message-role message-content make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/event/event-bus.rkt" make-event-bus subscribe!)
         (only-in "../util/message/protocol-types.rkt" event-event event-payload)
         (only-in "helpers/fast-fixtures.rkt" with-deterministic-retries))

;; ── Helpers ──────────────────────────────────────────────

(define (make-stream-timeout [partial-chars 500])
  (exn:fail:network:timeout:stream "Stream timeout"
                                   (current-continuation-marks)
                                   #t ; received-heartbeats?
                                   #t ; received-any-data?
                                   'content
                                   partial-chars))

(define base-ctx
  (list (make-message (generate-id) #f 'system 'message (list (make-text-part "system")) 0 (hasheq))
        (make-message (generate-id) #f 'user 'message (list (make-text-part "hello")) 0 (hasheq))))

(define base-settings (hasheq 'max-tokens 4096 'model "test-model"))

;; W2 deterministic re-tier: capture the auto-retry.start events the retry
;; chain emits on the bus so tests assert the LOGICAL computed backoff — the
;; exact delay sequence production scale 1.0 computes — instead of wall-clock
;; duration. Under the deterministic seam the sleep is skipped but every
;; reported delay, attempt count, and retry-exhausted field is unchanged.
(define (make-retry-event-bus)
  (define bus (make-event-bus))
  (define evts (box '()))
  (subscribe! bus
              (lambda (e)
                (when (equal? (event-event e) "auto-retry.start")
                  (set-box! evts (cons (event-payload e) (unbox evts))))))
  (values bus evts))

;; The production-scale delay computation for the retry chain used below
;; (call-with-provider-retry pins base-delay-ms 1000; max-delay-ms default
;; 60000; deterministic jitter source pinned to 1.0 = max backoff).
(define (expected-logical-delay attempt)
  (compute-retry-delay attempt 1000 60000 0 (lambda () 1.0)))

;; ── Test: exn:fail:stream-error carries partial text ─────

(test-case "exn:fail:stream-error carries partial text for recovery"
  ;; Simulate: stream-from-provider catches error, wraps with partial text, re-raises
  (define captured
    (with-handlers ([exn:fail:stream-error? (lambda (e) (exn:fail:stream-error-partial-text e))])
      (raise (exn:fail:stream-error "stream error"
                                    (current-continuation-marks)
                                    "500 chars of partial output..."
                                    '()
                                    (make-stream-timeout 500)))))
  (check-equal? captured "500 chars of partial output..."))

;; ── Test: opt-in partial recovery injects continuation context ──

(test-case "partial recovery injects continuation context on retry"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (define-values (retry-bus retry-delays) (make-retry-event-bus))

  (define result
    (with-deterministic-retries
      (lambda ()
        (call-with-provider-retry (lambda (ctx settings)
                                    (set-box! attempt (add1 (unbox attempt)))
                                    (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
                                    (if (= (unbox attempt) 1)
                                        ;; First attempt: wrap exception with partial text
                                        (raise (exn:fail:stream-error "stream timeout"
                                                                      (current-continuation-marks)
                                                                      "I was halfway through explaining"
                                                                      '()
                                                                      (make-stream-timeout 500)))
                                        ;; Second attempt: succeed
                                        'success))
                                  base-ctx
                                  base-settings
                                  retry-bus
                                  "test-session"
                                  "test-turn"
                                  300
                                  #:partial-recovery #t
                                  #:partial-recovery-min-chars 10))))

  (check-equal? result 'success)
  ;; Initial attempt (second in list) got original context
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx))
  ;; Retry attempt (first in list) got continuation prompt prepended (1 extra message)
  (check-equal? (length (first (unbox received-ctxs))) (add1 (length base-ctx)))
  ;; The continuation prompt contains partial text
  (define retry-ctx (first (unbox received-ctxs)))
  (define continuation-msg (first retry-ctx))
  (check-equal? (message-role continuation-msg) 'assistant)
  (define content-str (format "~a" (message-content continuation-msg)))
  (check-pred (lambda (s) (string-contains? s "I was halfway through explaining")) content-str)
  ;; W2: the retry reports the production-scale computed delay (deterministic
  ;; jitter 1.0 → base * 2^attempt) even though the wall-clock sleep is skipped.
  (define retry-evts (reverse (unbox retry-delays)))
  (check-equal? (length retry-evts) 1 "exactly one auto-retry.start event")
  (check-equal? (hash-ref (first retry-evts) 'attempt) 1)
  (check-equal? (hash-ref (first retry-evts) 'delayMs) (expected-logical-delay 0)))

;; ── Test: min-chars threshold prevents using tiny fragments ──

(test-case "partial recovery skips when partial text below threshold"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (define-values (retry-bus retry-delays) (make-retry-event-bus))

  (define result
    (with-deterministic-retries
      (lambda ()
        (call-with-provider-retry (lambda (ctx settings)
                                    (set-box! attempt (add1 (unbox attempt)))
                                    (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
                                    (if (= (unbox attempt) 1)
                                        ;; Short partial text (below threshold of 200)
                                        (raise (exn:fail:stream-error "stream timeout"
                                                                      (current-continuation-marks)
                                                                      "hi"
                                                                      '()
                                                                      (make-stream-timeout 2)))
                                        'success))
                                  base-ctx
                                  base-settings
                                  retry-bus
                                  "test-session"
                                  "test-turn-2"
                                  300
                                  #:partial-recovery #t
                                  #:partial-recovery-min-chars 200))))

  (check-equal? result 'success)
  ;; Retry context should NOT have continuation (below threshold)
  ;; Same length as original context
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx))
  ;; W2: a retry still happened and reported the production-scale delay.
  (define retry-evts (reverse (unbox retry-delays)))
  (check-equal? (length retry-evts) 1 "exactly one auto-retry.start event")
  (check-equal? (hash-ref (first retry-evts) 'delayMs) (expected-logical-delay 0)))

;; ── Test: partial recovery disabled by default ──

(test-case "partial recovery disabled by default - no continuation injection"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (define-values (retry-bus retry-delays) (make-retry-event-bus))

  (define result
    (with-deterministic-retries
      (lambda ()
        (call-with-provider-retry
         (lambda (ctx settings)
           (set-box! attempt (add1 (unbox attempt)))
           (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
           (if (= (unbox attempt) 1)
               (raise (exn:fail:stream-error "stream timeout"
                                             (current-continuation-marks)
                                             "Lots of partial output that should NOT be used for recovery"
                                             '()
                                             (make-stream-timeout 500)))
               'success))
         base-ctx
         base-settings
         retry-bus
         "test-session"
         "test-turn-3"
         300))))

  (check-equal? result 'success)
  ;; No continuation injection (partial-recovery defaults to #f)
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx))
  ;; W2: a retry still happened and reported the production-scale delay.
  (define retry-evts (reverse (unbox retry-delays)))
  (check-equal? (length retry-evts) 1 "exactly one auto-retry.start event")
  (check-equal? (hash-ref (first retry-evts) 'delayMs) (expected-logical-delay 0)))

;; ── Test: no partial text - no injection ──

(test-case "no partial text means no continuation injection"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (define-values (retry-bus retry-delays) (make-retry-event-bus))

  (define result
    (with-deterministic-retries
      (lambda ()
        (call-with-provider-retry (lambda (ctx settings)
                                    (set-box! attempt (add1 (unbox attempt)))
                                    (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
                                    (if (= (unbox attempt) 1)
                                        ;; No partial text — just raise the original
                                        (raise (make-stream-timeout 0))
                                        'success))
                                  base-ctx
                                  base-settings
                                  retry-bus
                                  "test-session"
                                  "test-turn-5"
                                  300
                                  #:partial-recovery #t
                                  #:partial-recovery-min-chars 10))))

  (check-equal? result 'success)
  ;; No injection (no partial text available)
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx))
  ;; W2: a retry still happened and reported the production-scale delay.
  (define retry-evts (reverse (unbox retry-delays)))
  (check-equal? (length retry-evts) 1 "exactly one auto-retry.start event")
  (check-equal? (hash-ref (first retry-evts) 'delayMs) (expected-logical-delay 0)))

;; ── Test: partial messages attached to exception for transcript flush ──

(test-case "partial messages attached to exception chain"
  ;; When retries are exhausted, the exception should carry partial messages
  ;; so session-lifecycle can flush them to session.jsonl
  (define partial-msgs (list (hasheq 'role 'assistant 'content "partial")))
  (define attempt (box 0))
  (define-values (retry-bus retry-delays) (make-retry-event-bus))
  (define caught-exn
    (with-deterministic-retries
      (lambda ()
        (with-handlers ([exn:fail? (lambda (e) e)])
          (call-with-provider-retry (lambda (ctx settings)
                                      (set-box! attempt (add1 (unbox attempt)))
                                      ;; Always fail with stream error carrying messages
                                      (raise (exn:fail:stream-error "persistent timeout"
                                                                    (current-continuation-marks)
                                                                    "partial text"
                                                                    partial-msgs
                                                                    (make-stream-timeout 500))))
                                    base-ctx
                                    base-settings
                                    retry-bus
                                    "test-session"
                                    "test-turn-6"
                                    10))))) ; very short ceiling

  ;; The final exception should be exn:fail:stream-error wrapping retry-exhausted
  (check-pred exn:fail:stream-error? caught-exn)
  ;; And should carry the partial messages
  (check-equal? (exn:fail:stream-error-partial-messages caught-exn) partial-msgs)
  ;; F5b: the deep unwrap must still find retry metadata through the partial wrap
  (define inner (find-retry-exhausted caught-exn))
  (check-not-false inner "retry metadata must survive partial recovery wrapping")
  (check-pred retry-exhausted? inner)
  ;; W2: retry attempts were recorded with real (non-symbolic) delays even
  ;; under the tiny 10 ms ceiling. No exact-delay assertion here because the
  ;; ceiling may truncate the production backoff; equality with the
  ;; production-scale computation is asserted in the ceiling-free cases above.
  (define retry-evts (reverse (unbox retry-delays)))
  (check-true (>= (length retry-evts) 1) "at least one retry event recorded")
  (for ([evt retry-evts])
    (check-pred real? (hash-ref evt 'delayMs)))
  ;; [kimi milestone W2] (#9394): interactive default raised 2 → 5, so the loop
  ;; retries up to 5 times. The metadata (attempts) must survive partial
  ;; wrapping; the exact count is NOT asserted because the explicit 10s
  ;; cumulative ceiling in this test may legitimately terminate the loop before
  ;; the 5th retry under slow/loaded CI runners (ceiling is enforced on retry
  ;; attempts). Asserting at least one attempt proves the metadata survived.
  (check-true (>= (retry-exhausted-attempts inner) 1)
              (format "retry metadata survived (attempts=~a)" (retry-exhausted-attempts inner))))
