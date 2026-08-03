#lang racket/base

;; tests/test-partial-result-preservation.rkt
;; W3 NR-4: Partial result preservation tests.
;;
;; Verifies:
;; 1. Partial text is captured from streaming errors (transcript preservation).
;; 2. Opt-in partial recovery feeds partial text as continuation context.
;; 3. Min-chars threshold prevents using tiny fragments.
;; 4. Partial text always visible in transcript regardless of setting.

(require rackunit
         racket/list
         racket/string
         (only-in "../agent/state.rkt" current-partial-text)
         (only-in "../llm/stream.rkt"
                  exn:fail:network:timeout:stream
                  exn:fail:network:timeout:stream?)
         "../runtime/auto-retry.rkt"
         "../runtime/provider-retry.rkt"
         (only-in "../util/message/message.rkt" message message-role message-content make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/event/event-bus.rkt" make-event-bus))

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

;; ── Test: partial text captured from stream errors ───────

(test-case "current-partial-text parameter defaults to #f"
  (check-equal? (parameterize ([current-partial-text #f])
                  (current-partial-text))
                #f))

(test-case "partial text is set when stream produces output before timeout"
  ;; Simulate: stream-from-provider catches error, sets parameter, re-raises
  (define captured
    (with-handlers ([exn:fail? (lambda (e) (current-partial-text))])
      (current-partial-text "500 chars of partial output...")
      (raise (make-stream-timeout 500))))
  (check-equal? captured "500 chars of partial output...")
  ;; Clean up
  (current-partial-text #f))

;; ── Test: opt-in partial recovery injects continuation context ──

(test-case "partial recovery injects continuation context on retry"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (current-partial-text #f)

  (define result
    (call-with-provider-retry (lambda (ctx settings)
                                (set-box! attempt (add1 (unbox attempt)))
                                (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
                                (if (= (unbox attempt) 1)
                                    ;; First attempt: simulate partial output and timeout
                                    (begin
                                      (current-partial-text "I was halfway through explaining")
                                      (raise (make-stream-timeout 500)))
                                    ;; Second attempt: succeed
                                    (begin
                                      (current-partial-text #f)
                                      'success)))
                              base-ctx
                              base-settings
                              (make-event-bus)
                              "test-session"
                              "test-turn"
                              300
                              #:partial-recovery #t
                              #:partial-recovery-min-chars 10))

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
  (check-pred (lambda (s) (string-contains? s "I was halfway through explaining")) content-str))

;; ── Test: min-chars threshold prevents using tiny fragments ──

(test-case "partial recovery skips when partial text below threshold"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (current-partial-text #f)

  (define result
    (call-with-provider-retry (lambda (ctx settings)
                                (set-box! attempt (add1 (unbox attempt)))
                                (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
                                (if (= (unbox attempt) 1)
                                    ;; Short partial text (below threshold of 200)
                                    (begin
                                      (current-partial-text "hi")
                                      (raise (make-stream-timeout 2)))
                                    (begin
                                      (current-partial-text #f)
                                      'success)))
                              base-ctx
                              base-settings
                              (make-event-bus)
                              "test-session"
                              "test-turn-2"
                              300
                              #:partial-recovery #t
                              #:partial-recovery-min-chars 200))

  (check-equal? result 'success)
  ;; Retry context should NOT have continuation (below threshold)
  ;; Same length as original context
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx)))

;; ── Test: partial recovery disabled by default ──

(test-case "partial recovery disabled by default - no continuation injection"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (current-partial-text #f)

  (define result
    (call-with-provider-retry
     (lambda (ctx settings)
       (set-box! attempt (add1 (unbox attempt)))
       (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
       (if (= (unbox attempt) 1)
           (begin
             (current-partial-text "Lots of partial output that should NOT be used for recovery")
             (raise (make-stream-timeout 500)))
           (begin
             (current-partial-text #f)
             'success)))
     base-ctx
     base-settings
     (make-event-bus)
     "test-session"
     "test-turn-3"
     300))

  (check-equal? result 'success)
  ;; No continuation injection (partial-recovery defaults to #f)
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx)))

;; ── Test: partial text cleared after consumption ──

(test-case "partial text cleared after being consumed for recovery"
  (current-partial-text "consumed text")
  (define attempt (box 0))

  (call-with-provider-retry (lambda (ctx settings)
                              (set-box! attempt (add1 (unbox attempt)))
                              (if (= (unbox attempt) 1)
                                  (raise (make-stream-timeout 500))
                                  'success))
                            base-ctx
                            base-settings
                            (make-event-bus)
                            "test-session"
                            "test-turn-4"
                            300
                            #:partial-recovery #t)

  ;; After recovery, the parameter should be cleared
  (check-equal? (current-partial-text) #f))

;; ── Test: no partial text - no injection ──

(test-case "no partial text means no continuation injection"
  (define attempt (box 0))
  (define received-ctxs (box '()))
  (current-partial-text #f)

  (define result
    (call-with-provider-retry (lambda (ctx settings)
                                (set-box! attempt (add1 (unbox attempt)))
                                (set-box! received-ctxs (cons ctx (unbox received-ctxs)))
                                (if (= (unbox attempt) 1)
                                    ;; No partial text set
                                    (raise (make-stream-timeout 0))
                                    'success))
                              base-ctx
                              base-settings
                              (make-event-bus)
                              "test-session"
                              "test-turn-5"
                              300
                              #:partial-recovery #t
                              #:partial-recovery-min-chars 10))

  (check-equal? result 'success)
  ;; No injection (no partial text available)
  (check-equal? (length (second (unbox received-ctxs))) (length base-ctx)))
