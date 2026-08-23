#lang racket/base

;; W1 NR-1: Mid-Stream Stall Classification & Progressive Circuit Breaker
;; Tests for output-chars metadata, minimal-output-stall? predicate,
;; stall-severity classification, and progressive circuit breaker behavior.

(require rackunit
         "../runtime/auto-retry.rkt"
         "../llm/stream.rkt")

;; Helper: construct a stream timeout exception with output-chars
;; @speed fast  ;; @suite default
;; @boundary unit
(define (stream-timeout-exn #:heartbeats? [hb? #f]
                            #:any-data? [any? #t]
                            #:phase [phase 'content]
                            #:output-chars [chars 0])
  (exn:fail:network:timeout:stream "Stream timeout"
                                   (current-continuation-marks)
                                   hb?
                                   any?
                                   phase
                                   chars))

(test-case "exn:fail:network:timeout:stream carries output-chars field"
  (define e (stream-timeout-exn #:output-chars 250))
  (check-equal? (exn:fail:network:timeout:stream-output-chars e) 250))

(test-case "minimal-output-stall? classifies < threshold chars as stall"
  (check-true (minimal-output-stall? (stream-timeout-exn #:output-chars 50)))
  (check-true (minimal-output-stall? (stream-timeout-exn #:output-chars 99)))
  (check-false (minimal-output-stall? (stream-timeout-exn #:output-chars 100)))
  (check-false (minimal-output-stall? (stream-timeout-exn #:output-chars 500))))

(test-case "held request (zero data) is not minimal-output-stall"
  (check-false (minimal-output-stall? (stream-timeout-exn #:any-data? #f #:phase 'initial))))

(test-case "stall-severity returns correct classification"
  (check-equal? (stall-severity (stream-timeout-exn #:any-data? #f #:phase 'initial)) 'initial-hold)
  (check-equal? (stall-severity (stream-timeout-exn #:output-chars 50)) 'minimal-output)
  (check-equal? (stall-severity (stream-timeout-exn #:output-chars 500)) 'partial-output))

(test-case "progressive breaker: 2 consecutive minimal-output stalls → circuit-break"
  (define attempt (box 0))
  (define circuit-breaks (box 0))
  (define retries (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (stream-timeout-exn #:output-chars 10)))
                                #:max-retries 3
                                #:base-delay-ms 1
                                #:stall-max-consecutive 2
                                #:stall-min-output-chars 100
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries))))
                                #:on-circuit-break
                                (lambda (_ exn)
                                  (set-box! circuit-breaks (add1 (unbox circuit-breaks)))))))
  ;; First stall retries, second consecutive triggers progressive break
  (check-equal? (unbox attempt) 2)
  (check-equal? (unbox retries) 1)
  (check-equal? (unbox circuit-breaks) 1))

(test-case "progressive breaker: partial-output stall gets full retry budget"
  (define attempt (box 0))
  (define retries (box 0))
  (define circuit-breaks (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (stream-timeout-exn #:output-chars 500)))
                                #:max-retries 2
                                #:base-delay-ms 1
                                #:stall-max-consecutive 2
                                #:stall-min-output-chars 100
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries))))
                                #:on-circuit-break
                                (lambda (_ exn)
                                  (set-box! circuit-breaks (add1 (unbox circuit-breaks)))))))
  ;; 500 chars is partial-output → full retry, no progressive break
  (check-equal? (unbox attempt) 3)
  (check-equal? (unbox retries) 2)
  (check-equal? (unbox circuit-breaks) 0))

(test-case "progressive breaker: non-stall error resets consecutive counter"
  (define attempt (box 0))
  (define retries (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry
                (lambda ()
                  (set-box! attempt (add1 (unbox attempt)))
                  (cond
                    [(= (unbox attempt) 1)
                     ;; First: rate-limit error (not a stall)
                     (raise (exn:fail "429 Too Many Requests" (current-continuation-marks)))]
                    ;; Then minimal-output stalls
                    [(<= (unbox attempt) 4) (raise (stream-timeout-exn #:output-chars 10))]))
                #:max-retries 5
                #:base-delay-ms 1
                #:stall-max-consecutive 2
                #:stall-min-output-chars 100
                #:on-retry (lambda args (set-box! retries (add1 (unbox retries)))))))
  ;; Attempt 1: rate-limit → retry (reset stall counter)
  ;; Attempt 2: minimal stall → retry (stall counter = 1)
  ;; Attempt 3: minimal stall → progressive break (stall counter = 2)
  (check-equal? (unbox attempt) 3)
  (check-equal? (unbox retries) 2))

;; ============================================================
;; BUG-0018 W1: keepalive liveness
;; ============================================================
;; The phase timeouts are per-read windows: every received line — including
;; SSE comment heartbeats (`: ...`) and zero-delta data chunks — completes a
;; read and resets the idle clock. A socket that is demonstrably alive is
;; therefore never killed by the thinking/content idle window, even when the
;; window is far shorter than the elapsed wall time; only true silence (no
;; bytes at all for a whole window) or the total-duration budget raises.

(require (only-in "../llm/stream.rkt" sse-comment-line? exn:fail:network:timeout:stream-phase)
         racket/port)

;; @speed fast  ;; @suite default
;; @boundary unit
(test-case "BUG-0018: heartbeat comment lines are classified as SSE comments"
  (check-true (sse-comment-line? ": keepalive"))
  (check-true (sse-comment-line? ":"))
  (check-false (sse-comment-line? "data: {}")))

;; A writer thread emitting one heartbeat per 20 ms — each individual gap is
;; under the 50 ms thinking window even though aggregate wall time far exceeds
;; it, so no idle raise may occur.
(test-case "BUG-0018: heartbeat-only stream survives past the idle window"
  (define-values (in out) (make-pipe))
  (define writer
    (thread (lambda ()
              (let loop ([i 0])
                (when (< i 40) ; ~800 ms total, 16x the 50 ms window
                  (fprintf out ": hb-~a\n\n" i)
                  (flush-output out)
                  (sleep 0.02)
                  (loop (add1 i))))
              (fprintf out "data: [DONE]\n\n")
              (close-output-port out))))
  (define gen
    (stream-sse-events in
                       (lambda (_parsed) '())
                       #:initial-timeout 0.05
                       #:stream-timeout 0.05
                       #:thinking-timeout 0.05
                       #:max-total-timeout 10))
  ;; Draining to completion without an idle-timeout raise is the assertion.
  (define saw-end?
    (with-handlers ([exn:fail:network:timeout:stream? (lambda (_e) #f)])
      (let loop ()
        (define v (gen))
        (if v
            (loop)
            #t))))
  (check-true saw-end? "heartbeat-only stream was killed by the idle window"))

(test-case "BUG-0018: true silence still raises within the thinking window"
  (define-values (in _out) (make-pipe))
  (define raised? #f)
  (define gen
    (stream-sse-events in
                       (lambda (_parsed) '())
                       #:initial-timeout 0.05
                       #:stream-timeout 0.05
                       #:thinking-timeout 0.05
                       #:max-total-timeout 10))
  (with-handlers ([exn:fail:network:timeout:stream?
                   (lambda (e)
                     (set! raised? #t)
                     (check-equal? (exn:fail:network:timeout:stream-phase e) 'initial))])
    (gen))
  (check-true raised? "silent stream did not raise the idle timeout"))
