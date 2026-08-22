#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit
;; @not-test true ;; v1.00.13 W0 (#9454): committed red — promoted in W4 (#9478)

;; tests/test-stream-liveness-classification.rkt
;; v1.00.13 (RL-8): heartbeat-aware held-request classification.
;;
;;   zero data + zero heartbeat + initial timeout = held/dead peer (unchanged)
;;   heartbeat-only initial stall = live-but-no-content, NOT a held request
;;   mid-stream stalls remain retryable regardless of heartbeat state
;;
;; Split from tests/test-network-failure-context.rkt in W3 so the structured
;; failure-context contract could go green independently (W3 #9473). The
;; heartbeat-aware classifier lands in W4 (#9478); today held-request?
;; ignores heartbeat metadata, so the middle assertion is red.

(require rackunit)

(define held-request? (dynamic-require '"../runtime/auto-retry.rkt" 'held-request?))

(define (stream-timeout heartbeats? data? phase chars)
  ((dynamic-require '"../llm/stream.rkt" 'exn:fail:network:timeout:stream)
   "HTTP read timeout"
   (current-continuation-marks)
   heartbeats?
   data?
   phase
   chars))

(test-case "zero-liveness initial stall remains a held request"
  (check-true (held-request? (stream-timeout #f #f 'initial 0))))

(test-case "heartbeat-only initial stall is live-but-no-content, not a held request"
  (check-false (held-request? (stream-timeout #t #f 'initial 0))
               "RL-8: heartbeat-only stream must not be classified as held request"))

(test-case "mid-stream stalls remain retryable regardless of heartbeat state"
  (check-false (held-request? (stream-timeout #f #t 'thinking 40)))
  (check-false (held-request? (stream-timeout #t #t 'content 900))))
