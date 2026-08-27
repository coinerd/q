#lang racket/base

;; BUG-0022 repro: provider health gate must not count a turn's own retries
;; against itself. Before the fix, a default health tracker (window 60s,
;; threshold 3) truncated a 5-retry budget to exactly 2 same-turn retries
;; (min(max-retries, threshold-1)) for any fast-repeating error.
;;
;; Reproduction recipe from BUG-0022: drive the auto-retry loop with the
;; same wiring shape runtime/provider-retry.rkt uses (health gate closed
;; over a tracker); assert the FULL configured budget is consumed within
;; one turn, and that cross-turn counting still trips the gate on a
;; subsequent turn.
;;
;; Run: racket tests/test-bug-0022-health-gate-truncation.rkt

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/provider-health.rkt"
                  make-provider-health
                  record-failure!
                  provider-healthy?
                  provider-health-recent-failures
                  default-health-window-secs
                  default-health-failure-threshold)
         (only-in "../runtime/auto-retry.rkt"
                  with-auto-retry
                  retry-exhausted?
                  current-auto-retry-sleep-scale))

;; Fixture network exception — mirrors the outer message the session layer
;; raises for the BUG-0022 layer-1 conn-pool trigger ("Network error
;; contacting …" is what the retry classifier's string fallback keys on).
(define (make-net-exn)
  (exn:fail:network
   "Network error contacting api.z.ai: llm/conn-pool: no response status line from peer (after 2 retries)"
   (current-continuation-marks)))

;; The turn wiring under test, mirroring provider-retry.rkt's
;; health-check-proc with the FIXED same-turn semantics: record only the
;; turn-initial failure (attempt 0); retries stay bounded by max-retries,
;; per-type budgets, the cumulative ceiling, and the held/stall breakers.
(define (run-turn! health thunk-fails? #:max-retries [max-retries 5])
  (define attempts 0)
  (define retries 0)
  (define outcome
    (with-handlers ([retry-exhausted? (lambda (e) 'exhausted)]
                    ;; Gate denial at attempt 0 re-raises the ORIGINAL
                    ;; exception (auto-retry's [(> attempt 0)] branch).
                    [exn:fail? (lambda (e) 'gate-denied)])
      (parameterize ([current-auto-retry-sleep-scale 0.0])
        (with-auto-retry
         (lambda ()
           (set! attempts (add1 attempts))
           (when thunk-fails?
             (raise (make-net-exn)))
           'ok)
         #:max-retries max-retries
         ;; Mirrors the v1.00.05 W2 (#9394) budget scaling at
         ;; provider-retry's real call site — the DEFAULT
         ;; per-type budgets pin 'timeout to 2, which is the
         ;; same truncation class this test guards against.
         #:per-type-budgets
         (hash 'timeout max-retries 'rate-limit (max 4 max-retries) 'provider-error max-retries)
         #:base-delay-ms 1
         #:health-check-proc (lambda (exn attempt)
                               (when (= attempt 0)
                                 (record-failure! health))
                               (provider-healthy? health
                                                  #:window-secs default-health-window-secs
                                                  #:threshold default-health-failure-threshold))
         #:on-retry (lambda (attempt max-retries delay-ms msg err-type)
                      (set! retries (add1 retries)))))))
  (list outcome attempts retries))

(define bug-0022-suite
  (test-suite "bug-0022"
    (test-begin
      "same-turn retries are not counted by the health gate"
      ;; Healthy tracker + one failing turn → the FULL 5-retry budget runs.
      (define health (make-provider-health))
      (define r (run-turn! health #t))
      (check-equal? (list-ref r 0) 'exhausted "turn exhausts the full budget")
      (check-equal? (list-ref r 1) 6 "1 initial attempt + 5 retries = 6 calls")
      (check-equal? (list-ref r 2) 5 "all 5 on-retry events fired — budget NOT truncated to 2")
      ;; The tracker saw exactly ONE failure (attempt 0), not one per retry.
      (check-equal? (length (provider-health-recent-failures health))
                    1
                    "only the turn-initial failure is recorded"))
    (test-begin
      "cross-turn protection still works"
      ;; Three prior TURN-INITIAL failures in the window → the gate denies the
      ;; next turn's retries at attempt 0 (a genuinely sick provider gets no
      ;; same-turn budget either).
      (define health (make-provider-health))
      (record-failure! health)
      (record-failure! health)
      (record-failure! health)
      (check-false (provider-healthy? health
                                      #:window-secs default-health-window-secs
                                      #:threshold default-health-failure-threshold))
      (define r (run-turn! health #t))
      (check-equal? (list-ref r 0) 'gate-denied "gate denies retries for a sick provider")
      (check-equal? (list-ref r 1) 1 "only the initial attempt runs")
      (check-equal? (list-ref r 2) 0 "no on-retry events when gate denies"))))

(module+ main
  (void (run-tests bug-0022-suite)))
