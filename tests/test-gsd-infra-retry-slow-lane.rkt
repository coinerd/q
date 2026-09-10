#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-gsd-infra-retry-slow-lane.rkt — BUG-0067 pure seams.
;;
;; The v1.00.29 launch-day incidents (campaign 6ae3ac51): three api.z.ai
;; bursts in one afternoon, two of which exhausted the fast budget
;; (3 × 30/60/120s ≈ 3.5 min of patience) and stopped the campaign for a
;; transient-class failure — each stop costing a manual /retry. The slow
;; lane keeps re-attempting with backoff that keeps DOUBLING past the fast
;; lane's 120s cap (240/480/900/900…, capped at the max-delay parameter)
;; until a total patience horizon is consumed. The attempt is never
;; consumed either way; patience 0 restores the pre-BUG-0067
;; fail-closed stop verbatim.
;;
;; This file pins the PURE seams (backoff shape, patience accounting,
;; terminal messages, telemetry phase, settings resolver). The
;; orchestrator-level integration lives in
;; tests/test-gsd-campaign-infra-retry.rkt; the on-disk settings wiring in
;; tests/test-campaign-infra-retry-config.rkt.

(require rackunit
         racket/list
         racket/string
         (only-in "../extensions/gsd/infra-retry-policy.rkt"
                  infra-slow-lane-backoff-secs
                  infra-patience-consume!
                  infra-patience-exhausted-message
                  infra-retry-exhausted-message
                  resolve-effective-infra-retry-policy
                  emit-infra-retry-event!)
         (only-in "../extensions/gsd/policy.rkt"
                  current-gsd-campaign-infra-retries
                  current-gsd-campaign-infra-max-delay
                  current-gsd-campaign-infra-slow-delay
                  current-gsd-campaign-infra-wait-chunk-secs)
         (only-in "../extensions/gsd/events.rkt"
                  make-event-collector
                  collector-events
                  set-gsd-event-bus!))

;; ============================================================
;; Slow-lane backoff shape: doubling continuation, max-delay cap
;; ============================================================

(test-case "slow-lane backoff keeps doubling past the fast lane's 120s cap"
  (parameterize ([current-gsd-campaign-infra-max-delay 900])
    ;; attempt-index continues the fast lane's numbering: a fast budget
    ;; of 3 means the first slow-lane wait is index 4.
    (check-equal? (infra-slow-lane-backoff-secs 4) 240)
    (check-equal? (infra-slow-lane-backoff-secs 5) 480)
    ;; 30·2^5 = 960 caps at the max-delay default of 900.
    (check-equal? (infra-slow-lane-backoff-secs 6) 900)
    (check-equal? (infra-slow-lane-backoff-secs 7) 900)))

(test-case "slow-lane backoff honors the max-delay parameter"
  (parameterize ([current-gsd-campaign-infra-max-delay 100])
    (check-equal? (infra-slow-lane-backoff-secs 4) 100)
    (check-equal? (infra-slow-lane-backoff-secs 9) 100)))

(test-case "slow-lane backoff shape is operator-injectable (test seam)"
  (parameterize ([current-gsd-campaign-infra-slow-delay (lambda (_idx) 1)])
    (check-equal? (infra-slow-lane-backoff-secs 4) 1)
    (check-equal? (infra-slow-lane-backoff-secs 99) 1)))

(test-case "wait-chunk default is 15s and rejects non-positive chunks"
  (check-equal? (current-gsd-campaign-infra-wait-chunk-secs) 15)
  (check-exn exn:fail:contract?
             (lambda () (current-gsd-campaign-infra-wait-chunk-secs 0))
             "a 0s chunk would spin the cancellation loop forever"))

;; ============================================================
;; Patience accounting
;; ============================================================

(test-case "patience consumption decrements and floors at zero"
  (define patience-box (box 100))
  (check-equal? (infra-patience-consume! patience-box 30) 70)
  (check-equal? (infra-patience-consume! patience-box 30) 40)
  ;; A backoff larger than what remains floors at 0 — never negative.
  (check-equal? (infra-patience-consume! patience-box 999) 0)
  (check-equal? (unbox patience-box) 0))

;; ============================================================
;; Terminal messages
;; ============================================================

(define (ledger-state)
  (box (cons 3 '((1 1700000000) (2 1700000005)))))

(test-case "legacy fast-budget exhaustion message is byte-for-byte preserved"
  (define msg
    (parameterize ([current-gsd-campaign-infra-retries 3])
      (infra-retry-exhausted-message (ledger-state))))
  (check-true (string-contains? msg "provider/network failure persisted after 3 automatic retries"))
  (check-true (string-contains? msg "(attempt not consumed)"))
  (check-true (string-contains? msg "re-run /go when the provider is healthy"))
  (check-true (string-contains? msg "Failures:"))
  (check-true (string-contains? msg "attempt 1 at 1700000000")))

(test-case "patience-exhausted message keeps the legacy prefix and adds slow-lane accounting"
  (define msg
    (parameterize ([current-gsd-campaign-infra-retries 3])
      (infra-patience-exhausted-message (ledger-state) 4 2400 7200)))
  (check-true (string-contains? msg "provider/network failure persisted after 3 automatic")
              "legacy prefix survives so existing tooling/logs stay greppable")
  (check-true (string-contains? msg "+ 4 patient slow-lane retries"))
  (check-true (string-contains? msg "2400s backoff within the 7200s patience horizon"))
  (check-true (string-contains? msg "attempt not consumed"))
  (check-true (string-contains? msg "re-run /go when the provider is healthy"))
  (check-true (string-contains? msg "Failures:"))
  (check-true (string-contains? msg "attempt 2 at 1700000005")))

;; ============================================================
;; Telemetry: phase distinguishes fast retries from slow-lane waits
;; ============================================================

(test-case "infra-retry events carry a phase marker (fast default, slow opt-in)"
  (define-values (collect! query) (make-event-collector))
  (set-gsd-event-bus! collect!)
  (emit-infra-retry-event! 0 1 30)
  (emit-infra-retry-event! 0 1 240 #:phase 'slow)
  (define evts (collector-events query))
  (check-equal? (length evts) 2)
  (define data1 (hash-ref (first evts) 'data))
  (define data2 (hash-ref (second evts) 'data))
  (check-equal? (hash-ref data1 'phase) 'fast)
  (check-equal? (hash-ref data1 'delay) 30)
  (check-equal? (hash-ref data2 'phase) 'slow)
  (check-equal? (hash-ref data2 'delay) 240)
  (set-gsd-event-bus! void))

;; ============================================================
;; Settings resolver (composition root): precedence + defaults
;; ============================================================

(test-case "resolver falls back to canonical parameter defaults without settings"
  (define-values (retries patience max-delay) (resolve-effective-infra-retry-policy #f))
  (check-equal? retries 3)
  (check-equal? patience 7200)
  (check-equal? max-delay 900))

(test-case "keyword overrides win over defaults (run-campaign-wave test seam)"
  (define-values (retries patience max-delay)
    (resolve-effective-infra-retry-policy #f #:retries 0 #:patience 0 #:max-delay 5))
  (check-equal? retries 0)
  (check-equal? patience 0)
  (check-equal? max-delay 5))
