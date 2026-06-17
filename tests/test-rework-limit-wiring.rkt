#lang racket/base

;; tests/test-rework-limit-wiring.rkt
;; v0.99.21 W0 (F-2): Verify that verifier-max-rework-iterations setting
;; actually controls the gsd-max-rework-iterations parameter.

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/state-machine.rkt"
         (prefix-in settings: "../runtime/settings-query.rkt")
         "../runtime/settings-core.rkt")

(define (make-test-settings config-hash)
  (q-settings (hash) config-hash config-hash))

(define suite
  (test-suite "Rework-Limit Wiring (v0.99.21 W0 F-2)"

    (test-case "gsd-max-rework-iterations is a parameter"
      (check-true (procedure? gsd-max-rework-iterations))
      ;; Verify it has a default value
      (check-true (exact-positive-integer? (gsd-max-rework-iterations))))

    (test-case "verifier-max-rework-iterations reads from settings"
      ;; Default is 3
      (define s1 (make-test-settings (hasheq)))
      (check-equal? (settings:verifier-max-rework-iterations s1) 3)

      ;; Custom value
      (define s2 (make-test-settings
                  (hasheq 'mas
                          (hasheq 'verifier
                                  (hasheq 'max-rework-iterations 7)))))
      (check-equal? (settings:verifier-max-rework-iterations s2) 7))

    (test-case "verifier-max-rework-iterations coerces string values"
      (define s (make-test-settings
                 (hasheq 'mas
                         (hasheq 'verifier
                                 (hasheq 'max-rework-iterations "5")))))
      (check-equal? (settings:verifier-max-rework-iterations s) 5))

    (test-case "parameter can be set from settings value"
      ;; Simulate the wiring that run-modes.rkt does
      (define s (make-test-settings
                 (hasheq 'mas
                         (hasheq 'verifier
                                 (hasheq 'max-rework-iterations 10)))))
      (define old-val (gsd-max-rework-iterations))
      (gsd-max-rework-iterations (settings:verifier-max-rework-iterations s))
      (check-equal? (gsd-max-rework-iterations) 10)
      ;; Restore
      (gsd-max-rework-iterations old-val))))


(run-tests suite)
