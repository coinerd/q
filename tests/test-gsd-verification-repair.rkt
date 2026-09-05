#lang racket/base
;; @covers extensions/gsd/verification-repair.rkt
;; @speed fast  ;; @suite extensions
;; @boundary unit

;; tests/test-gsd-verification-repair.rkt — BUG-0060 unit coverage for the
;; bounded same-wave verification repair policy (classification + progress
;; guard). The durable retry transition is covered end-to-end by
;; tests/test-gsd-go-orchestrator.rkt.

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/verification-repair.rkt"
         "../extensions/gsd/delivery-verifier.rkt")

(define (verify-failure-evidence detail)
  (list (cons "verify" (cons #f detail))))

(define (non-verify-evidence detail)
  (list (cons "git-diff" (cons #f detail))))

(define verify-failure
  (delivery-verification
   #f
   (verify-failure-evidence "cmd=racket test.rkt exit=1 state=failed log=/tmp/v.log")
   "delivery verification failed:\nverify: cmd=racket test.rkt exit=1 state=failed log=/tmp/v.log"))

(define verification-repair-unit-suite
  (test-suite "verification-repair (BUG-0060)"

    (test-case "repairable classification: owned verify command failure is repairable"
      (check-true (repairable-verification-rejection? verify-failure)))

    (test-case "repairable classification: boolean verifier rejection is NOT repairable"
      (check-false (repairable-verification-rejection? #f))
      (check-false (repairable-verification-rejection? #t)))

    (test-case "repairable classification: approved verification is NOT repairable"
      (check-false (repairable-verification-rejection? (delivery-verification #t '() "approved"))))

    (test-case "repairable classification: non-verify evidence failures are NOT repairable"
      (check-false (repairable-verification-rejection?
                    (delivery-verification #f
                                           (non-verify-evidence "cmd=git exit=1 log=/tmp/v.log")
                                           "delivery verification failed"))))

    (test-case "repairable classification: verify evidence without the cmd/exit/log contract is NOT repairable"
      (check-false
       (repairable-verification-rejection?
        (delivery-verification #f (verify-failure-evidence "no structured detail") "vague failure"))))

    (test-case "no-progress guard: identical heads block a second repair"
      (check-true (verification-repair-no-progress? "abc123" "abc123")))

    (test-case "no-progress guard: progress or missing heads allow repair"
      (check-false (verification-repair-no-progress? "abc123" "def456"))
      (check-false (verification-repair-no-progress? #f "abc123"))
      (check-false (verification-repair-no-progress? "abc123" #f)))))

(void (run-tests verification-repair-unit-suite))
