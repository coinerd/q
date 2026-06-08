#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-llm-timing.rkt — Unit test for shared timing utility
;;
;; Verifies that log-stream-setup-timing accepts both exact and inexact
;; numbers (contract is real?, not exact-nonnegative-integer?).

(require rackunit
         rackunit/text-ui
         "../llm/timing.rkt")

(define timing-tests
  (test-suite "llm-timing"

    (test-case "log-stream-setup-timing accepts inexact milliseconds"
      ;; Must not raise contract error with inexact (real? includes inexact)
      (check-not-exn
       (lambda ()
         (log-stream-setup-timing "test-provider"
                                   (current-inexact-milliseconds)))))

    (test-case "log-stream-setup-timing accepts exact milliseconds"
      (check-not-exn
       (lambda ()
         (log-stream-setup-timing "test-provider" 1000))))

    (test-case "log-stream-setup-timing rejects non-real"
      (check-exn
       exn:fail:contract?
       (lambda ()
         (log-stream-setup-timing "test-provider" "not-a-number"))))

    (test-case "log-stream-setup-timing rejects non-string provider"
      (check-exn
       exn:fail:contract?
       (lambda ()
         (log-stream-setup-timing 42 1000))))))

(run-tests timing-tests)
