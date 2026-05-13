#lang racket

;; BOUNDARY: contract

;; tests/test-contract-hardening.rkt -- W0: contract hardening tests
;;
;; Tests S4-F1: contract-out on main.rkt SDK functions

(require rackunit
         rackunit/text-ui
         "../main.rkt")

(define contract-suite
  (test-suite "contract hardening"

    ;; -- make-agent-session rejects non-hash/non-config --
    (test-case "make-agent-session rejects string argument"
      (check-exn exn:fail:contract?
                 (lambda () (make-agent-session "bad"))))

    ;; -- close-session! rejects non-agent-session --
    (test-case "close-session! rejects string argument"
      (check-exn exn:fail:contract?
                 (lambda () (close-session! "bad"))))

    ;; -- resume-agent-session rejects non-string session-id --
    (test-case "resume-agent-session rejects number session-id"
      (check-exn exn:fail:contract?
                 (lambda () (resume-agent-session 42 #f))))

    ;; -- fork-session rejects non-agent-session --
    (test-case "fork-session rejects string argument"
      (check-exn exn:fail:contract?
                 (lambda () (fork-session "bad"))))))

(run-tests contract-suite 'verbose)
