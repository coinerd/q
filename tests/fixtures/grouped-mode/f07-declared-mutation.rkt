#lang racket/base

;; W7 grouped-mode characterization fixture: DECLARED MUTATION.
;; Declares @mutates env,cwd and really does mutate them when its tests run
;; — but grouped never reaches the test body: eligibility is checked first,
;; so a grouped request must fall back to subprocess with the stable reason
;; 'declared-mutation.

;; @speed fast
;; @suite testing
;; @mutates env,cwd

(require rackunit
         rackunit/text-ui)

(define f07-suite
  (test-suite "f07-declared-mutation"
    (test-case "mutates-env"
      (putenv "F07_DECLARED_MUTATION" "1")
      (check-true (string? (getenv "F07_DECLARED_MUTATION"))))
    (test-case "mutates-cwd"
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (check-true (directory-exists? (current-directory)))))))

(module+ test
  (run-tests f07-suite))
