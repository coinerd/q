#lang racket

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         "../extensions/github/helpers.rkt")

(define github-helpers-tests
  (test-suite "github helpers contracts"
    (test-case "input validators accept safe values and reject unsafe values"
      (check-true (valid-identifier? "feature_1.2-branch"))
      (check-false (valid-identifier? "bad branch"))
      (check-true (valid-number? 42))
      (check-true (valid-number? "42"))
      (check-false (valid-number? 0))
      (check-true (valid-state? "open"))
      (check-false (valid-state? "pending"))
      (check-true (valid-method? "squash"))
      (check-false (valid-method? "octopus")))

    (test-case "gh/git helper command contracts reject non-string rest args"
      (check-exn exn:fail:contract? (lambda () (gh-exec-result "issue" 123)))
      (check-exn exn:fail:contract? (lambda () (git-exec-result "status" 123))))

    (test-case "get-repo-info returns false values when gh is disabled"
      (parameterize ([gh-binary-path 'disabled])
        (define-values (owner repo) (get-repo-info))
        (check-false owner)
        (check-false repo)))

    (test-case "unavailable error returns a tool result"
      (check-not-false (gh-unavailable-error)))))

(module+ main
  (run-tests github-helpers-tests))
