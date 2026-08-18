#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         "../extensions/github/helpers.rkt")

(define github-helpers-tests
  (test-suite "github helpers contracts"
    (test-case "input validators accept safe values and reject unsafe values"
      (check-true (valid-identifier? "feature_1.2-branch"))
      (check-false (valid-identifier? "bad branch"))
      (check-true (valid-number? 42))
      (check-true (valid-number? "42"))
      (check-false (valid-number? 0))
      (check-false (valid-number? "0"))
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
      (check-not-false (gh-unavailable-error)))

    (test-case "resolve-git-root supports an execution cwd above a q repository"
      (define outer (make-temporary-file "github-roots-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define repo (build-path outer "q"))
                      (make-directory* (build-path repo ".git"))
                      (make-directory* (build-path repo "extensions"))
                      ;; Directory-path flags may differ by platform/temp-path
                      ;; construction; path components are the stable identity.
                      (check-equal? (explode-path (resolve-git-root outer))
                                    (explode-path (simplify-path repo)))
                      (check-equal? (explode-path (resolve-git-root (build-path repo "extensions")))
                                    (explode-path (simplify-path repo))))
                    (lambda () (delete-directory/files outer))))

    (test-case "resolve-planning-root prefers canonical parent planning in parent/q layout"
      (define outer (make-temporary-file "github-planning-roots-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define repo (build-path outer "q"))
                      (define canonical (build-path outer ".planning"))
                      (make-directory* (build-path repo ".git"))
                      (make-directory* (build-path repo ".planning"))
                      (make-directory* canonical)
                      (check-equal? (resolve-planning-root repo) (simplify-path canonical))
                      (check-equal? (resolve-planning-root outer) (simplify-path canonical)))
                    (lambda () (delete-directory/files outer))))))

(module+ main
  (run-tests github-helpers-tests))
