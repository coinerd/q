#lang racket

;;; test-changelog-bug-ref-lint.rkt — W0 characterization pin for
;;; BUG-0049: `lint-release-notes` performs NO cross-check between
;;; `BUG-NNNN` tokens cited in a CHANGELOG entry and the bug registry
;;; (.planning/bugs/). A changelog entry citing a completely
;;; non-existent bug (BUG-9999) still passes the lint with zero
;;; errors.
;;;
;;; Flip owner: W5 (changelog bug-ref lint). When W5 adds the
;;; registry cross-check, this pin must be flipped into a
;;; fix-regression test asserting the phantom BUG-9999 reference is
;;; REPORTED as an error.

(require rackunit
         racket/file
         racket/path
         racket/string
         (file "../scripts/lint-release-notes.rkt"))

(define repo-root
  (simplify-path (build-path (find-system-path 'run-file) 'up 'up)))

;; --- Pin 1 (source level): the lint implementation has no registry
;;; cross-check seam today — the linter source never references the
;;; bug registry directory.
(define linter-source
  (file->string (build-path repo-root "scripts" "lint-release-notes.rkt")))

(check-false
 (regexp-match? #px"(?i:bugs/|registry|BUG-\\{?0?\\}?)" linter-source)
 "lint-release-notes.rkt contains no bug-registry cross-check (absent seam)")

;; --- Pin 2 (behavioral): a fully-compliant v9.9.9 entry that cites a
;;; nonexistent BUG-9999 token passes the lint with zero errors.
(define tmp-dir (make-temporary-file "bug0049-tmp~a" 'directory))
(define changelog-path (build-path tmp-dir "CHANGELOG.md"))

(define (cleanup!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp-dir)))

(with-handlers ([exn:fail? (lambda (e) (cleanup!) (raise e))])

  (call-with-output-file #:exists 'truncate
    changelog-path
    (lambda (out)
      (display
       (string-append
        "# Changelog\n\n"
        "## v9.9.9\n\n"
        "### User-Visible Changes\n"
        "- Fixed the frobnicator reported in BUG-9999 (registry-phantom id).\n\n"
        "### Breaking / Behavior Changes\n"
        "- None.\n\n"
        "### Migration Notes\n"
        "- None required.\n\n"
        "### Testing\n"
        "- racket tests/test-changelog-bug-ref-lint.rkt\n\n"
        "### Operational / Release\n"
        "- Tag v9.9.9.\n")
       out)))

  (define errors (lint-changelog changelog-path "9.9.9"))
  (check-equal? errors '()
                "BUG-9999 (nonexistent in the registry) passes lint-changelog — no cross-check exists")

  (cleanup!))

(displayln "PASS test-changelog-bug-ref-lint (BUG-0049 pin: phantom BUG-9999 reference accepted)")
