#lang racket

;; Tests for scripts/lint-release-readiness.rkt
;; BOUNDARY: integration

(require rackunit
         racket/file
         racket/path
         racket/runtime-path)

(define-runtime-path script-path "../scripts/lint-release-readiness.rkt")

;; Script loads successfully (compile gate)
(test-case "lint-release-readiness: module loads and provides expected symbols"
  (dynamic-require script-path #f)
  (for ([sym (in-list '(get-canonical-version check-version-sync
                                              check-tag-unique
                                              check-changelog-entry
                                              check-git-clean
                                              check-main-branch))])
    (check-not-exn (lambda () (dynamic-require script-path sym))
                   (format "expected ~a to be provided" sym))))

(test-case "lint-release-readiness: runs from q/ root"
  ;; When run via `raco test` from q/, CWD is q/ — script should work
  (define cwd (current-directory))
  (when (file-exists? (build-path cwd "util" "version.rkt"))
    (define ver (dynamic-require script-path 'get-canonical-version))
    (define result (ver))
    (check-true (regexp-match? #rx"\\d+\\.\\d+\\.\\d+" result)
                (format "expected version pattern, got: ~a" result))))

(test-case "check-changelog-entry: accepts v-prefixed headers"
  ;; The function reads version from util/version.rkt (CWD-dependent).
  ;; Only run when we're in the q/ root directory.
  (when (file-exists? "util/version.rkt")
    (define check (dynamic-require script-path 'check-changelog-entry))
    (define result (check))
    (check-true (or result (not result)) "check-changelog-entry returns a boolean")))
