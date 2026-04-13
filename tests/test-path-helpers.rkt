#lang racket

(require rackunit
         "../util/path-helpers.rkt")

;; ============================================================
;; Test suite: util/path-helpers.rkt — path-only
;; ============================================================

(test-case "path-only: returns directory for path with file"
  (check-equal? (path-only "foo/bar.txt") (string->path "foo/")))

(test-case "path-only: returns #f for simple filename (relative)"
  (check-equal? (path-only "simple.txt") #f))

(test-case "path-only: nested path returns parent directory"
  (check-equal? (path-only "a/b/c.txt") (string->path "a/b/")))
