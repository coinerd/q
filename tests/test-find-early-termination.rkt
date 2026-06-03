#lang racket

;; BOUNDARY: integration

;; test-find-early-termination.rkt — BUG-1 regression tests
;; Validates that walk-dir stops recursing once max-results matches are found.

(require rackunit
         rackunit/text-ui
         racket/file
         "../tools/builtins/find.rkt"
         (only-in "../tools/tool.rkt" tool-result-content tool-result-details tool-result-is-error?)
         "helpers/fixtures.rkt")

(define (result-content r)
  (for/list ([part (in-list (tool-result-content r))])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))

(define (result-total r)
  (hash-ref (tool-result-details r) 'total-found 0))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace))

(define early-term-tests
  (test-suite "find-early-termination"

    (test-case "walk-dir stops recursing after max-results found"
      (with-temp-dir
       (lambda (dir)
         ;; Create 5 directories, each with a .txt file
         (for ([i (in-range 5)])
           (create-file dir (format "d~a/match.txt" i)))
         ;; Create deep nesting under each
         (for ([i (in-range 5)])
           (create-file dir (format "d~a/sub/deep/deep~a.txt" i i)))
         (define r
           (tool-find
            (hasheq 'path (path->string dir) 'name "match.txt" 'max-results 2 'max-depth 10)))
         (check-false (tool-result-is-error? r))
         (check-equal? (length (result-content r)) 2)
         ;; With early termination, total-found <= 5 (not 10)
         (check-true (<= (result-total r) 5)
                     (format "total-found ~a should be <= 5" (result-total r))))))

    (test-case "walk-dir with max-results=1 returns immediately"
      (with-temp-dir (lambda (dir)
                       (create-file dir "top.txt")
                       (for ([i (in-range 10)])
                         (create-file dir (format "sub~a/file.txt" i))
                         (create-file dir (format "sub~a/deep/file.txt" i))
                         (create-file dir (format "sub~a/deep/deeper/file.txt" i)))
                       (define start (current-inexact-milliseconds))
                       (define r
                         (tool-find (hasheq 'path (path->string dir) 'max-results 1 'max-depth 10)))
                       (define elapsed (- (current-inexact-milliseconds) start))
                       (check-false (tool-result-is-error? r))
                       (check-equal? (length (result-content r)) 1)
                       (check-true (< elapsed 500.0) (format "Expected < 500ms, got ~ams" elapsed)))))

    (test-case "walk-dir finds results across levels then stops"
      (with-temp-dir
       (lambda (dir)
         (create-file dir "root.txt")
         (create-file dir "sub/a.txt")
         (create-file dir "sub/deep/b.txt")
         (create-file dir "sub/deep/deeper/c.txt")
         (create-file dir "other/x.txt")
         (create-file dir "other/y.txt")
         (define r
           (tool-find (hasheq 'path (path->string dir) 'name "*.txt" 'max-results 3 'max-depth 10)))
         (check-false (tool-result-is-error? r))
         (check-equal? (length (result-content r)) 3))))))

(run-tests early-term-tests)
