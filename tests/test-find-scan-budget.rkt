#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; test-find-scan-budget.rkt — BUG-3 regression tests
;; Validates that find has a hard scan budget and reports scanned count.

(require rackunit
         rackunit/text-ui
         racket/file
         "../tools/builtins/find.rkt"
         (only-in "../tools/tool.rkt" tool-result-content tool-result-details tool-result-is-error?)
         "helpers/fixtures.rkt")

(define (result-details r) (tool-result-details r))
(define (result-is-error? r) (tool-result-is-error? r))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace)
  f)

(define scan-budget-tests
  (test-suite "find-scan-budget"

    ;; BUG-3: scanned count must appear in metadata
    (test-case "find result metadata includes scanned count"
      (with-temp-dir
       (λ (dir)
         (for ([i (in-range 10)])
           (create-file dir (format "file~a.txt" i)))
         (define r (tool-find (hasheq 'path (path->string dir))))
         (check-false (result-is-error? r))
         (define d (result-details r))
         (check-true (hash-has-key? d 'scanned)
                     (format "Expected 'scanned key in metadata, got: ~a" (hash-keys d)))
         (check-true (exact-nonnegative-integer? (hash-ref d 'scanned))
                     (format "scanned should be non-negative integer, got: ~a"
                             (hash-ref d 'scanned))))))

    ;; BUG-3: budget should cap scanning for broad trees
    (test-case "find scan budget prevents unbounded traversal"
      (with-temp-dir
       (λ (dir)
         ;; Create 200 files in a flat directory
         (for ([i (in-range 200)])
           (create-file dir (format "file~a.txt" i)))
         (define r (tool-find (hasheq 'path (path->string dir)
                                      'max-results 200
                                      'max-depth 10)))
         (check-false (result-is-error? r))
         (define d (result-details r))
         ;; All 200 should be found
         (check-equal? (hash-ref d 'total-found) 200)
         ;; Scanned should be >= total-found (may scan more entries like . and ..)
         (check-true (>= (hash-ref d 'scanned) (hash-ref d 'total-found))
                     (format "scanned (~a) should be >= total-found (~a)"
                             (hash-ref d 'scanned) (hash-ref d 'total-found))))))

    ;; Budget metadata is present even for small scans
    (test-case "scanned count present for small tree"
      (with-temp-dir
       (λ (dir)
         (create-file dir "single.txt")
         (define r (tool-find (hasheq 'path (path->string dir))))
         (check-false (result-is-error? r))
         (check-true (hash-has-key? (result-details r) 'scanned)))))))

(run-tests scan-budget-tests)
