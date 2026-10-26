#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration

;; test-find-scan-budget.rkt — BUG-3 regression tests
;; Validates that find has a hard scan budget and reports scanned count.
;;
;; R97 (BUG-0097) additions: absolute-path emission and denial records do
;; not change budget accounting — max-results/total-found/scanned semantics
;; are preserved; denied entries never consume result slots.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         "../tools/builtins/find.rkt"
         (only-in "../tools/tool.rkt" tool-result-content tool-result-details tool-result-is-error?)
         "helpers/fixtures.rkt")

(define (result-details r)
  (tool-result-details r))
(define (result-is-error? r)
  (tool-result-is-error? r))
(define (result-content-strings r)
  (for/list ([part (in-list (tool-result-content r))])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace)
  f)

(define scan-budget-tests
  (test-suite "find-scan-budget"

    ;; BUG-3: scanned count must appear in metadata
    (test-case "find result metadata includes scanned count"
      (with-temp-dir (λ (dir)
                       (for ([i (in-range 10)])
                         (create-file dir (format "file~a.txt" i)))
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define d (result-details r))
                       (check-true (hash-has-key? d 'scanned)
                                   (format "Expected 'scanned key in metadata, got: ~a"
                                           (hash-keys d)))
                       (check-true (exact-nonnegative-integer? (hash-ref d 'scanned))
                                   (format "scanned should be non-negative integer, got: ~a"
                                           (hash-ref d 'scanned))))))

    ;; BUG-3: budget should cap scanning for broad trees
    (test-case "find scan budget prevents unbounded traversal"
      (with-temp-dir (λ (dir)
                       ;; Create 200 files in a flat directory
                       (for ([i (in-range 200)])
                         (create-file dir (format "file~a.txt" i)))
                       (define r
                         (tool-find (hasheq 'path (path->string dir) 'max-results 200 'max-depth 10)))
                       (check-false (result-is-error? r))
                       (define d (result-details r))
                       ;; All 200 should be found
                       (check-equal? (hash-ref d 'total-found) 200)
                       ;; Scanned should be >= total-found (may scan more entries like . and ..)
                       (check-true (>= (hash-ref d 'scanned) (hash-ref d 'total-found))
                                   (format "scanned (~a) should be >= total-found (~a)"
                                           (hash-ref d 'scanned)
                                           (hash-ref d 'total-found))))))

    ;; Budget metadata is present even for small scans
    (test-case "scanned count present for small tree"
      (with-temp-dir (λ (dir)
                       (create-file dir "single.txt")
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (check-true (hash-has-key? (result-details r) 'scanned)))))

;; ============================================================
    ;; R97: absolute emission must not disturb budget accounting
    ;; ============================================================

    ;; max-results counts only EMITTED matches; a dangling symlink denial must
    ;; not consume a result slot (old relative-display code counted it as a
    ;; match).
    (test-case "R97: denied dangling link does not consume max-results slots"
      (with-temp-dir (λ (dir)
                       (for ([i (in-range 5)])
                         (create-file dir (format "f~a.txt" i)))
                       (make-file-or-directory-link (build-path dir "missing-target")
                                                    (build-path dir "dangle"))
                       (define r (tool-find (hasheq 'path (path->string dir) 'max-results 5)))
                       (check-false (result-is-error? r))
                       (define d (result-details r))
                       (check-equal? (hash-ref d 'total-found) 5)
                       (check-equal? (length (result-content-strings r)) 5)
                       (check-false (hash-ref d 'truncated?))
                       ;; structured denial is recorded separately
                       (define denied (hash-ref d 'denied))
                       (check-equal? (length denied) 1)
                       (check-equal? (hash-ref (car denied) 'reason) "dangling-symlink")
                       ;; all emitted results are absolute
                       (for ([s (in-list (result-content-strings r))])
                         (check-true (absolute-path? s) (format "must be absolute: ~a" s)))
                       ;; budget metadata untouched
                       (check-true (>= (hash-ref d 'scanned) (hash-ref d 'total-found))))))

    ;; scanned/total-found/truncated? accounting is identical with absolute
    ;; results on a broad flat tree.
    (test-case "R97: budget accounting unchanged with absolute results"
      (with-temp-dir (λ (dir)
                       (for ([i (in-range 50)])
                         (create-file dir (format "file~a.txt" i)))
                       (define r (tool-find (hasheq 'path (path->string dir)
                                                 'max-results 10 'max-depth 10)))
                       (check-false (result-is-error? r))
                       (define d (result-details r))
                       (check-equal? (hash-ref d 'total-found) 50)
                       (check-equal? (length (result-content-strings r)) 10)
                       (check-true (hash-ref d 'truncated?))
                       (check-true (>= (hash-ref d 'scanned) 50))
                       ;; path-base metadata declares the absolute namespace
                       (check-equal? (hash-ref d 'path-base) 'absolute)))))

)

(run-tests scan-budget-tests)