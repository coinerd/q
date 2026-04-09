#lang racket

;; test-find.rkt — tests for tools/builtins/find.rkt
;; TDD: tests first, then implementation.
;; Uses temporary directories for isolation.

(require rackunit
         rackunit/text-ui
         racket/file
         "../tools/builtins/find.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(require (only-in "../tools/tool.rkt"
                  tool-result-content tool-result-details tool-result-is-error?))

(define (result-content r)
  (define parts (tool-result-content r))
  (for/list ([part (in-list parts)])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))

(define (result-details r)    (tool-result-details r))
(define (result-is-error? r)  (tool-result-is-error? r))

(define (with-temp-dir thunk)
  (define dir (make-temporary-file "q-find-test-~a" 'directory))
  (dynamic-wind
    void
    (λ () (thunk dir))
    (λ ()
      (with-handlers ([exn:fail? void])
        (delete-directory/files dir)))))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f
    (lambda (out) (display content out))
    #:exists 'replace)
  f)

(define (create-dir dir name)
  (define d (build-path dir name))
  (make-directory* d)
  d)

;; ============================================================
;; Tests
;; ============================================================

(define find-tests
  (test-suite
   "find-tool"

   ;; 1. Find all entries in a directory tree (type=any includes dirs)
   (test-case "find all entries in a directory tree"
     (with-temp-dir
      (λ (dir)
        (create-file dir "a.txt")
        (create-file dir "sub/b.txt")
        (create-file dir "sub/deep/c.txt")
        (define r (tool-find (hasheq 'path (path->string dir))))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? (sort c string<?)
                      (sort '("a.txt" "sub" "sub/b.txt" "sub/deep" "sub/deep/c.txt") string<?))
        (check-equal? (hash-ref (result-details r) 'total-found) 5)
        (check-false (hash-ref (result-details r) 'truncated?)))))

   ;; 2. Filter by name glob
   (test-case "filter by name glob *.txt"
     (with-temp-dir
      (λ (dir)
        (create-file dir "a.txt")
        (create-file dir "b.rkt")
        (create-file dir "sub/c.txt")
        (create-file dir "sub/d.rkt")
        (define r (tool-find (hasheq 'path (path->string dir)
                                     'name "*.txt")))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? (sort c string<?) (sort '("a.txt" "sub/c.txt") string<?))
        (check-equal? (hash-ref (result-details r) 'total-found) 2))))

   ;; 3. Filter by type "file" only
   (test-case "filter by type file only"
     (with-temp-dir
      (λ (dir)
        (create-file dir "a.txt")
        (create-dir dir "subdir")
        (create-file dir "subdir/b.txt")
        (define r (tool-find (hasheq 'path (path->string dir)
                                     'type "file")))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? (sort c string<?) (sort '("a.txt" "subdir/b.txt") string<?))
        (check-false (member "subdir" c)))))

   ;; 4. Filter by type "dir" only
   (test-case "filter by type dir only"
     (with-temp-dir
      (λ (dir)
        (create-file dir "a.txt")
        (create-dir dir "subdir")
        (create-dir dir "subdir/nested")
        (define r (tool-find (hasheq 'path (path->string dir)
                                     'type "dir")))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? (sort c string<?) (sort '("subdir" "subdir/nested") string<?))
        (check-false (member "a.txt" c)))))

   ;; 5. Max depth limits recursion
   (test-case "max depth limits recursion"
     (with-temp-dir
      (λ (dir)
        (create-file dir "l0.txt")
        (create-file dir "d1/l1.txt")
        (create-file dir "d1/d2/l2.txt")
        (create-file dir "d1/d2/d3/l3.txt")
        (define r0 (tool-find (hasheq 'path (path->string dir)
                                      'max-depth 0)))
        (check-false (result-is-error? r0))
        (define c0 (result-content r0))
        (check-not-false (member "l0.txt" c0))
        (check-not-false (member "d1" c0))
        (check-false (member "d1/l1.txt" c0))
        (define r1 (tool-find (hasheq 'path (path->string dir)
                                      'max-depth 1)))
        (define c1 (result-content r1))
        (check-not-false (member "l0.txt" c1))
        (check-not-false (member "d1/l1.txt" c1))
        (check-false (member "d1/d2/l2.txt" c1)))))

   ;; 6. Max results limits output
   (test-case "max results limits output"
     (with-temp-dir
      (λ (dir)
        (for ([i (in-range 1 11)])
          (create-file dir (format "file~a.txt" i)))
        (define r (tool-find (hasheq 'path (path->string dir)
                                     'max-results 3)))
        (check-false (result-is-error? r))
        (check-equal? (length (result-content r)) 3)
        (check-equal? (hash-ref (result-details r) 'total-found) 10)
        (check-true (hash-ref (result-details r) 'truncated?)))))

   ;; 7. Hidden files skipped by default
   (test-case "hidden files skipped by default"
     (with-temp-dir
      (λ (dir)
        (create-file dir "visible.txt")
        (create-file dir ".hidden.txt")
        (create-file dir ".hiddendir/secret.txt")
        (define r (tool-find (hasheq 'path (path->string dir))))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? c '("visible.txt")))))

   ;; 8. VCS directories skipped
   (test-case "VCS directories skipped"
     (with-temp-dir
      (λ (dir)
        (create-file dir "code.rkt")
        (create-file dir ".git/HEAD")
        (create-file dir ".git/objects/abc")
        (create-file dir "node_modules/pkg/index.js")
        (create-file dir ".hg/store/data")
        (create-file dir ".svn/entries")
        (define r (tool-find (hasheq 'path (path->string dir))))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? c '("code.rkt")))))

   ;; 9. Non-existent path returns error
   (test-case "non-existent path returns error"
     (define r (tool-find (hasheq 'path "/tmp/q-no-such-dir-xyz-999")))
     (check-true (result-is-error? r))
     (define c (string-join (result-content r) ""))
     (check-true (regexp-match? #rx"[Nn]ot [Ff]ound" c)))

   ;; 10. Missing path returns error
   (test-case "missing path argument returns error"
     (define r (tool-find (hasheq)))
     (check-true (result-is-error? r))
     (define c (string-join (result-content r) ""))
     (check-true (regexp-match? #rx"[Mm]issing" c)))

   ;; 11. File path (not directory) returns error
   (test-case "file path instead of directory returns error"
     (with-temp-dir
      (λ (dir)
        (define f (create-file dir "regular.txt"))
        (define r (tool-find (hasheq 'path (path->string f))))
        (check-true (result-is-error? r))
        (define c (string-join (result-content r) ""))
        (check-true (regexp-match? #rx"[Nn]ot a [Dd]irectory" c)))))

   ;; 12. Empty directory returns success with 0 results
   (test-case "empty directory returns success with 0 results"
     (with-temp-dir
      (λ (dir)
        (define r (tool-find (hasheq 'path (path->string dir))))
        (check-false (result-is-error? r))
        (check-equal? (result-content r) '())
        (check-equal? (hash-ref (result-details r) 'total-found) 0)
        (check-false (hash-ref (result-details r) 'truncated?)))))

   ;; --- Additional edge cases ---

   (test-case "details include search-root"
     (with-temp-dir
      (λ (dir)
        (define r (tool-find (hasheq 'path (path->string dir))))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'search-root)
                      (path->string dir)))))

   (test-case "name glob with question mark matches single char"
     (with-temp-dir
      (λ (dir)
        (create-file dir "ab.txt")
        (create-file dir "abc.txt")
        (define r (tool-find (hasheq 'path (path->string dir)
                                     'name "a?.txt")))
        (check-false (result-is-error? r))
        (define c (result-content r))
        (check-equal? c '("ab.txt")))))

   ;; BUG-13 regression: find with path="." must not crash
   (test-case "find with relative path '.' does not crash"
     (with-temp-dir
      (λ (dir)
        (create-file dir "alpha.txt")
        (create-file dir "sub/beta.txt")
        (define orig-cd (current-directory))
        (dynamic-wind
          (lambda () (current-directory dir))
          (lambda ()
            (define r (tool-find (hasheq 'path ".")))
            (check-false (result-is-error? r))
            (define c (result-content r))
            (check-true (andmap string? c))
            (check-not-false (member "alpha.txt" c))
            (check-not-false (member "sub" c))
            (check-not-false (member "sub/beta.txt" c)))
          (lambda () (current-directory orig-cd))))))

   (test-case "truncated is false when results fit"
     (with-temp-dir
      (λ (dir)
        (create-file dir "single.txt")
        (define r (tool-find (hasheq 'path (path->string dir)
                                     'max-results 100)))
        (check-false (result-is-error? r))
        (check-false (hash-ref (result-details r) 'truncated?)))))
   ))

;; ============================================================
;; Run
;; ============================================================

(run-tests find-tests)
