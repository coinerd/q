#lang racket

;; @covers tools/builtins/find.rkt
;; @covers tools/tool.rkt

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; test-find.rkt — tests for tools/builtins/find.rkt
;; TDD: tests first, then implementation.
;; Uses temporary directories for isolation.
;; @boundary integration
;;
;; R97 (BUG-0097): every find content result string is an ABSOLUTE path in
;; the executing filesystem namespace, directly passable as read's path
;; argument there. Results are built from traversal paths (not user-string
;; concatenation), search-root metadata stays absolute, and details carry
;; path-base 'absolute. Output compatibility change: results were
;; root-relative display strings before R97.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/runtime-path
         "../tools/builtins/find.rkt"
         (only-in "../tools/builtins/read.rkt" tool-read))

(define-runtime-path test-dir ".")

;; ============================================================
;; Helpers
;; ============================================================

(require (only-in "../tools/tool.rkt" tool-result-content tool-result-details tool-result-is-error?)
         "helpers/fixtures.rkt")

(define (result-content r)
  (define parts (tool-result-content r))
  (for/list ([part (in-list parts)])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))

(define (result-details r)
  (tool-result-details r))
(define (result-is-error? r)
  (tool-result-is-error? r))

(define (result-text r)
  (string-join (result-content r) ""))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace)
  f)

(define (create-dir dir name)
  (define d (build-path dir name))
  (make-directory* d)
  d)

;; R97 helper: every result string must be an absolute filesystem path
;; with no ".." segments (built from traversal paths, not user strings).
(define (check-absolute-results! r)
  (for ([s (in-list (result-content r))])
    (check-true (absolute-path? s) (format "result must be absolute: ~a" s))
    (check-false (string-contains? s "..") (format "result must not contain '..': ~a" s))))

;; R97 helper: read a found path and return the concatenated content text.
(define (read-found-text found-path)
  (define rr (tool-read (hasheq 'path found-path)))
  (check-false (result-is-error? rr) (format "read must succeed for ~a" found-path))
  (result-text rr))

;; ============================================================
;; Tests
;; ============================================================

(define find-tests
  (test-suite "find-tool"

    ;; 1. Find all entries in a directory tree (type=any includes dirs)
    (test-case "find all entries in a directory tree"
      (with-temp-dir (λ (dir)
                       (create-file dir "a.txt")
                       (create-file dir "sub/b.txt")
                       (create-file dir "sub/deep/c.txt")
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? (sort c string<?)
                                     (sort (list (path->string (build-path dir "a.txt"))
                                                 (path->string (build-path dir "sub"))
                                                 (path->string (build-path dir "sub/b.txt"))
                                                 (path->string (build-path dir "sub/deep"))
                                                 (path->string (build-path dir "sub/deep/c.txt")))
                                           string<?))
                       (check-equal? (hash-ref (result-details r) 'total-found) 5)
                       (check-false (hash-ref (result-details r) 'truncated?)))))

    ;; 2. Filter by name glob
    (test-case "filter by name glob *.txt"
      (with-temp-dir (λ (dir)
                       (create-file dir "a.txt")
                       (create-file dir "b.rkt")
                       (create-file dir "sub/c.txt")
                       (create-file dir "sub/d.rkt")
                       (define r (tool-find (hasheq 'path (path->string dir) 'name "*.txt")))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? (sort c string<?)
                                     (sort (list (path->string (build-path dir "a.txt"))
                                                 (path->string (build-path dir "sub/c.txt")))
                                           string<?))
                       (check-equal? (hash-ref (result-details r) 'total-found) 2))))

    ;; 3. Filter by type "file" only
    (test-case "filter by type file only"
      (with-temp-dir (λ (dir)
                       (create-file dir "a.txt")
                       (create-dir dir "subdir")
                       (create-file dir "subdir/b.txt")
                       (define r (tool-find (hasheq 'path (path->string dir) 'type "file")))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? (sort c string<?)
                                     (sort (list (path->string (build-path dir "a.txt"))
                                                 (path->string (build-path dir "subdir/b.txt")))
                                           string<?))
                       (check-false (member (path->string (build-path dir "subdir")) c)))))

    ;; 4. Filter by type "dir" only
    (test-case "filter by type dir only"
      (with-temp-dir (λ (dir)
                       (create-file dir "a.txt")
                       (create-dir dir "subdir")
                       (create-dir dir "subdir/nested")
                       (define r (tool-find (hasheq 'path (path->string dir) 'type "dir")))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? (sort c string<?)
                                     (sort (list (path->string (build-path dir "subdir"))
                                                 (path->string (build-path dir "subdir/nested")))
                                           string<?))
                       (check-false (member (path->string (build-path dir "a.txt")) c)))))

    ;; 5. Max depth limits recursion
    (test-case "max depth limits recursion"
      (with-temp-dir (λ (dir)
                       (create-file dir "l0.txt")
                       (create-file dir "d1/l1.txt")
                       (create-file dir "d1/d2/l2.txt")
                       (create-file dir "d1/d2/d3/l3.txt")
                       (define r0 (tool-find (hasheq 'path (path->string dir) 'max-depth 0)))
                       (check-false (result-is-error? r0))
                       (define c0 (result-content r0))
                       (check-not-false (member (path->string (build-path dir "l0.txt")) c0))
                       (check-not-false (member (path->string (build-path dir "d1")) c0))
                       (check-false (member (path->string (build-path dir "d1/l1.txt")) c0))
                       (define r1 (tool-find (hasheq 'path (path->string dir) 'max-depth 1)))
                       (define c1 (result-content r1))
                       (check-not-false (member (path->string (build-path dir "l0.txt")) c1))
                       (check-not-false (member (path->string (build-path dir "d1/l1.txt")) c1))
                       (check-false (member (path->string (build-path dir "d1/d2/l2.txt")) c1)))))

    ;; 6. Max results limits output
    (test-case "max results limits output"
      (with-temp-dir (λ (dir)
                       (for ([i (in-range 1 11)])
                         (create-file dir (format "file~a.txt" i)))
                       (define r (tool-find (hasheq 'path (path->string dir) 'max-results 3)))
                       (check-false (result-is-error? r))
                       (check-equal? (length (result-content r)) 3)
                       (check-equal? (hash-ref (result-details r) 'total-found) 10)
                       (check-true (hash-ref (result-details r) 'truncated?)))))

    ;; 7. Hidden files skipped by default
    (test-case "hidden files skipped by default"
      (with-temp-dir (λ (dir)
                       (create-file dir "visible.txt")
                       (create-file dir ".hidden.txt")
                       (create-file dir ".hiddendir/secret.txt")
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? c (list (path->string (build-path dir "visible.txt")))))))

    ;; 8. VCS directories skipped
    (test-case "VCS directories skipped"
      (with-temp-dir (λ (dir)
                       (create-file dir "code.rkt")
                       (create-file dir ".git/HEAD")
                       (create-file dir ".git/objects/abc")
                       (create-file dir "node_modules/pkg/index.js")
                       (create-file dir ".hg/store/data")
                       (create-file dir ".svn/entries")
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? c (list (path->string (build-path dir "code.rkt")))))))

    ;; 9. Non-existent path returns error
    (test-case "find: non-existent path returns error"
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
      (with-temp-dir (λ (dir)
                       (define f (create-file dir "regular.txt"))
                       (define r (tool-find (hasheq 'path (path->string f))))
                       (check-true (result-is-error? r))
                       (define c (string-join (result-content r) ""))
                       (check-true (regexp-match? #rx"[Nn]ot a [Dd]irectory" c)))))

    ;; 12. Empty directory returns success with 0 results
    (test-case "empty directory returns success with 0 results"
      (with-temp-dir (λ (dir)
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (check-equal? (result-content r) '())
                       (check-equal? (hash-ref (result-details r) 'total-found) 0)
                       (check-false (hash-ref (result-details r) 'truncated?)))))

    ;; --- Additional edge cases ---

    (test-case "details include search-root (absolute) and path-base absolute"
      (with-temp-dir (λ (dir)
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define d (result-details r))
                       (check-equal? (hash-ref d 'search-root)
                                     (path->string (simple-form-path dir)))
                       (check-true (absolute-path? (hash-ref d 'search-root)))
                       ;; R97: output namespace declared in metadata
                       (check-equal? (hash-ref d 'path-base #f) 'absolute))))

    (test-case "name glob with question mark matches single char"
      (with-temp-dir (λ (dir)
                       (create-file dir "ab.txt")
                       (create-file dir "abc.txt")
                       (define r (tool-find (hasheq 'path (path->string dir) 'name "a?.txt")))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? c (list (path->string (build-path dir "ab.txt")))))))

    ;; BUG-13 regression: find with path="." must not crash.
    ;; R97: results are absolute even for a dot root.
    (test-case "find with relative path '.' does not crash and returns absolute results"
      (with-temp-dir (λ (dir)
                       (create-file dir "alpha.txt")
                       (create-file dir "sub/beta.txt")
                       (define orig-cd (current-directory))
                       (dynamic-wind (lambda () (current-directory dir))
                                     (lambda ()
                                       (define r (tool-find (hasheq 'path ".")))
                                       (check-false (result-is-error? r))
                                       (define c (result-content r))
                                       (check-true (andmap string? c))
                                       (check-not-false (member (path->string (build-path dir "alpha.txt")) c))
                                       (check-not-false (member (path->string (build-path dir "sub")) c))
                                       (check-not-false (member (path->string (build-path dir "sub/beta.txt")) c)))
                                     (lambda () (current-directory orig-cd))))))

    (test-case "truncated is false when results fit"
      (with-temp-dir (λ (dir)
                       (create-file dir "single.txt")
                       (define r (tool-find (hasheq 'path (path->string dir) 'max-results 100)))
                       (check-false (result-is-error? r))
                       (check-false (hash-ref (result-details r) 'truncated?)))))

    ;; ============================================================
    ;; R97 (BUG-0097): absolute, openable find -> read roundtrip
    ;; ============================================================

    ;; R97 core contract: every result is an absolute path directly
    ;; passable as read.path in the executing filesystem namespace.
    (test-case "R97: results are absolute paths directly readable by read"
      (with-temp-dir (λ (dir)
                       (create-file dir "docs/contracts/spec.txt" "R97-DOCS-SENTINEL")
                       (define r (tool-find (hasheq 'path (path->string dir) 'name "*.txt")))
                       (check-false (result-is-error? r))
                       (check-absolute-results! r)
                       (define found (car (result-content r)))
                       (check-equal? found (path->string (build-path dir "docs/contracts/spec.txt")))
                       ;; The roundtrip: found path is openable by read AS-IS.
                       (check-true (string-contains? (read-found-text found)
                                                     "R97-DOCS-SENTINEL")))))

    ;; R97: relative (dot) root and absolute (non-dot) root agree.
    (test-case "R97: dot root and absolute root produce identical absolute results"
      (with-temp-dir (λ (dir)
                       (create-file dir "x.txt")
                       (create-file dir "s/y.txt")
                       (define orig-cd (current-directory))
                       (dynamic-wind (lambda () (current-directory dir))
                                     (lambda ()
                                       (define r-dot (tool-find (hasheq 'path ".")))
                                       (define r-abs (tool-find (hasheq 'path (path->string dir))))
                                       (check-false (result-is-error? r-dot))
                                       (check-false (result-is-error? r-abs))
                                       (check-absolute-results! r-dot)
                                       (check-absolute-results! r-abs)
                                       (check-equal? (sort (result-content r-dot) string<?)
                                       (sort (result-content r-abs) string<?)))
                       (lambda () (current-directory orig-cd))))))

    ;; R97: absolute root OUTSIDE the current working directory is still
    ;; openable; spaces and Unicode survive the roundtrip.
    (test-case "R97: absolute root outside cwd with spaces/Unicode roundtrips"
      (with-temp-dir (λ (dir)
                       (define deep (create-dir dir "dir with spaces/ünïcodé-データ"))
                       (create-file deep "note.txt" "unicode-space-sentinel")
                       ;; cwd is deliberately NOT dir (repo root while tests run)
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (check-absolute-results! r)
                       (define c (result-content r))
                       (define found
                         (findf (lambda (s) (string-suffix? s "note.txt")) c))
                       (check-not-false found)
                       (check-true (string-contains? found "dir with spaces"))
                       (check-true (string-contains? found "ünïcodé-データ"))
                       (check-true (string-contains? (read-found-text found)
                                                     "unicode-space-sentinel")))))

    ;; R97: CWD switched AFTER find still resolves — absolute results do
    ;; not depend on the current directory, and a conflicting same-named
    ;; relative path must not shadow the found file.
    (test-case "R97: CWD switched after find still resolves (conflicting decoy ignored)"
      (with-temp-dir (λ (docs-root)
                       ;; fixture: docs/contracts/spec.txt (the true target)
                       (create-file docs-root "docs/contracts/spec.txt" "docs-contracts-sentinel")
                       ;; conflicting file that the OLD root-relative result
                       ;; "contracts/spec.txt" would resolve to from a new cwd
                       (define r (tool-find (hasheq 'path (path->string docs-root))))
                       (check-false (result-is-error? r))
                       (define found
                         (findf (lambda (s) (string-suffix? s "docs/contracts/spec.txt"))
                                (result-content r)))
                       (check-not-false found)
                       (with-temp-dir (λ (other-cwd)
                                        (create-file other-cwd "contracts/spec.txt" "decoy-sentinel")
                                        (define orig-cd (current-directory))
                                        (dynamic-wind (lambda () (current-directory other-cwd))
                                                      (lambda ()
                                                        ;; absolute result resolves to the
                                                        ;; docs sentinel, not the decoy
                                                        (check-true (string-contains?
                                                                     (read-found-text found)
                                                                     "docs-contracts-sentinel")))
                                                      (lambda () (current-directory orig-cd))))))))

    ;; R97: '..' in the requested root never leaks into results.
    (test-case "R97: '..' in root path is normalized; results stay absolute"
      (with-temp-dir (λ (dir)
                       (create-file dir "s/inner.txt")
                       (create-file dir "top.txt")
                       (define orig-cd (current-directory))
                       (dynamic-wind (lambda () (current-directory dir))
                                     (lambda ()
                                       (define r1 (tool-find (hasheq 'path "s/..")))
                                       (check-false (result-is-error? r1))
                                       (check-absolute-results! r1)
                                       ;; top.txt, s, s/inner.txt
                                       (check-equal? (length (result-content r1)) 3)
                                       ;; absolute argument with '..' behaves the same
                                       (define r2
                                         (tool-find
                                          (hasheq 'path
                                                  (path->string (build-path dir "s" "..")))))
                                       (check-false (result-is-error? r2))
                                       (check-absolute-results! r2)
                                       (check-equal? (sort (result-content r1) string<?)
                                                     (sort (result-content r2) string<?)))
                       (lambda () (current-directory orig-cd))))))

    ;; R97: directory results — read correctly rejects them with an error.
    (test-case "R97: directory results are rejected by read"
      (with-temp-dir (λ (dir)
                       (create-file dir "f.txt")
                       (create-dir dir "subdir")
                       (define r (tool-find (hasheq 'path (path->string dir) 'type "dir")))
                       (check-false (result-is-error? r))
                       (define c (result-content r))
                       (check-equal? (length c) 1)
                       (define rr (tool-read (hasheq 'path (car c))))
                       (check-true (result-is-error? rr)
                                   (format "read must reject directory result: ~a" (car c)))
                       (check-true (> (string-length (result-text rr)) 0)))))))

;; ============================================================
;; Run
;; ============================================================

;; Robustness: negative max-depth clamps to 0
(test-case "negative max-depth clamps to 0"
  (with-temp-dir (λ (dir)
                   (create-file dir "l0.txt")
                   (create-file dir "sub/l1.txt")
                   (define r (tool-find (hasheq 'path (path->string dir) 'max-depth -1)))
                   (check-false (result-is-error? r))
                   (define c (result-content r))
                   (check-not-false (member (path->string (build-path dir "l0.txt")) c))
                   (check-not-false (member (path->string (build-path dir "sub")) c))
                   (check-false (member (path->string (build-path dir "sub/l1.txt")) c)))))

;; Robustness: negative max-results clamps to 1
(test-case "negative max-results clamps to 1"
  (with-temp-dir (λ (dir)
                   (create-file dir "a.txt")
                   (create-file dir "b.txt")
                   (define r (tool-find (hasheq 'path (path->string dir) 'max-results -5)))
                   (check-false (result-is-error? r))
                   (check-equal? (length (result-content r)) 1)
                   (check-equal? (hash-ref (result-details r) 'total-found) 2)
                   (check-true (hash-ref (result-details r) 'truncated?)))))

;; Edge case: empty name pattern matches all
(test-case "empty name pattern matches all"
  (with-temp-dir (λ (dir)
                   (create-file dir "a.txt")
                   (create-file dir "b.rkt")
                   (define r (tool-find (hasheq 'path (path->string dir) 'name "")))
                   (check-false (result-is-error? r))
                   (define c (result-content r))
                   (check-equal? (length c) 2))))

;; R97: even clamped/degenerate calls return absolute results
(test-case "R97: negative-clamp and empty-pattern results stay absolute"
  (with-temp-dir (λ (dir)
                   (create-file dir "a.txt")
                   (define r (tool-find (hasheq 'path (path->string dir) 'max-results -5)))
                   (check-false (result-is-error? r))
                   (check-absolute-results! r))))

(run-tests find-tests)
