#lang racket

;; test-ls.rkt — tests for tools/builtins/ls.rkt
;; TDD: comprehensive tests for the directory listing tool.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../tools/builtins/ls.rkt")

;; ============================================================
;; Result helpers
;; ============================================================

(require (only-in "../tools/tool.rkt"
                  tool-result? tool-result-content tool-result-details tool-result-is-error?))

;; Extract strings from content (handles both plain strings and content-part hashes)
(define (result-content r)
  (define parts (tool-result-content r))
  (for/list ([part (in-list parts)])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))
(define (result-details r)    (tool-result-details r))
(define (result-is-error? r)  (tool-result-is-error? r))

;; ============================================================
;; Temp directory fixture
;; ============================================================

(define (with-test-dir thunk)
  (define dir (make-temporary-file "q-ls-test-~a" 'directory))
  (dynamic-wind
    void
    (λ () (thunk dir))
    (λ () (delete-directory/files dir))))

;; Create test files/dirs inside a given base
(define (populate-test-dir base)
  ;; Files
  (call-with-output-file (build-path base "banana.txt")
    (λ (p) (display "yellow" p)))
  (call-with-output-file (build-path base "apple.txt")
    (λ (p) (display "red fruit is here" p)))  ; 18 bytes
  (call-with-output-file (build-path base "cherry.dat")
    (λ (p) (display "x" p)))  ; 1 byte
  ;; Hidden file
  (call-with-output-file (build-path base ".secret")
    (λ (p) (display "hidden stuff" p)))
  ;; Subdirectory
  (make-directory (build-path base "subdir"))
  ;; Hidden subdirectory
  (make-directory (build-path base ".hidden-dir")))

;; ============================================================
;; Tests
;; ============================================================

(define ls-tests
  (test-suite
   "tool-ls"

   ;; 1. List entries in a directory
   (test-case "list entries in a directory"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir)))
        (check-false (result-is-error? r))
        (define content (result-content r))
        (check-not-false (member "banana.txt" content))
        (check-not-false (member "apple.txt" content))
        (check-not-false (member "cherry.dat" content))
        (check-not-false (member "subdir" content)))))

   ;; 2. Hidden files excluded by default
   (test-case "hidden files excluded by default"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir)))
        (check-false (result-is-error? r))
        (check-false (member ".secret" (result-content r)))
        (check-false (member ".hidden-dir" (result-content r))))))

   ;; 3. Hidden files shown with all? flag
   (test-case "hidden files shown with all? flag"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir 'all? #t)))
        (check-false (result-is-error? r))
        (check-not-false (member ".secret" (result-content r)))
        (check-not-false (member ".hidden-dir" (result-content r))))))

   ;; 4. Long format includes type and size
   (test-case "long format includes type and size"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir 'long? #t)))
        (check-false (result-is-error? r))
        (define content (result-content r))
        ;; subdir should have 'd' type and '-' size
        (check-not-false (ormap (λ (l) (regexp-match? #rx"^d - subdir$" l)) content)
                         (format "expected dir line, got: ~a" content))
        ;; banana.txt should have '-' type and 6 bytes
        (check-not-false (ormap (λ (l) (regexp-match? #rx"^- 6 banana.txt$" l)) content)
                         (format "expected banana line, got: ~a" content))
        ;; cherry.dat is 1 byte
        (check-not-false (ormap (λ (l) (regexp-match? #rx"^- 1 cherry.dat$" l)) content)
                         (format "expected cherry line, got: ~a" content))
        ;; apple.txt is 18 bytes
        (check-not-false (ormap (λ (l) (regexp-match? #rx"^- 17 apple.txt$" l)) content)
                         (format "expected apple line, got: ~a" content)))))

   ;; 5. Sort by name (default)
   (test-case "sort by name (default)"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir)))
        (check-false (result-is-error? r))
        (define content (result-content r))
        ;; Directories first, then files alphabetically
        (define dir-entries (filter (λ (n) (equal? n "subdir")) content))
        (define file-entries (filter (λ (n) (member n '("apple.txt" "banana.txt" "cherry.dat"))) content))
        (check-equal? dir-entries '("subdir"))
        (check-equal? file-entries '("apple.txt" "banana.txt" "cherry.dat")))))

   ;; 6. Sort by size
   (test-case "sort by size"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir 'sort-by "size")))
        (check-false (result-is-error? r))
        (define content (result-content r))
        (define sizes
          (for/list ([name (in-list content)])
            (define full (build-path dir name))
            (if (file-exists? full) (file-size full) -1)))
        ;; Should be non-increasing (largest first)
        (check-equal? sizes (sort sizes >)))))

   ;; 7. Sort by date
   (test-case "sort by date"
     (with-test-dir
      (λ (dir)
        ;; Create files with a time gap
        (call-with-output-file (build-path dir "old.txt")
          (λ (p) (display "old" p)))
        (file-or-directory-modify-seconds (build-path dir "old.txt") 1000000)
        (sleep 0.05)
        (call-with-output-file (build-path dir "new.txt")
          (λ (p) (display "new content here" p)))
        (define r (tool-ls (hasheq 'path dir 'sort-by "date")))
        (check-false (result-is-error? r))
        (define content (result-content r))
        ;; newest first
        (check-equal? (first content) "new.txt")
        (check-equal? (second content) "old.txt"))))

   ;; 8. Directories listed first in name sort
   (test-case "directories listed first in name sort"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        ;; Add a file that sorts before "subdir" alphabetically
        (call-with-output-file (build-path dir "aaa-file.txt")
          (λ (p) (display "a" p)))
        (define r (tool-ls (hasheq 'path dir)))
        (check-false (result-is-error? r))
        (define content (result-content r))
        ;; "subdir" should appear before "aaa-file.txt"
        (define subdir-idx (index-of content "subdir"))
        (define aaa-idx (index-of content "aaa-file.txt"))
        (check-true (and subdir-idx aaa-idx (< subdir-idx aaa-idx))))))

   ;; 9. Non-existent path returns error
   (test-case "non-existent path returns error"
     (define r (tool-ls (hasheq 'path "/no/such/directory/ever")))
     (check-true (result-is-error? r))
     (check-not-false (regexp-match? #rx"not found" (first (result-content r)))))

   ;; 10. Missing path returns error
   (test-case "missing path returns error"
     (define r (tool-ls (hasheq)))
     (check-true (result-is-error? r))
     (check-not-false (regexp-match? #rx"Missing" (first (result-content r)))))

   ;; 11. File path (not directory) returns error
   (test-case "file path (not directory) returns error"
     (with-test-dir
      (λ (dir)
        (define fp (build-path dir "afile.txt"))
        (call-with-output-file fp (λ (p) (display "hi" p)))
        (define r (tool-ls (hasheq 'path (path->string fp))))
        (check-true (result-is-error? r))
        (check-not-false (regexp-match? #rx"Not a directory" (first (result-content r)))))))

   ;; 12. Empty directory returns success with 0 entries
   (test-case "empty directory returns success with 0 entries"
     (with-test-dir
      (λ (dir)
        (define r (tool-ls (hasheq 'path dir)))
        (check-false (result-is-error? r))
        (check-equal? (result-content r) '())
        (check-equal? (hash-ref (result-details r) 'total-entries) 0)
        (check-equal? (hash-ref (result-details r) 'directories) 0)
        (check-equal? (hash-ref (result-details r) 'files) 0))))

   ;; 13. Symlinks shown with 'l' type in long format
   (test-case "symlinks shown with 'l' type in long format"
     (with-test-dir
      (λ (dir)
        (call-with-output-file (build-path dir "target.txt")
          (λ (p) (display "hello" p)))
        (make-file-or-directory-link (build-path dir "target.txt")
                                     (build-path dir "link-to-target"))
        (define r (tool-ls (hasheq 'path dir 'long? #t)))
        (check-false (result-is-error? r))
        (define content (result-content r))
        (check-not-false
         (ormap (λ (l) (regexp-match? #rx"^l - link-to-target$" l)) content)
         (format "Expected symlink line, got: ~a" content)))))

   ;; --- Details hash verification ---
   (test-case "details hash contains expected keys"
     (with-test-dir
      (λ (dir)
        (populate-test-dir dir)
        (define r (tool-ls (hasheq 'path dir)))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-entries) 4) ; apple, banana, cherry, subdir
        (check-equal? (hash-ref d 'directories) 1)
        (check-equal? (hash-ref d 'files) 3)
        (check-equal? (hash-ref d 'path) dir))))

   ))

;; ============================================================
;; Run
;; ============================================================

(run-tests ls-tests)
