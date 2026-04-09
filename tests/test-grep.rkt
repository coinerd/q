#lang racket

;; test-grep.rkt — tests for tools/builtins/grep.rkt
;; TDD: comprehensive tests for tool-grep
;; Uses temporary files/dirs for isolation. No external dependencies.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../tools/builtins/grep.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (with-temp-dir thunk)
  (define dir (make-temporary-file "q-grep-test-~a" 'directory))
  (dynamic-wind
    void
    (λ () (thunk dir))
    (λ ()
      (with-handlers ([exn:fail? void])
        (delete-directory/files dir)))))

(define (write-string-to-file path content)
  (call-with-output-file path
    (lambda (out) (display content out))
    #:exists 'replace))

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

;; ============================================================
;; Tests
;; ============================================================

(define grep-tests
  (test-suite
   "grep-tool"

   ;; 1. Search in a single file, matches found
   (test-case "single file with matches"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "sample.txt"))
        (write-string-to-file f "alpha\nbeta\ngamma\nbeta delta\n")
        (define r (tool-grep (hasheq 'pattern "beta"
                                     'path (path->string f))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 2)
        (check-equal? (hash-ref d 'files-with-matches) 1)
        (check-false (hash-ref d 'truncated?))
        (define c (string-join (result-content r) "\n"))
        (check-true (regexp-match? #rx"sample\\.txt:2: beta" c))
        (check-true (regexp-match? #rx"sample\\.txt:4: beta delta" c)))))

   ;; 2. Search in a single file, no matches
   (test-case "single file with no matches"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "sample.txt"))
        (write-string-to-file f "alpha\ngamma\ndelta\n")
        (define r (tool-grep (hasheq 'pattern "xyz"
                                     'path (path->string f))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 0)
        (check-equal? (hash-ref d 'files-with-matches) 0)
        (check-equal? (result-content r) '()))))

   ;; 3. Search with case-insensitive flag
   (test-case "case-insensitive search"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "mixed.txt"))
        (write-string-to-file f "Hello World\nhello world\nHELLO WORLD\nfoo\n")
        (define r (tool-grep (hasheq 'pattern "hello"
                                     'path (path->string f)
                                     'case-insensitive? #t)))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 3))))

   ;; 4. Search in directory with glob filter
   (test-case "directory search with glob filter"
     (with-temp-dir
      (λ (dir)
        (write-string-to-file (build-path dir "a.txt") "findme in a\n")
        (write-string-to-file (build-path dir "b.rkt") "findme in b\n")
        (write-string-to-file (build-path dir "c.txt") "no match here\n")
        (define r (tool-grep (hasheq 'pattern "findme"
                                     'path (path->string dir)
                                     'glob "*.txt")))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 1)
        (define c (string-join (result-content r) "\n"))
        (check-true (regexp-match? #rx"a\\.txt:1:" c))
        (check-false (regexp-match? #rx"b\\.rkt" c)))))

   ;; 5. Context lines included around matches
   (test-case "context lines around matches"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "ctx.txt"))
        (write-string-to-file f "line1\nline2\nTARGET\nline4\nline5\n")
        (define r (tool-grep (hasheq 'pattern "TARGET"
                                     'path (path->string f)
                                     'context-lines 1)))
        (check-false (result-is-error? r))
        (define c (string-join (result-content r) "\n"))
        (check-true (regexp-match? #rx"ctx\\.txt:2:" c))
        (check-true (regexp-match? #rx"ctx\\.txt:3: TARGET" c))
        (check-true (regexp-match? #rx"ctx\\.txt:4:" c))
        (check-false (regexp-match? #rx"ctx\\.txt:1:" c))
        (check-false (regexp-match? #rx"ctx\\.txt:5:" c)))))

   ;; 6. Max results limits output
   (test-case "max-results limits output"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "many.txt"))
        (define lines (for/list ([i (in-range 1 21)])
                        (format "~a\n" (if (member i (list 2 5 8 11 14 17 20))
                                           (format "match line ~a" i)
                                           (format "plain line ~a" i)))))
        (write-string-to-file f (string-join lines ""))
        (define r (tool-grep (hasheq 'pattern "match"
                                     'path (path->string f)
                                     'max-results 3
                                     'context-lines 0)))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 7)
        (check-true (hash-ref d 'truncated?))
        (define match-lines (filter (lambda (l) (regexp-match? #rx"match line" l))
                                    (result-content r)))
        (check-equal? (length match-lines) 3))))

   ;; 7. Binary file is skipped
   (test-case "binary file is skipped silently"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "binary.dat"))
        (call-with-output-file f
          (lambda (out)
            (write-bytes #"\x00\x01\x02\xff\xfe\xfd" out))
          #:exists 'replace)
        (define r (tool-grep (hasheq 'pattern "anything"
                                     'path (path->string f))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 0)
        (check-equal? (hash-ref d 'files-with-matches) 0)
        (check-equal? (result-content r) '()))))

   ;; 8. Non-existent path returns error
   (test-case "non-existent path returns error"
     (define r (tool-grep (hasheq 'pattern "foo"
                                  'path "/tmp/q-no-such-path-xyz-999")))
     (check-true (result-is-error? r))
     (define c (string-join (result-content r) ""))
     (check-true (regexp-match? #rx"[Nn]ot [Ff]ound" c)))

   ;; 9. Missing pattern returns error
   (test-case "missing pattern returns error"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "x.txt"))
        (write-string-to-file f "content\n")
        (define r (tool-grep (hasheq 'path (path->string f))))
        (check-true (result-is-error? r))
        (define c (string-join (result-content r) ""))
        (check-true (regexp-match? #rx"[Pp]attern" c)))))

   ;; 10. Hidden files/directories are skipped
   (test-case "hidden files and directories are skipped"
     (with-temp-dir
      (λ (dir)
        (write-string-to-file (build-path dir "visible.txt") "findme visible\n")
        (write-string-to-file (build-path dir ".hidden.txt") "findme hidden\n")
        (define hidden-dir (build-path dir ".secrets"))
        (make-directory hidden-dir)
        (write-string-to-file (build-path hidden-dir "data.txt") "findme secret\n")
        (define r (tool-grep (hasheq 'pattern "findme"
                                     'path (path->string dir))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 1)
        (define c (string-join (result-content r) "\n"))
        (check-true (regexp-match? #rx"visible\\.txt" c))
        (check-false (regexp-match? #rx"hidden" c))
        (check-false (regexp-match? #rx"secret" c)))))

   ;; 11. VCS directories are skipped
   (test-case "VCS directories are skipped"
     (with-temp-dir
      (λ (dir)
        (write-string-to-file (build-path dir "code.txt") "findme code\n")
        (define git-dir (build-path dir ".git"))
        (make-directory git-dir)
        (write-string-to-file (build-path git-dir "config") "findme git\n")
        (define nm-dir (build-path dir "node_modules"))
        (make-directory nm-dir)
        (write-string-to-file (build-path nm-dir "package.js") "findme node\n")
        (define r (tool-grep (hasheq 'pattern "findme"
                                     'path (path->string dir))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 1)
        (define c (string-join (result-content r) "\n"))
        (check-true (regexp-match? #rx"code\\.txt" c))
        (check-false (regexp-match? #rx"\\.git" c))
        (check-false (regexp-match? #rx"node_modules" c)))))

   ;; 12. Empty file returns success with 0 matches
   (test-case "empty file returns success with 0 matches"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "empty.txt"))
        (write-string-to-file f "")
        (define r (tool-grep (hasheq 'pattern "anything"
                                     'path (path->string f))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 0)
        (check-equal? (hash-ref d 'files-with-matches) 0)
        (check-equal? (result-content r) '()))))

   ;; Additional: regex pattern support
   (test-case "regex pattern works"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "regex.txt"))
        (write-string-to-file f "foo123bar\nfoo456bar\nnomatch\nfoo78bar\n")
        (define r (tool-grep (hasheq 'pattern "foo[0-9]+bar"
                                     'path (path->string f))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 3))))

   ;; Additional: recursive directory search
   (test-case "recursive directory search finds nested files"
     (with-temp-dir
      (λ (dir)
        (write-string-to-file (build-path dir "top.txt") "target top\n")
        (define sub (build-path dir "sub"))
        (make-directory sub)
        (write-string-to-file (build-path sub "nested.txt") "target nested\n")
        (define deep (build-path sub "deep"))
        (make-directory deep)
        (write-string-to-file (build-path deep "deep.txt") "target deep\n")
        (define r (tool-grep (hasheq 'pattern "target"
                                     'path (path->string dir))))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'total-matches) 3)
        (check-equal? (hash-ref d 'files-with-matches) 3))))

   ;; Additional: context lines = 0 means no context
   (test-case "context-lines 0 means no context"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "noctx.txt"))
        (write-string-to-file f "before\nMATCH\nafter\n")
        (define r (tool-grep (hasheq 'pattern "MATCH"
                                     'path (path->string f)
                                     'context-lines 0)))
        (check-false (result-is-error? r))
        (define c (string-join (result-content r) "\n"))
        (check-true (regexp-match? #rx"noctx\\.txt:2: MATCH" c))
        (check-false (regexp-match? #rx"noctx\\.txt:1:" c))
        (check-false (regexp-match? #rx"noctx\\.txt:3:" c)))))
   ))

;; ============================================================
;; Run
;; ============================================================

(run-tests grep-tests)
