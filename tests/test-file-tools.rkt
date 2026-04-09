#lang racket

;; test-file-tools.rkt — tests for read.rkt, write.rkt, edit.rkt
;; Uses temporary files for isolation. No external dependencies.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../tools/builtins/read.rkt"
         "../tools/builtins/write.rkt"
         "../tools/builtins/edit.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (with-temp-dir thunk)
  (define d (make-temporary-file "q-test-~a" 'directory))
  (dynamic-wind
    void
    (λ () (thunk d))
    (λ ()
      (with-handlers ([exn:fail? void])
        (delete-directory/files d)))))

(define (temp-file-path dir [name "testfile.txt"])
  (build-path dir name))

(define (write-string-to-file path content)
  (call-with-output-file path
    (lambda (out) (display content out))
    #:exists 'replace))

;; ============================================================
;; Tool result helpers — tools now return tool-result structs
;; ============================================================

(require (only-in "../tools/tool.rkt"
                  tool-result? tool-result-content tool-result-details tool-result-is-error?))

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
;; READ tests
;; ============================================================

(define read-tests
  (test-suite
   "read-tool"

   (test-case "read existing file returns content with line numbers"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "line one\nline two\nline three\n")
        (define r (tool-read (hasheq 'path (path->string f))))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'total-lines) 3)
        (check-equal? (hash-ref (result-details r) 'start-line) 1)
        (check-equal? (hash-ref (result-details r) 'end-line) 3)
        (define c (string-join (result-content r) ""))
        (check-true (regexp-match? #rx"1\\|" c))
        (check-true (regexp-match? #rx"line one" c)))))

   (test-case "read with offset and limit"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "aaa\nbbb\nccc\nddd\neee\n")
        (define r (tool-read (hasheq 'path (path->string f)
                                     'offset 2
                                     'limit 3)))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'start-line) 2)
        (check-equal? (hash-ref (result-details r) 'end-line) 4)
        (check-equal? (hash-ref (result-details r) 'total-lines) 5)
        (define c (string-join (result-content r) ""))
        (check-true (regexp-match? #rx"bbb" c))
        (check-true (regexp-match? #rx"ccc" c))
        (check-true (regexp-match? #rx"ddd" c))
        (check-false (regexp-match? #rx"aaa" c))
        (check-false (regexp-match? #rx"eee" c)))))

   (test-case "read non-existent file returns error"
     (define r (tool-read (hasheq 'path "/tmp/q-no-such-file-xyz-999.txt")))
     (check-true (result-is-error? r))
     (define c (string-join (result-content r) ""))
     (check-true (regexp-match? #rx"not found" c)))

   (test-case "read file exceeding default line limit is bounded"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (call-with-output-file f
          (lambda (out)
            (for ([i (in-range 1 3001)])
              (fprintf out "line ~a\n" i)))
          #:exists 'replace)
        (define r (tool-read (hasheq 'path (path->string f))))
        (check-false (result-is-error? r))
        (define end (hash-ref (result-details r) 'end-line))
        (define total (hash-ref (result-details r) 'total-lines))
        (check-equal? total 3000)
        (check-true (< end total) "output should be bounded"))))

   (test-case "read empty file returns empty content with 0 total lines"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "")
        (define r (tool-read (hasheq 'path (path->string f))))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'total-lines) 0)
        (check-equal? (result-content r) '()))))

   (test-case "read single-line file without trailing newline"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "only line")
        (define r (tool-read (hasheq 'path (path->string f))))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'total-lines) 1)
        (check-equal? (hash-ref (result-details r) 'start-line) 1)
        (check-equal? (hash-ref (result-details r) 'end-line) 1))))

   (test-case "read binary file returns error"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "binary.dat"))
        (call-with-output-file f
          (lambda (out)
            (write-bytes #"\x00\x01\x02\xff\xfe\xfd" out))
          #:exists 'replace)
        (define r (tool-read (hasheq 'path (path->string f))))
        (check-true (result-is-error? r))
        (define c (string-join (result-content r) ""))
        (check-true (or (regexp-match? #rx"[Bb]inary" c))))))
   ))

;; ============================================================
;; WRITE tests
;; ============================================================

(define write-tests
  (test-suite
   "write-tool"

   (test-case "write creates new file with content"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir "new.txt"))
        (define content "hello world\nsecond line")
        (define r (tool-write (hasheq 'path (path->string f)
                                      'content content)))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'bytes-written)
                      (string-length content))
        (check-equal? (file->string f) content))))

   (test-case "write creates parent directories"
     (with-temp-dir
      (λ (dir)
        (define f (build-path dir "sub" "dir" "file.txt"))
        (define r (tool-write (hasheq 'path (path->string f)
                                      'content "nested content")))
        (check-false (result-is-error? r))
        (check-true (file-exists? f))
        (check-equal? (file->string f) "nested content"))))

   (test-case "write overwrites existing file"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "old content")
        (define r (tool-write (hasheq 'path (path->string f)
                                      'content "new content")))
        (check-false (result-is-error? r))
        (check-equal? (file->string f) "new content"))))

   (test-case "write to read-only location returns error"
     (define r (tool-write (hasheq 'path "/dev/null/impossible/path.txt"
                                   'content "fail")))
     (check-true (result-is-error? r)))

   (test-case "write empty string creates empty file"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir "empty.txt"))
        (define r (tool-write (hasheq 'path (path->string f)
                                      'content "")))
        (check-false (result-is-error? r))
        (check-equal? (file-size f) 0))))

   (test-case "write returns path in details"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir "path-test.txt"))
        (define r (tool-write (hasheq 'path (path->string f)
                                      'content "x")))
        (check-false (result-is-error? r))
        (check-equal? (hash-ref (result-details r) 'path)
                      (path->string f)))))
   ))

;; ============================================================
;; EDIT tests
;; ============================================================

(define edit-tests
  (test-suite
   "edit-tool"

   (test-case "edit replaces exact match"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "foo bar baz")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text "bar"
                                     'new-text "QUX")))
        (check-false (result-is-error? r))
        (check-equal? (file->string f) "foo QUX baz"))))

   (test-case "edit with old-text not found returns error"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "hello world")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text "xyz"
                                     'new-text "abc")))
        (check-true (result-is-error? r))
        (define c (string-join (result-content r) ""))
        (check-true (regexp-match? #rx"not found" c))
        (check-equal? (file->string f) "hello world"))))

   (test-case "edit with ambiguous match returns error with count"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "aaa bbb aaa bbb aaa")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text "aaa"
                                     'new-text "XXX")))
        (define is-err (result-is-error? r))
        (check-true is-err)
        (define c (string-join (result-content r) ""))
        (check-true (regexp-match? #rx"more specific" c))
        (check-equal? (file->string f) "aaa bbb aaa bbb aaa"))))

   (test-case "edit with multi-line old-text"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "line1\nline2\nline3\nline4")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text "line2\nline3"
                                     'new-text "REPLACED")))
        (check-false (result-is-error? r))
        (check-equal? (file->string f) "line1\nREPLACED\nline4"))))

   (test-case "edit on non-existent file returns error"
     (define r (tool-edit (hasheq 'path "/tmp/q-no-such-file-edit-xyz.txt"
                                  'old-text "x"
                                  'new-text "y")))
     (check-true (result-is-error? r))
     (define c (string-join (result-content r) ""))
     (check-true (regexp-match? #rx"not found" c)))

   (test-case "edit returns diff info in details"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "the quick brown fox")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text "quick brown"
                                     'new-text "slow white")))
        (check-false (result-is-error? r))
        (define d (result-details r))
        (check-equal? (hash-ref d 'replacements) 1)
        (check-equal? (hash-ref d 'old-length) (string-length "quick brown"))
        (check-equal? (hash-ref d 'new-length) (string-length "slow white"))
        (check-true (hash-has-key? d 'path)))))

   (test-case "edit with old-text equal to new-text is still valid"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "unchanged")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text "unchanged"
                                     'new-text "unchanged")))
        (check-false (result-is-error? r))
        (check-equal? (file->string f) "unchanged"))))

   (test-case "edit empty file with empty old-text is ambiguous/error"
     (with-temp-dir
      (λ (dir)
        (define f (temp-file-path dir))
        (write-string-to-file f "some content")
        (define r (tool-edit (hasheq 'path (path->string f)
                                     'old-text ""
                                     'new-text "x")))
        (check-true (result-is-error? r)))))
   ))

;; ============================================================
;; Run all
;; ============================================================

(run-tests
 (test-suite
  "file-tools"
  read-tests
  write-tests
  edit-tests))
