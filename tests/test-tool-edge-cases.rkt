#lang racket

;; test-tool-edge-cases.rkt — Edge case tests for tool-write and tool-read (#259)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../tools/builtins/read.rkt"
         "../tools/builtins/write.rkt"
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?))

;; ============================================================
;; Helpers
;; ============================================================

(define (with-temp-dir thunk)
  (define d (make-temporary-file "q-test-edge-~a" 'directory))
  (dynamic-wind void
                (λ () (thunk d))
                (λ ()
                  (with-handlers ([exn:fail? void])
                    (delete-directory/files d)))))

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

;; ============================================================
;; WRITE edge cases
;; ============================================================

(define write-edge-tests
  (test-suite "tool-write edge cases"

    (test-case "write content just under 1MB limit succeeds"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "large.txt"))
                       (define size (sub1 (current-max-write-bytes)))
                       (define content (make-string size #\A))
                       (define r (tool-write (hasheq 'path (path->string f) 'content content)))
                       (check-false (result-is-error? r))
                       (check-equal? (file-size f) size))))

    (test-case "write content exactly at 1MB limit succeeds"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "exact.txt"))
                       (define size (current-max-write-bytes))
                       (define content (make-string size #\B))
                       (define r (tool-write (hasheq 'path (path->string f) 'content content)))
                       (check-false (result-is-error? r))
                       (check-equal? (file-size f) size))))

    (test-case "write content exceeding 1MB limit is rejected"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "too-big.txt"))
                       (define size (add1 (current-max-write-bytes)))
                       (define content (make-string size #\C))
                       (define r (tool-write (hasheq 'path (path->string f) 'content content)))
                       (check-true (result-is-error? r))
                       (define c (string-join (result-content r) ""))
                       (check-true (regexp-match? #rx"exceeds limit" c)))))

    (test-case "write multi-byte UTF-8 content works"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "utf8.txt"))
                       (define content "Hello world unicode")
                       (define r (tool-write (hasheq 'path (path->string f) 'content content)))
                       (check-false (result-is-error? r))
                       (check-equal? (file->string f) content))))))

;; ============================================================
;; READ edge cases
;; ============================================================

(define read-edge-tests
  (test-suite "tool-read edge cases"

    (test-case "read empty file returns 0 total lines"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "empty.txt"))
                       (call-with-output-file f void #:exists 'replace)
                       (define r (tool-read (hasheq 'path (path->string f))))
                       (check-false (result-is-error? r))
                       (check-equal? (hash-ref (result-details r) 'total-lines) 0))))

    (test-case "read binary file returns error"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "binary.dat"))
                       (call-with-output-file f
                                              (lambda (out)
                                                (write-bytes #"\x00\x01\x02\xff\xfe\xfd" out))
                                              #:exists 'replace)
                       (define r (tool-read (hasheq 'path (path->string f))))
                       (check-true (result-is-error? r))
                       (define c (string-join (result-content r) ""))
                       (check-true (regexp-match? #rx"[Bb]inary" c)))))

    (test-case "read file with UTF-8 BOM works"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "bom.txt"))
                       (define bom-bytes (bytes #xEF #xBB #xBF))
                       (call-with-output-file f
                                              (lambda (out)
                                                (write-bytes bom-bytes out)
                                                (display "Hello BOM" out))
                                              #:exists 'replace)
                       (define r (tool-read (hasheq 'path (path->string f))))
                       (check-false (result-is-error? r))
                       (define c (string-join (result-content r) ""))
                       (check-true (regexp-match? #rx"Hello BOM" c)))))

    (test-case "read file with very long single line"
      (with-temp-dir
       (λ (dir)
         (define f (build-path dir "longline.txt"))
         (define long-line (make-string 50000 #\X))
         (call-with-output-file f (lambda (out) (display long-line out)) #:exists 'replace)
         (define r (tool-read (hasheq 'path (path->string f))))
         (check-false (result-is-error? r))
         (define c (string-join (result-content r) ""))
         (check-true (regexp-match? #rx"X" c)))))

    (test-case "read file with mixed line endings"
      (with-temp-dir (λ (dir)
                       (define f (build-path dir "mixed-endings.txt"))
                       (call-with-output-file f
                                              (lambda (out)
                                                (write-bytes #"line1\nline2\r\nline3\n" out))
                                              #:exists 'replace)
                       (define r (tool-read (hasheq 'path (path->string f))))
                       (check-false (result-is-error? r))
                       (check-true (>= (hash-ref (result-details r) 'total-lines) 2)))))

    (test-case "read file containing only whitespace"
      (with-temp-dir
       (λ (dir)
         (define f (build-path dir "whitespace.txt"))
         (call-with-output-file f (lambda (out) (display "   \n\n\t\n  " out)) #:exists 'replace)
         (define r (tool-read (hasheq 'path (path->string f))))
         (check-false (result-is-error? r))
         (check-true (>= (hash-ref (result-details r) 'total-lines) 1)))))

    (test-case "read file with Unicode preserves content"
      (with-temp-dir
       (λ (dir)
         (define f (build-path dir "unicode.txt"))
         (define content "Héllo wörld unicode test\nLine two")
         (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace)
         (define r (tool-read (hasheq 'path (path->string f))))
         (check-false (result-is-error? r))
         (define c (string-join (result-content r) ""))
         (check-true (regexp-match? #rx"Héllo" c)))))))

;; ============================================================
;; Run all
;; ============================================================

(run-tests (test-suite "tool-edge-cases"
             write-edge-tests
             read-edge-tests))
