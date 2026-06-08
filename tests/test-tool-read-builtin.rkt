#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tool-read-builtin.rkt — T-2 tests for tools/builtins/read.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../tools/tool-struct.rkt" tool-execute)
         (only-in "../tools/tool.rkt" tool-result? tool-result-content tool-result-details tool-result-is-error?)
         (only-in "../tools/builtins/read.rkt" read))

;; Helper: invoke read tool with args
(define (call-read args)
  ((tool-execute read) args #f))

;; Helper: create temp file with content, return path
(define (make-temp-file content)
  (define path (make-temporary-file "q-read-test-~a"))
  (call-with-output-file path #:exists 'truncate
    (lambda (p) (display content p)))
  path)

(define (cleanup-path p)
  (when (file-exists? p) (delete-file p)))

;; Helper: extract error text from error result content
(define (error-text result)
  (define c (tool-result-content result))
  (if (and (pair? c) (hash? (car c)))
      (hash-ref (car c) 'text "")
      (format "~a" c)))

(define read-suite
  (test-suite
   "Read builtin tests"

   (test-case "read entire file returns correct content"
     (define p (make-temp-file "hello world\nline two\n"))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-read (hasheq 'path p)))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (check-equal? (hash-ref (tool-result-details result) 'total-lines) 2))
       (lambda () (cleanup-path p))))

   (test-case "tool-read-builtin: read non-existent file returns error"
     (define result (call-read (hasheq 'path "/tmp/nonexistent-q-test-file-xyz")))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "not found")))

   (test-case "read with offset starts at correct line"
     (define p (make-temp-file "line1\nline2\nline3\nline4\n"))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-read (hasheq 'path p 'offset 3)))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         ;; Content is list of strings, first should contain "3| line3"
         (define content (tool-result-content result))
         (check-true (and (pair? content)
                          (string-contains? (car content) "line3"))))
       (lambda () (cleanup-path p))))

   (test-case "read with limit caps output"
     (define p (make-temp-file "line1\nline2\nline3\nline4\nline5\n"))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-read (hasheq 'path p 'limit 2)))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (define details (tool-result-details result))
         (check-equal? (hash-ref details 'end-line) 2))
       (lambda () (cleanup-path p))))

   (test-case "tool-read-builtin: read binary file returns error"
     (define p (make-temp-file ""))
     (dynamic-wind
       (lambda ()
         (call-with-output-file p #:exists 'truncate
           (lambda (port) (write-bytes #"\x00\x01\x02\x03" port))))
       (lambda ()
         (define result (call-read (hasheq 'path p)))
         (check-pred tool-result? result)
         (check-true (tool-result-is-error? result))
         (check-true (string-contains? (error-text result) "binary")))
       (lambda () (cleanup-path p))))

   (test-case "read empty file returns empty success"
     (define p (make-temp-file ""))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-read (hasheq 'path p)))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (define details (tool-result-details result))
         (check-equal? (hash-ref details 'total-lines) 0))
       (lambda () (cleanup-path p))))

   (test-case "read with no path returns error"
     (define result (call-read (hasheq)))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "Missing")))
   ))

(run-tests read-suite)
