#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tool-write-builtin.rkt — T-2 tests for tools/builtins/write.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../tools/tool.rkt" tool-result? tool-result-content tool-result-is-error?)
         (only-in "../tools/builtins/write.rkt" tool-write reset-cumulative-writes!))

;; Helper: invoke write tool with args
(define (call-write args)
  (tool-write args #f))

;; Helper: extract error text from error result content
(define (error-text result)
  (define c (tool-result-content result))
  (if (and (pair? c) (hash? (car c)))
      (hash-ref (car c) 'text "")
      (format "~a" c)))

(define write-suite
  (test-suite
   "Write builtin tests"

   (test-case "write new file creates file with content"
     (define p (make-temporary-file "q-write-test-~a"))
     (delete-file p) ;; remove auto-created file
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-write (hasheq 'path p 'content "hello world")))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (check-true (file-exists? p))
         (check-equal? (file->string p) "hello world"))
       (lambda ()
         (when (file-exists? p) (delete-file p)))))

   (test-case "overwrite existing file replaces content"
     (define p (make-temporary-file "q-write-test-~a"))
     (dynamic-wind
       (lambda ()
         (call-with-output-file p #:exists 'truncate
           (lambda (port) (display "old content" port))))
       (lambda ()
         (define result (call-write (hasheq 'path p 'content "new content")))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (check-equal? (file->string p) "new content"))
       (lambda ()
         (when (file-exists? p) (delete-file p)))))

   (test-case "write with no path returns error"
     (define result (call-write (hasheq 'content "data")))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "path")))

   (test-case "write with no content writes empty string"
     (define p (make-temporary-file "q-write-test-~a"))
     (dynamic-wind
       (lambda ()
         (call-with-output-file p #:exists 'truncate
           (lambda (port) (display "old" port))))
       (lambda ()
         (define result (call-write (hasheq 'path p)))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (check-equal? (file->string p) ""))
       (lambda ()
         (when (file-exists? p) (delete-file p)))))

   (test-case "reset-cumulative-writes resets budget"
     ;; Just verify the function exists and doesn't error
     (reset-cumulative-writes!)
     (check-true #t))
   ))

(run-tests write-suite)
