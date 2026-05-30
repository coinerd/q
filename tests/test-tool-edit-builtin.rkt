#lang racket/base

;; tests/test-tool-edit-builtin.rkt — T-2 tests for tools/builtins/edit.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../tools/tool.rkt" tool-result? tool-result-content tool-result-is-error?)
         (only-in "../tools/builtins/edit.rkt" tool-edit))

(define (call-edit args)
  (tool-edit args #f))

(define (error-text result)
  (define c (tool-result-content result))
  (if (and (pair? c) (hash? (car c)))
      (hash-ref (car c) 'text "")
      (format "~a" c)))

(define (make-temp-file content)
  (define path (make-temporary-file "q-edit-test-~a"))
  (call-with-output-file path #:exists 'truncate
    (lambda (p) (display content p)))
  path)

(define (cleanup-path p)
  (when (file-exists? p) (delete-file p)))

(define edit-suite
  (test-suite
   "Edit builtin tests"

   (test-case "exact unique replacement succeeds"
     (define p (make-temp-file "hello world\nfoo bar\n"))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-edit (hasheq 'path p
                                           'old-text "foo bar"
                                           'new-text "baz qux")))
         (check-pred tool-result? result)
         (check-false (tool-result-is-error? result))
         (check-true (string-contains? (file->string p) "baz qux"))
         (check-false (string-contains? (file->string p) "foo bar")))
       (lambda () (cleanup-path p))))

   (test-case "multiple matches returns error"
     (define p (make-temp-file "aaa\naaa\n"))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-edit (hasheq 'path p
                                           'old-text "aaa"
                                           'new-text "bbb")))
         (check-pred tool-result? result)
         (check-true (tool-result-is-error? result))
         (check-true (string-contains? (error-text result) "times")))
       (lambda () (cleanup-path p))))

   (test-case "edit non-existent file returns error"
     (define result (call-edit (hasheq 'path "/tmp/nonexistent-q-edit-xyz"
                                       'old-text "old"
                                       'new-text "new")))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "not found")))

   (test-case "edit with no old-text returns error"
     (define result (call-edit (hasheq 'path "/tmp/fake" 'new-text "new")))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "old-text")))

   (test-case "edit with no path returns error"
     (define result (call-edit (hasheq 'old-text "old" 'new-text "new")))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "path")))

   (test-case "edit with no new-text returns error"
     (define result (call-edit (hasheq 'path "/tmp/fake" 'old-text "old")))
     (check-pred tool-result? result)
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (error-text result) "new-text")))

   (test-case "no match for old-text returns error"
     (define p (make-temp-file "hello world\n"))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define result (call-edit (hasheq 'path p
                                           'old-text "nonexistent text"
                                           'new-text "replacement")))
         (check-pred tool-result? result)
         (check-true (tool-result-is-error? result))
         (define txt (error-text result))
         (check-true (string-contains? txt "0")))
       (lambda () (cleanup-path p))))
   ))

(run-tests edit-suite)
