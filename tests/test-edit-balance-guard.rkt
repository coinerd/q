#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; W1 regression tests for v0.99.79 structural-split safety and
;; per-call max-old-text-len override.

(require rackunit
         racket/file
         racket/string
         "../tools/builtins/edit.rkt"
         "../tools/tool.rkt")

(define (result-message result)
  (define content (car (tool-result-content result)))
  (if (hash? content)
      (hash-ref content 'text)
      content))

(define (with-temp-rkt content proc)
  (define path (make-temporary-file "q-edit-balance-~a.rkt"))
  (dynamic-wind (lambda () (display-to-file content path #:exists 'replace))
                (lambda () (proc path))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "balance guard flags depth-increasing partial-form edit"
  (with-temp-rkt
   "#lang racket/base\n(define answer (+ x 1))\n"
   (lambda (path)
     ;; Depth-changing edit: W0 parse check rejects it, and the balance
     ;; guard adds routing guidance to the error message.
     (define result (tool-edit (hasheq 'path path 'old-text "(+ x 1)" 'new-text "(begin (+ x 1)")))
     (check-true (tool-result-is-error? result) "depth-changing edit should be rejected")
     (check-equal? (file->string path) "#lang racket/base\n(define answer (+ x 1))\n")
     (define msg (result-message result))
     (check-true (string-contains? msg "depth") "warning should mention depth")
     (check-true (string-contains? msg "whole-form") "warning should route to whole-form replacement")
     (check-true (string-contains? msg "structural") "warning should mention structural edit tool"))))

(test-case "balance guard does not flag depth-neutral edit"
  (with-temp-rkt "#lang racket/base\n(define answer 41)\n"
                 (lambda (path)
                   (define result (tool-edit (hasheq 'path path 'old-text "41" 'new-text "42")))
                   (check-false (tool-result-is-error? result))
                   (check-false (string-contains? (result-message result) "depth")))))

(test-case "balance guard ignores non-Racket files"
  (define path (make-temporary-file "q-edit-balance-~a.txt"))
  (dynamic-wind (lambda () (display-to-file "a (b) c" path #:exists 'replace))
                (lambda ()
                  (define result
                    (tool-edit (hasheq 'path path 'old-text "(b)" 'new-text "(begin (b)")))
                  (check-false (tool-result-is-error? result))
                  (check-false (string-contains? (result-message result) "depth")))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "default max-old-text-len is still 2000"
  (define original (format "#lang racket/base\n(define long-thing ~a)\n" (make-string 2001 #\x)))
  (with-temp-rkt
   original
   (lambda (path)
     (define old-text (make-string 2001 #\x))
     (define result (tool-edit (hasheq 'path path 'old-text old-text 'new-text "\"short\"")))
     (check-true (tool-result-is-error? result))
     (define msg (result-message result))
     (check-true (string-contains? msg "2000") "default limit should be reported")
     (check-true (string-contains? msg "max-old-text-len") "error should route to override")
     (check-true (string-contains? msg "do not split") "error should warn against partial splits")
     (check-equal? (file->string path) original))))

(test-case "per-call max-old-text-len override allows whole-form replacement"
  (define original (format "#lang racket/base\n(define long-thing ~a)\n" (make-string 600 #\x)))
  (with-temp-rkt
   original
   (lambda (path)
     (define old-text (make-string 600 #\x))
     (define result
       (tool-edit (hasheq 'path path 'old-text old-text 'new-text "\"short\"" 'max-old-text-len 600)))
     (check-false (tool-result-is-error? result))
     (check-equal? (file->string path) "#lang racket/base\n(define long-thing \"short\")\n"))))

(test-case "per-call max-old-text-len above safe ceiling is rejected"
  (define path (make-temporary-file "q-edit-balance-~a.rkt"))
  (dynamic-wind
   (lambda () (display-to-file "#lang racket/base\n(define x 1)\n" path #:exists 'replace))
   (lambda ()
     (define result
       (tool-edit (hasheq 'path path 'old-text "1" 'new-text "2" 'max-old-text-len 2001)))
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (result-message result) "2000")))
   (lambda ()
     (when (file-exists? path)
       (delete-file path)))))

(test-case "invalid max-old-text-len type is rejected"
  (define path (make-temporary-file "q-edit-balance-~a.rkt"))
  (dynamic-wind
   (lambda () (display-to-file "#lang racket/base\n(define x 1)\n" path #:exists 'replace))
   (lambda ()
     (define result
       (tool-edit (hasheq 'path path 'old-text "1" 'new-text "2" 'max-old-text-len "600")))
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (result-message result) "max-old-text-len")))
   (lambda ()
     (when (file-exists? path)
       (delete-file path)))))

(test-case "balance guard is string-aware and ignores parens inside strings"
  (with-temp-rkt "#lang racket/base\n(define s \"a\")\n"
                 (lambda (path)
                   (define result
                     (tool-edit (hasheq 'path path 'old-text "\"a\"" 'new-text "\"a (b\"")))
                   (check-false (tool-result-is-error? result))
                   (check-false (string-contains? (result-message result) "depth"))
                   (check-equal? (file->string path) "#lang racket/base\n(define s \"a (b\")\n"))))
