#lang racket/base

;; extensions/racket-tooling/analysis.rkt — static analysis coordination
;;
;; Extracted from racket-tooling-handlers.rkt.
;; handle-racket-check: format, syntax, test, expand, and all-mode checks.

(require racket/string
         (only-in "../../util/errors.rkt" raise-extension-error)
         racket/list
         json
         (only-in "../racket-tooling-helpers.rkt"
                  raco-fmt raco-make raco-test raco-expand)
         (only-in "../tool-api.rkt" make-success-result))

(provide handle-racket-check)

(define (handle-racket-check args [exec-ctx #f])
  (define path (hash-ref args 'path ""))
  (define mode (hash-ref args 'mode "syntax"))
  (when (string=? path "")
    (raise-extension-error "path is required" 'racket-tooling 'check))
  (unless (file-exists? path)
    (raise-extension-error (format "File not found: ~a" path) 'racket-tooling 'check))

  (define results '())

  (when (member mode '("format" "all"))
    (define-values (ec out err) (raco-fmt path))
    (set! results
          (cons (hasheq 'step "format" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
                results)))

  (when (member mode '("syntax" "all"))
    (define-values (ec out err) (raco-make path))
    (set! results
          (cons (hasheq 'step "compile" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
                results)))

  (when (member mode '("test" "all"))
    (define-values (ec out err) (raco-test path))
    (set! results
          (cons (hasheq 'step "test" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
                results)))

  (when (member mode '("expand" "all"))
    (define-values (ec out err) (raco-expand path))
    (set! results
          (cons (hasheq 'step "expand" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
                results)))

  (when (null? results)
    (define-values (ec out err) (raco-make path))
    (set! results
          (list (hasheq 'step "compile" 'pass? (zero? ec) 'output (string-trim (string-append out err))))))

  (define all-pass? (andmap (lambda (r) (hash-ref r 'pass? #f)) results))
  (make-success-result
   (list (hasheq 'type "text"
                 'text (jsexpr->string
                        (hasheq 'file path 'all-pass? all-pass? 'results (reverse results)))))))
