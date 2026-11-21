#lang racket

;; @covers extensions/gsd/supplemental-disposition-decision.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit
;;
;; Purity gate for the CL4 decision core. Two independent checks:
;;   1. a static source audit — the module's only `require` is `racket/base`, so
;;      its transitive import closure is exactly that, and no effectful
;;      identifier appears anywhere in the code;
;;   2. a CL2 navigator cross-check — the derived static index reports no dynamic
;;      require edge and no non-`racket/base` module edge for this file.
;; A failure here means the core stopped being a pure decision and must not be
;; extended with adapter work.

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         (only-in "../scripts/semantic-navigator.rkt" module-query))

(define-runtime-path repo-root "..")
(define core-module "extensions/gsd/supplemental-disposition-decision.rkt")

(define (read-source-forms path)
  ;; The `#lang` line is not a readable datum inside another module; the body is.
  (define text (regexp-replace #rx"^#lang[^\n]*\n" (file->string path) ""))
  (call-with-input-string text
                          (lambda (in)
                            (let loop ([acc '()])
                              (define datum (read in))
                              (if (eof-object? datum)
                                  (reverse acc)
                                  (loop (cons datum acc)))))))

(define (top-level-requires forms)
  (for/list ([form (in-list forms)]
             #:when (and (list? form) (pair? form) (eq? (car form) 'require)))
    (cdr form)))

(define (collect-symbols datum)
  (cond
    [(symbol? datum) (list datum)]
    [(pair? datum) (append (collect-symbols (car datum)) (collect-symbols (cdr datum)))]
    [(vector? datum) (append-map collect-symbols (vector->list datum))]
    [else '()]))

(define forbidden-identifiers
  '(dynamic-require system
                    system*
                    system*/exit-code
                    subprocess
                    subprocess*
                    current-directory
                    current-output-port
                    current-error-port
                    current-input-port
                    open-input-file
                    open-output-file
                    call-with-output-file
                    file->string
                    file->bytes
                    make-parameter
                    current-seconds
                    current-inexact-milliseconds))

(define (query)
  (module-query core-module #:root repo-root))

(test-case "the core has no explicit require beyond its racket/base language"
  (define path (build-path repo-root core-module))
  (check-true (and (regexp-match? #rx"^#lang racket/base\n" (file->string path)) #t))
  (define forms (read-source-forms path))
  ;; `#lang racket/base` is the module's only import; any explicit require would
  ;; widen the transitive closure this gate is protecting.
  (check-equal? (top-level-requires forms) '())
  (check-false (ormap (lambda (form) (and (list? form) (pair? form) (eq? (car form) 'module+)))
                      forms)))

(test-case "the core uses no effectful identifier"
  (define forms (read-source-forms (build-path repo-root core-module)))
  (define hits
    (filter (lambda (s) (memq s forbidden-identifiers)) (append-map collect-symbols forms)))
  (check-equal? hits '()))

(test-case "the derived static index reports no effectful or dynamic edge"
  (define q (query))
  (check-equal? (hash-ref q 'dynamic-requires '()) '())
  (for ([spec (in-list (hash-ref q 'requires '()))])
    (check-true (equal? spec "racket/base") (format "unexpected require: ~a" spec))))

(test-case "the core exports only views, decisions and the two evaluators"
  (define exports (map (lambda (e) (hash-ref e 'name)) (hash-ref (query) 'exports '())))
  (define allowed
    '(disposition-request-view disposition-amendment-view
                               disposition-terminal-view
                               disposition-campaign-view
                               disposition-context-view
                               disposition-source-view
                               disposition-decision
                               disposition-decision?
                               disposition-decision-status
                               disposition-decision-code
                               disposition-decision-message
                               decide-disposition-identity
                               decide-disposition-terminal
                               decide-disposition-allocation
                               decide-disposition-campaign
                               decide-disposition-context-identity
                               decide-disposition-context-equality
                               decide-disposition-context
                               decide-disposition-source
                               decide-disposition-lineage
                               decide-disposition-evidence))
  (check-equal? (sort exports string<?) (sort (map symbol->string allowed) string<?)))
