#lang racket/base
;; Pure C0 -> C1 relation; no command execution, snapshot mutation or authority.
(require racket/list
         racket/string
         "review-proof-format.rkt")
(provide make-effective-verify-command
         effective-verify-superset?)

;; Deliberately narrower than a shell grammar. Every path component is literal,
;; relative and nonempty; dot components, flags, escapes and operators refuse.
(define (source-path? p)
  (and (string? p)
       (regexp-match? #px"^[A-Za-z0-9_][A-Za-z0-9_./-]*[.]rkt$" p)
       (for/and ([part (in-list (string-split p "/" #:trim? #f))])
         (and (not (member part '("" "." ".."))) (not (string-prefix? part "-"))))))
(define (test-path? p)
  (and (source-path? p) (string-prefix? p "tests/")))
(define (path-stage? stage prefix path?)
  (and (string-prefix? stage prefix)
       (let ([args (string-split (substring stage (string-length prefix)) " " #:trim? #f)])
         (and (pair? args) (andmap path? args)))))
(define (approved-stages command)
  (define stages (owned-chain-stages command))
  (and stages
       (= (length stages) 5)
       (proof-verify-command-eligible? (string-join stages " && "))
       (path-stage? (list-ref stages 0) "raco fmt -i " source-path?)
       (path-stage? (list-ref stages 1) "raco make " source-path?)
       (path-stage? (list-ref stages 2) "racket scripts/run-tests.rkt " test-path?)
       (owned-red-focused-command? (list-ref stages 2))
       (member (list-ref stages 3)
               '("racket scripts/run-tests.rkt --suite fast"
                 "racket scripts/run-tests.rkt --suite fast --jobs 2"))
       (member (list-ref stages 4)
               '("racket scripts/lint-all.rkt" "racket scripts/lint-format.rkt"
                                               "racket scripts/metrics.rkt"))
       stages))
(define (make-effective-verify-command base-command test-path)
  (define stages (approved-stages base-command))
  (unless (and stages (test-path? test-path))
    (error 'make-effective-verify-command "unsupported base Verify chain or test path"))
  (define focused (list-ref stages 2))
  (when (member test-path (string-split focused))
    (error 'make-effective-verify-command "test already present in focused stage"))
  (string-join (list-set stages 2 (string-append focused " " test-path)) " && "))
(define (effective-verify-superset? base-command effective-command test-path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define expected (make-effective-verify-command base-command test-path))
    (define actual (approved-stages effective-command))
    (and actual (equal? actual (owned-chain-stages expected)))))
