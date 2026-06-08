#lang racket

;; @speed fast
;; @suite default

;; tests/test-contract-metrics.rkt
;; TDD tests for contract-metrics exact counting (only any/c inside contract forms)

(require rackunit
         rackunit/text-ui
         (only-in "../scripts/contract-metrics.rkt"
                  count-anyc-in-file
                  anyc-in-contract-form?))

(define contract-metrics-suite
  (test-suite "contract-metrics-exact"

    ;; ── count-anyc-in-file: basic cases ──

    (test-case "file with no contract-out has zero any/c"
      (define content "#lang racket\n(define x 42)\n")
      (check-equal? (count-anyc-in-file content) 0))

    (test-case "file with contract-out but no any/c has zero"
      (define content "#lang racket\n(provide (contract-out [foo -> string?]))\n")
      (check-equal? (count-anyc-in-file content) 0))

    (test-case "single any/c in contract-out counts as 1"
      (define content "#lang racket\n(provide (contract-out [foo -> any/c]))\n")
      (check-equal? (count-anyc-in-file content) 1))

    (test-case "any/c in comment is NOT counted"
      (define content "#lang racket\n;; any/c should not be counted\n(provide (contract-out [foo -> string?]))\n")
      (check-equal? (count-anyc-in-file content) 0))

    (test-case "any/c in string is NOT counted"
      (define content "#lang racket\n(define doc \"any/c is a contract\")\n(provide (contract-out [foo -> string?]))\n")
      (check-equal? (count-anyc-in-file content) 0))

    (test-case "multiple any/c in contract-out all counted"
      (define content "#lang racket\n(provide (contract-out\n  [foo (-> any/c any/c string?)]\n  [bar (-> any/c)]))\n")
      (check-equal? (count-anyc-in-file content) 3))

    (test-case "any/c outside contract-out NOT counted"
      (define content "#lang racket\n(define (f x) x) ;; x is any/c\n(provide (contract-out [foo -> string?]))\n")
      (check-equal? (count-anyc-in-file content) 0))

    (test-case "any/c in define/contract IS counted"
      (define content "#lang racket\n(define/contract (foo x)\n  (-> any/c string?)\n  (format \"~a\" x))\n")
      (check-equal? (count-anyc-in-file content) 1))

    (test-case "any/c in ->* contract counted"
      (define content "#lang racket\n(provide (contract-out\n  [foo (->* (any/c) (#:bar any/c) any/c)]))\n")
      (check-equal? (count-anyc-in-file content) 3))

    ;; ── anyc-in-contract-form? helper ──

    (test-case "anyc-in-contract-form? returns 0 for non-contract form"
      (check-equal? (anyc-in-contract-form? "(define x 42)") 0))

    (test-case "anyc-in-contract-form? returns count for contract form"
      (check-equal? (anyc-in-contract-form? "(-> any/c string?)") 1))

    (test-case "anyc-in-contract-form? handles nested forms"
      (check-equal? (anyc-in-contract-form? "(->* (any/c) (#:opt any/c) string?)") 2))

    ;; ── Edge cases ──

    (test-case "malformed sexpr returns 0 (graceful)"
      (check-equal? (count-anyc-in-file "((broken") 0))

    (test-case "file with both contract-out and comments only counts contract"
      (define content "#lang racket
;; any/c in comment
;; Another any/c reference
(provide (contract-out
  [foo (-> any/c string?)]  ;; one any/c in contract
  [bar (-> string? any/c)]))  ;; another any/c in contract
;; any/c after contract
")
      (check-equal? (count-anyc-in-file content) 2))

    (test-case "multi-line contract forms counted correctly"
      (define content "#lang racket
(provide
 (contract-out
  [make-thing (->* (string?
                    exact-nonnegative-integer?)
                   (#:opt any/c
                    #:other any/c)
                   any/c)]))
")
      (check-equal? (count-anyc-in-file content) 3))))

(run-tests contract-metrics-suite)
