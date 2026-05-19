#lang racket

;; BOUNDARY: integration

;; tests/test-iteration-tool-bridge.rkt — tests for iteration/tool-turn-bridge

(require rackunit
         rackunit/text-ui
         racket/set
         "../runtime/iteration/tool-turn-bridge.rkt"
         "../runtime/working-set.rkt"
         "../util/protocol-types.rkt")

(define bridge-tests
  (test-suite "Iteration Tool-Turn Bridge"

    (test-case "take-at-most: short list unchanged"
      (check-equal? (take-at-most '(1 2 3) 5) '(1 2 3)))

    (test-case "take-at-most: truncates long list"
      (check-equal? (take-at-most '(1 2 3 4 5) 3) '(1 2 3)))

    (test-case "take-at-most: exact length"
      (check-equal? (take-at-most '(1 2 3) 3) '(1 2 3)))

    (test-case "compute-tool-counters: explore + implement"
      (define tcs
        (list (make-tool-call #f "read" (hash))
              (make-tool-call #f "edit" (hash))
              (make-tool-call #f "grep" (hash))))
      (define-values (exp impl) (compute-tool-counters tcs 0 0))
      (check-equal? exp 2)
      (check-equal? impl 1))

    (test-case "compute-tool-counters: no matching tools"
      (define tcs (list (make-tool-call #f "bash" (hash))))
      (define-values (exp impl) (compute-tool-counters tcs 0 0))
      (check-equal? exp 0)
      (check-equal? impl 0))

    (test-case "update-seen-paths: non-read tool resets"
      (define tcs (list (make-tool-call #f "edit" (hash 'path "foo.rkt"))))
      (define-values (new-paths inc?) (update-seen-paths tcs '("bar.rkt")))
      (check-equal? new-paths '())
      (check-false inc?))

    (test-case "update-seen-paths: read tool with new path"
      (define tcs (list (make-tool-call #f "read" (hash 'path "new.rkt"))))
      (define-values (new-paths inc?) (update-seen-paths tcs '()))
      (check-not-false (member "new.rkt" new-paths))
      (check-true inc?))

    (test-case "detect-read-spiral: no spiral on empty ws"
      (define tcs (list (make-tool-call #f "read" (hash 'path "foo.rkt"))))
      (define ws (make-working-set))
      (define spirals (detect-read-spiral tcs ws))
      (check-equal? spirals '()))))

(module+ main
  (run-tests bridge-tests))
(module+ test
  (run-tests bridge-tests))
