#lang racket/base

;; tests/test-gapd-memory-injection-default.rkt
;; v0.97.4 W1: GAP-D default memory injection budget

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget))

(define (resolve-injection-budget settings-budget profile max-ctx-tokens)
  ;; Simulates the GAP-D logic from wiring/run-modes.rkt
  (cond
    [settings-budget settings-budget]
    [(memq profile '(self-healing full)) (quotient max-ctx-tokens 20)]
    [else #f]))

(define suite
  (test-suite "gapd-memory-injection-default"

    ;; Explicit config wins over default
    (test-case "explicit settings-budget overrides default"
      (check-equal? (resolve-injection-budget 2000 'self-healing 128000) 2000)
      (check-equal? (resolve-injection-budget 2000 'off 128000) 2000)
      (check-equal? (resolve-injection-budget 2000 'full 128000) 2000))

    ;; self-healing profile without config → 5% of context
    (test-case "self-healing: default is 5% of max-context-tokens"
      (check-equal? (resolve-injection-budget #f 'self-healing 128000) 6400)
      (check-equal? (resolve-injection-budget #f 'self-healing 32768) 1638)
      (check-equal? (resolve-injection-budget #f 'self-healing 8192) 409))

    ;; full profile without config → 5% of context
    (test-case "full: default is 5% of max-context-tokens"
      (check-equal? (resolve-injection-budget #f 'full 128000) 6400)
      (check-equal? (resolve-injection-budget #f 'full 200000) 10000))

    ;; off/observe/bounded profiles keep #f
    (test-case "off/observe/bounded: no default injection budget"
      (check-equal? (resolve-injection-budget #f 'off 128000) #f)
      (check-equal? (resolve-injection-budget #f 'observe 128000) #f)
      (check-equal? (resolve-injection-budget #f 'bounded 128000) #f))

    ;; Zero context window edge case
    (test-case "zero context window returns 0 for self-healing"
      (check-equal? (resolve-injection-budget #f 'self-healing 0) 0))

    ;; F8: current-memory-injection-budget parameter smoke test
    (test-case "current-memory-injection-budget defaults to #f"
      (check-equal? (current-memory-injection-budget) #f))

    (test-case "current-memory-injection-budget can be set and read"
      (parameterize ([current-memory-injection-budget 6400])
        (check-equal? (current-memory-injection-budget) 6400)))

    (test-case "current-memory-injection-budget resets after parameterize"
      (parameterize ([current-memory-injection-budget 9999])
        (check-equal? (current-memory-injection-budget) 9999))
      (check-equal? (current-memory-injection-budget) #f))))

(run-tests suite)
