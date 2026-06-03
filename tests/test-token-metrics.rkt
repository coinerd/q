#lang racket/base

;; tests/test-token-metrics.rkt — Token metrics validation tests
;; v0.76.2 W2: Verify measurement functions are accurate.

(require rackunit
         rackunit/text-ui
         racket/list
         (only-in "../runtime/context-assembly/token-metrics.rkt"
                  context-metrics?
                  context-metrics-task-state
                  context-metrics-before-tokens
                  context-metrics-after-tokens
                  context-metrics-savings-tokens
                  context-metrics-savings-pct
                  context-metrics-category-breakdown
                  context-metrics-timestamp
                  measure-context-size
                  compute-savings
                  category-breakdown
                  compute-conclusion-coverage
                  measure-context-assembly)
         (only-in "../util/message/protocol-types.rkt" make-message make-text-part message-kind message-role)
         (only-in "../runtime/context-assembly/context-floor.rkt"
                  tiered-context?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c))

;; Helper: create a message with given text
(define (msg text)
  (make-message (format "id~a" (current-inexact-milliseconds))
                #f
                'user
                'text
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

;; Helper: create N messages with given text length
(define (msgs n text)
  (for/list ([i (in-range n)])
    (msg text)))

(define suite
  (test-suite "token-metrics"

    ;; ── measure-context-size ──
    (test-case "empty list has 0 tokens"
      (check-equal? (measure-context-size '()) 0))

    (test-case "single message token count is positive"
      (define m (msg "Hello world"))
      (check-true (> (measure-context-size (list m)) 0)))

    (test-case "token count scales with message count"
      (define one (measure-context-size (msgs 1 "Hello world this is a test message")))
      (define five (measure-context-size (msgs 5 "Hello world this is a test message")))
      (check-true (> five one) (format "five=~a should be > one=~a" five one)))

    (test-case "token count scales with text length"
      (define short (measure-context-size (msgs 1 "Hi")))
      (define long (measure-context-size (msgs 1 (make-string 100 #\a))))
      (check-true (> long short) (format "long=~a should be > short=~a" long short)))

    ;; ── compute-savings ──
    (test-case "savings is before minus after"
      (define-values (savings pct) (compute-savings 100 80))
      (check-equal? savings 20)
      (check-= pct 20.0 0.01))

    (test-case "zero savings when before equals after"
      (define-values (savings pct) (compute-savings 100 100))
      (check-equal? savings 0)
      (check-= pct 0.0 0.01))

    (test-case "negative savings when after > before"
      (define-values (savings pct) (compute-savings 80 100))
      (check-equal? savings -20)
      (check-= pct -25.0 0.01))

    (test-case "zero pct when before is 0"
      (define-values (savings pct) (compute-savings 0 0))
      (check-equal? savings 0)
      (check-= pct 0.0 0.01))

    (test-case "50% savings computed correctly"
      (define-values (savings pct) (compute-savings 200 100))
      (check-equal? savings 100)
      (check-= pct 50.0 0.01))

    ;; ── category-breakdown ──
    (test-case "breakdown has all expected keys"
      (define-values (tc metrics)
        (measure-context-assembly (msgs 3 "test") #:task-state 'exploration))
      (define breakdown (context-metrics-category-breakdown metrics))
      (check-true (hash-has-key? breakdown 'tier-a))
      (check-true (hash-has-key? breakdown 'tier-b))
      (check-true (hash-has-key? breakdown 'tier-c))
      (check-true (hash-has-key? breakdown 'total)))

    (test-case "breakdown total equals sum of tiers"
      (define-values (tc metrics)
        (measure-context-assembly (msgs 3 "test") #:task-state 'exploration))
      (define breakdown (context-metrics-category-breakdown metrics))
      (check-equal?
       (hash-ref breakdown 'total)
       (+ (hash-ref breakdown 'tier-a) (hash-ref breakdown 'tier-b) (hash-ref breakdown 'tier-c))))

    ;; ── compute-conclusion-coverage ──
    (test-case "empty tool list gives 0 coverage"
      (check-= (compute-conclusion-coverage '()) 0.0 0.01))

    (test-case "all conclusions gives 1.0 coverage"
      (check-= (compute-conclusion-coverage '("record_conclusion" "record_conclusion")) 1.0 0.01))

    (test-case "half conclusions gives 0.5 coverage"
      (check-= (compute-conclusion-coverage '("read" "record_conclusion")) 0.5 0.01))

    (test-case "no conclusions gives 0.0 coverage"
      (check-= (compute-conclusion-coverage '("read" "edit" "bash")) 0.0 0.01))

    (test-case "save-conclusion counts as conclusion"
      (check-= (compute-conclusion-coverage '("read" "save-conclusion")) 0.5 0.01))

    (test-case "save_conclusion counts as conclusion"
      (check-= (compute-conclusion-coverage '("save_conclusion")) 1.0 0.01))

    (test-case "coverage is 0.4 for 2/5 conclusions"
      (check-=
       (compute-conclusion-coverage '("read" "edit" "record_conclusion" "bash" "record_conclusion"))
       0.4
       0.01))

    ;; ── measure-context-assembly ──
    (test-case "returns tiered-context and metrics"
      (define-values (tc metrics) (measure-context-assembly (msgs 3 "test message content")))
      (check-true (tiered-context? tc))
      (check-true (context-metrics? metrics)))

    (test-case "metrics has correct task-state"
      (define-values (tc metrics)
        (measure-context-assembly (msgs 3 "test") #:task-state 'implementation))
      (check-equal? (context-metrics-task-state metrics) 'implementation))

    (test-case "metrics has non-negative before and after"
      (define-values (tc metrics) (measure-context-assembly (msgs 3 "test")))
      (check-true (>= (context-metrics-before-tokens metrics) 0))
      (check-true (>= (context-metrics-after-tokens metrics) 0)))

    (test-case "metrics timestamp is present"
      (define-values (tc metrics) (measure-context-assembly (msgs 3 "test")))
      (check-true (> (context-metrics-timestamp metrics) 0)))

    (test-case "metrics with no state uses 'none"
      (define-values (tc metrics) (measure-context-assembly (msgs 3 "test")))
      (check-equal? (context-metrics-task-state metrics) 'none))

    (test-case "state-aware assembly with preamble adds preamble overhead"
      (define-values (tc metrics)
        (measure-context-assembly (msgs 10 "test message content") #:task-state 'implementation))
      ;; v0.76.7 W7: Non-tautological — verify preamble exists in tier-a
      ;; for non-idle states (implementation state injects state-awareness preamble)
      (check-true (context-metrics? metrics))
      (define preamble-msg
        (for/first ([m (tiered-context-tier-a tc)]
                    #:when (equal? (message-kind m) 'system-instruction))
          m))
      (check-not-false preamble-msg "preamble message should be present for implementation state"))))

(run-tests suite)
