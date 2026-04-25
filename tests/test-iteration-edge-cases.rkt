#lang racket

;; tests/test-iteration-edge-cases.rkt — T08: Edge-case tests for iteration module
;;
;; Tests boundary conditions:
;; - ensure-hash-args edge cases
;; - check-mid-turn-budget! boundary behavior

(require rackunit
         racket/hash
         "../runtime/iteration.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; ensure-hash-args edge cases
;; ============================================================

(test-case "ensure-hash-args passes through hash unchanged"
  (define h (hasheq 'a 1 'b 2))
  (check-equal? (ensure-hash-args h) h))

(test-case "ensure-hash-args parses JSON string"
  (define result (ensure-hash-args "{\"key\": \"value\"}"))
  (check-equal? (hash-ref result 'key) "value"))

(test-case "ensure-hash-args handles empty JSON object string"
  (define result (ensure-hash-args "{}"))
  (check-true (hash? result))
  (check-equal? (hash-count result) 0))

(test-case "ensure-hash-args handles empty string"
  (define result (ensure-hash-args ""))
  (check-true (hash? result))
  (check-equal? (hash-count result) 0))

(test-case "ensure-hash-args handles malformed JSON gracefully"
  (define result (ensure-hash-args "not json"))
  (check-true (hash-has-key? result '_parse_failed)))

;; ============================================================
;; check-mid-turn-budget! edge cases
;; ============================================================

(test-case "check-mid-turn-budget! passes when well under limit"
  ;; check-mid-turn-budget! takes (ctx bus session-id config)
  ;; ctx = message list, bus = event bus, config with max-context-tokens
  (define bus (make-event-bus))
  (define config (hasheq 'max-context-tokens 1000))
  (check-not-exn
   (lambda ()
     (check-mid-turn-budget! '() bus "test-session" config))))

(test-case "check-mid-turn-budget! uses default when no config key"
  (define bus (make-event-bus))
  (define config (hasheq))
  ;; Empty context, default 128K tokens — should pass easily
  (check-not-exn
   (lambda ()
     (check-mid-turn-budget! '() bus "test-session" config))))
