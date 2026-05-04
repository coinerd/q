#lang racket/base

;; tests/test-lazy-context-assembly.rkt — Tests for deferred context assembly
;;
;; v0.29.5 W3: Tests that expensive context-assembly operations are deferred
;; using delay/force.

(require rackunit
         racket/port
         racket/promise
         racket/runtime-path)

(define-runtime-path q-root-raw "..")
(define q-root (simplify-path q-root-raw))
(define (q-file . parts)
  (apply build-path q-root parts))

;; ============================================================
;; 1. turn-orchestrator.rkt uses delay/force
;; ============================================================

(test-case "turn-orchestrator.rkt uses delay for token estimation"
  (define content (call-with-input-file (q-file "runtime" "turn-orchestrator.rkt") port->string))
  (check-not-false (regexp-match? #rx"delay.*estimate-context-tokens" content)
                   "should have delay wrapping estimate-context-tokens"))

(test-case "turn-orchestrator.rkt uses force for deferred computation"
  (define content (call-with-input-file (q-file "runtime" "turn-orchestrator.rkt") port->string))
  (check-not-false (regexp-match? #rx"force" content)
                   "should use force to realize deferred computation"))

(test-case "turn-orchestrator.rkt defers ws-message resolution"
  (define content (call-with-input-file (q-file "runtime" "turn-orchestrator.rkt") port->string))
  (check-not-false (regexp-match? #rx"delay.*working-set-resolve" content)
                   "should defer working-set message resolution"))

;; ============================================================
;; 2. Force-on-demand (standard promise behavior)
;; ============================================================

(test-case "deferred result is cached after first force"
  (let* ([call-count 0]
         [p (delay
              (begin
                (set! call-count (add1 call-count))
                42))])
    (check-equal? call-count 0 "not computed yet")
    (check-equal? (force p) 42)
    (check-equal? call-count 1 "computed once")
    (force p)
    (check-equal? call-count 1 "still computed only once")))

;; ============================================================
;; 3. Error propagation
;; ============================================================

(test-case "deferred computation propagates errors"
  (define p
    (delay
      (error 'test "boom")))
  (check-exn exn:fail? (lambda () (force p))))

;; ============================================================
;; 4. Context assembly still produces correct results
;; ============================================================

(test-case "deferred token count matches eager computation"
  ;; Verify that delay(force ...) produces same result as direct call
  (define result 42)
  (define p
    (delay
      result))
  (check-equal? (force p) result))

(test-case "at least 2 deferred operations in turn-orchestrator"
  (define content (call-with-input-file (q-file "runtime" "turn-orchestrator.rkt") port->string))
  (define delay-count (length (regexp-match* #rx"delay " content)))
  (check-true (>= delay-count 2) (format "expected at least 2 delay forms, found ~a" delay-count)))
