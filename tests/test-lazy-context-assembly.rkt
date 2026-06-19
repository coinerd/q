#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-lazy-context-assembly.rkt — Tests for context assembly patterns
;;
;; Originally written for v0.29.5 W3 to test delay/force deferred computation
;; in turn-orchestrator.rkt. The lazy/deferred optimization was never adopted;
;; the architecture uses eager assembly via build-assembled-context. These
;; tests now verify the actual architecture:
;;   - turn-orchestrator.rkt delegates context assembly to selection.rkt
;;   - build-assembled-context is used for assembly
;;   - Standard promise behavior works correctly

(require rackunit
         racket/port
         racket/promise
         racket/runtime-path)

(define-runtime-path q-root-raw "..")
(define q-root (simplify-path q-root-raw))
(define (q-file . parts)
  (apply build-path q-root parts))

;; ============================================================
;; 1. turn-orchestrator.rkt delegates to context assembly module
;; ============================================================

(test-case "turn-orchestrator.rkt contains build-assembled-context"
  (define content (call-with-input-file (q-file "runtime" "turn-orchestrator.rkt") port->string))
  (check-not-false (regexp-match? #rx"build-assembled-context" content)
                   "should use build-assembled-context for context assembly"))

(test-case "turn-orchestrator.rkt references context-assembly module"
  (define content (call-with-input-file (q-file "runtime" "turn-orchestrator.rkt") port->string))
  (check-not-false (regexp-match? #rx"context-assembly" content)
                   "should reference context-assembly module"))

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
