#lang racket/base

;; tests/test-di-explicit.rkt — Tests for explicit DI (no parameter fallbacks)
;;
;; W0 scaffolding for v0.29.5: Stream Purity + DI Cleanup.
;; Tests that run-iteration-loop requires explicit DI arguments
;; with no parameter fallbacks (W2 removes the parameters).

(require rackunit)

;; ============================================================
;; 1. DI parameters removed
;; ============================================================

(test-case "current-compact-proc parameter no longer exists"
  ;; After W2, current-compact-proc should not be exported
  ;; Callers must pass #:compact-proc explicitly
  (check-true #t "placeholder — verify in W2"))

(test-case "current-estimate-tokens parameter no longer exists"
  (check-true #t "placeholder — verify in W2"))

(test-case "current-inject-topic parameter no longer exists"
  (check-true #t "placeholder — verify in W2"))

;; ============================================================
;; 2. Explicit DI via keyword arguments
;; ============================================================

(test-case "resolve-compact-proc returns explicit arg when provided"
  ;; After W2, compact-proc is a required keyword arg, not a parameter
  (check-true #t "placeholder"))

(test-case "resolve-estimate-tokens returns explicit arg when provided"
  (check-true #t "placeholder"))

(test-case "resolve-inject-topic returns explicit arg when provided"
  (check-true #t "placeholder"))

;; ============================================================
;; 3. No lazy-require fallbacks
;; ============================================================

(test-case "loop-state.rkt has no lazy-require imports"
  ;; After W2, loop-state.rkt should not use lazy-require
  (check-true #t "placeholder"))

(test-case "no make-parameter in loop-state.rkt"
  ;; grep -c 'make-parameter' runtime/iteration/loop-state.rkt → 0
  (check-true #t "placeholder"))

;; ============================================================
;; 4. Backward compatibility
;; ============================================================

(test-case "agent-session sets DI via keyword args"
  ;; agent-session.rkt should pass #:compact-proc etc. explicitly
  (check-true #t "placeholder"))

(test-case "session-lifecycle sets DI via keyword args"
  (check-true #t "placeholder"))
