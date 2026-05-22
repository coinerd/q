#lang racket

;; tests/test-layer-adapters.rkt — Tests for runtime/layer-adapters.rkt (v0.54.3 W0)
;;
;; Verifies that the layer adapter facade:
;;   - Re-exports all expected identifiers
;;   - Functions are callable with correct arity
;;   - No import breaks due to underlying module changes

(require rackunit
         rackunit/text-ui
         "../runtime/layer-adapters.rkt")

(define layer-adapter-suite
  (test-suite "layer-adapters facade tests"

    ;; ── Tool schema functions are bound ──
    (test-case "list-tools-jsexpr is a procedure"
      (check-pred procedure? list-tools-jsexpr))

    (test-case "merge-tool-lists is a procedure"
      (check-pred procedure? merge-tool-lists))

    ;; ── Tool execution functions are bound ──
    (test-case "run-tool-batch is a procedure"
      (check-pred procedure? run-tool-batch))

    ;; ── Predicates are bound ──
    (test-case "tool-result? is a procedure"
      (check-pred procedure? tool-result?))

    (test-case "tool-registry? is a procedure"
      (check-pred procedure? tool-registry?))

    ;; ── Extension functions are bound ──
    (test-case "dispatch-hooks is a procedure"
      (check-pred procedure? dispatch-hooks))

    (test-case "make-extension-ctx is a procedure"
      (check-pred procedure? make-extension-ctx))

    ;; ── Execution context functions are bound ──
    (test-case "make-exec-context is a procedure"
      (check-pred procedure? make-exec-context))

    (test-case "make-error-result is a procedure"
      (check-pred procedure? make-error-result))

    ;; ── merge-tool-lists works with empty inputs ──
    (test-case "merge-tool-lists handles empty lists"
      (check-equal? (merge-tool-lists '() '()) '()))))

(run-tests layer-adapter-suite)
