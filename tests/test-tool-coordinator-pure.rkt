#lang racket

;; BOUNDARY: pure

;; tests/test-tool-coordinator-pure.rkt -- W2: Pure helper tests for tool-coordinator.rkt
;;
;; Tests the extracted pure helpers:
;;   - classify-tool-results
;;   - build-blocked-tool-results

(require rackunit
         rackunit/text-ui
         "../runtime/tool-coordinator.rkt"
         "../tools/scheduler.rkt"
         "../agent/event-bus.rkt"
         "../agent/event-emitter.rkt"
         "../util/message/protocol-types.rkt")

;; Helper to extract text string from a tool-result
(define (result-text tr)
  (hash-ref (first (tool-result-content tr)) 'text))

(define pure-suite
  (test-suite "tool-coordinator pure helpers"

    ;; -- classify-tool-results: all completed --
    (test-case "classify-tool-results marks all as completed when no errors"
      (define tc1 (make-tool-call "id-1" "grep" (hasheq 'pattern "foo")))
      (define tc2 (make-tool-call "id-2" "ls" (hasheq)))
      (define tr1 (make-tool-result "found" (hasheq) #f))
      (define tr2 (make-tool-result "dir" (hasheq) #f))
      (define result (classify-tool-results (list tc1 tc2) (list tr1 tr2)))
      (check-equal? (length result) 2)
      (check-equal? (hash-ref (first result) 'status) 'completed)
      (check-equal? (hash-ref (second result) 'status) 'completed)
      (check-equal? (hash-ref (first result) 'name) "grep")
      (check-equal? (hash-ref (second result) 'name) "ls"))

    ;; -- classify-tool-results: mixed errors --
    (test-case "classify-tool-results marks errors correctly"
      (define tc1 (make-tool-call "id-1" "grep" (hasheq)))
      (define tc2 (make-tool-call "id-2" "read" (hasheq)))
      (define tr1 (make-tool-result "error" (hasheq) #t))
      (define tr2 (make-tool-result "ok" (hasheq) #f))
      (define result (classify-tool-results (list tc1 tc2) (list tr1 tr2)))
      (check-equal? (hash-ref (first result) 'status) 'error)
      (check-equal? (hash-ref (second result) 'status) 'completed))

    ;; -- classify-tool-results: empty lists --
    (test-case "classify-tool-results with empty lists returns empty"
      (check-equal? (classify-tool-results '() '()) '()))

    ;; -- build-blocked-tool-results: creates errors for all --
    (test-case "build-blocked-tool-results creates error results"
      (define tc1 (make-tool-call "id-1" "grep" (hasheq)))
      (define tc2 (make-tool-call "id-2" "ls" (hasheq)))
      (define result (build-blocked-tool-results (list tc1 tc2)))
      (check-equal? (length result) 2)
      (check-true (tool-result-is-error? (first result)))
      (check-true (tool-result-is-error? (second result)))
      (check-true (string-contains? (result-text (first result)) "blocked")))

    ;; -- build-blocked-tool-results: empty list --
    (test-case "build-blocked-tool-results with empty list returns empty"
      (check-equal? (build-blocked-tool-results '()) '()))

    ;; -- build-blocked-tool-results: single entry --
    (test-case "build-blocked-tool-results single entry"
      (define tc (make-tool-call "id-1" "read" (hasheq)))
      (define result (build-blocked-tool-results (list tc)))
      (check-equal? (length result) 1)
      (check-true (tool-result-is-error? (first result)))
      (check-true (string-contains? (result-text (first result)) "read")))

    ;; ── v0.45.17 regression: emit-session-event! with non-hash payload ──
    ;; Superseded by v0.45.19 fix (scheduler-batch-stats->hash converts at source).
    ;; Kept as historical regression guard: verifies wrapping fallback works.
    (test-case "v0.45.17 regression: non-hash payload wrapped before emit"
      (define bus (make-event-bus))
      (define stats (scheduler-batch-stats 3 3 0 0))
      (check-false (hash? stats))
      (define wrapped
        (if (hash? stats)
            stats
            (hash 'raw stats)))
      (check-true (hash? wrapped))
      (check-equal? (hash-ref wrapped 'raw) stats)
      (check-not-exn (lambda ()
                       (emit-session-event! bus "test-session" "tool.batch.completed" wrapped))))

    ;; ── v0.45.19: scheduler-batch-stats→hash conversion ──
    ;; Replaces opaque (hash 'raw ...) wrapping with proper field-level conversion.
    ;; Downstream consumers (TUI, RPC, SDK) can now read batch stats fields.
    (test-case "v0.45.19: scheduler-batch-stats->hash produces correct hash"
      (define stats (scheduler-batch-stats 5 3 1 1))
      (define h (scheduler-batch-stats->hash stats))
      (check-true (hash? h))
      (check-equal? (hash-ref h 'total) 5)
      (check-equal? (hash-ref h 'executed) 3)
      (check-equal? (hash-ref h 'blocked) 1)
      (check-equal? (hash-ref h 'errors) 1)
      (define bus (make-event-bus))
      (check-not-exn (lambda () (emit-session-event! bus "test-session" "tool.batch.completed" h))))))

(run-tests pure-suite 'verbose)
