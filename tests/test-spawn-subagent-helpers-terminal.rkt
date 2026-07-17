#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-spawn-subagent-helpers-terminal.rkt
;; W1 (TMUX-04): Pure unit tests for typed terminal outcome helpers.

(require rackunit
         "../tools/builtins/spawn-subagent-helpers.rkt")

;; ============================================================
;; classify-terminal-status
;; ============================================================

(test-case "complete + content → completed"
  (check-equal? (classify-terminal-status 'complete #t) 'completed))

(test-case "complete + no content → approved-empty"
  (check-equal? (classify-terminal-status 'complete #f) 'approved-empty))

(test-case "max-turns-reached → timed-out"
  (check-equal? (classify-terminal-status 'max-turns-reached #t) 'timed-out)
  (check-equal? (classify-terminal-status 'max-turns-reached #f) 'timed-out))

(test-case "cancelled → cancelled"
  (check-equal? (classify-terminal-status 'cancelled #t) 'cancelled))

(test-case "unknown status → failed"
  (check-equal? (classify-terminal-status 'something-weird #t) 'failed)
  (check-equal? (classify-terminal-status 'something-weird #f) 'failed))

(test-case "only completed and approved-empty are successful child outcomes"
  (check-true (terminal-status-success? 'completed))
  (check-true (terminal-status-success? 'approved-empty))
  (for ([status (in-list '(failed timed-out cancelled denied))])
    (check-false (terminal-status-success? status))))

;; ============================================================
;; result-has-content?
;; ============================================================

(test-case "result-has-content? true for non-empty"
  (check-true (result-has-content? "hello"))
  (check-true (result-has-content? "  x  ")))

(test-case "result-has-content? false for empty/whitespace"
  (check-false (result-has-content? ""))
  (check-false (result-has-content? "   "))
  (check-false (result-has-content? "\t\n")))

(test-case "result-has-content? false for non-string"
  (check-false (result-has-content? #f))
  (check-false (result-has-content? '())))

;; ============================================================
;; make-safe-result-metadata
;; ============================================================

(test-case "make-safe-result-metadata produces all fields"
  (define meta (make-safe-result-metadata "hello world" "sess-123" 'completed))
  (check-equal? (hash-ref meta 'result-present?) #t)
  (check-equal? (hash-ref meta 'content-size) 11)
  (check-true (string? (hash-ref meta 'content-digest)))
  (check-equal? (hash-ref meta 'session-id) "sess-123")
  (check-equal? (hash-ref meta 'terminal-status) "completed"))

(test-case "make-safe-result-metadata handles empty text"
  (define meta (make-safe-result-metadata "" "sess-456" 'approved-empty))
  (check-equal? (hash-ref meta 'result-present?) #f)
  (check-equal? (hash-ref meta 'content-size) 0)
  (check-equal? (hash-ref meta 'terminal-status) "approved-empty"))

(test-case "make-safe-result-metadata handles #f text"
  (define meta (make-safe-result-metadata #f #f 'failed))
  (check-equal? (hash-ref meta 'result-present?) #f)
  (check-equal? (hash-ref meta 'content-size) 0)
  (check-equal? (hash-ref meta 'session-id) ""))

(test-case "make-safe-result-metadata digest is deterministic"
  (define meta1 (make-safe-result-metadata "same text" "s1" 'completed))
  (define meta2 (make-safe-result-metadata "same text" "s2" 'completed))
  (check-equal? (hash-ref meta1 'content-digest) (hash-ref meta2 'content-digest)))

(test-case "make-safe-result-metadata digest differs for different text"
  (define meta1 (make-safe-result-metadata "text A" "s1" 'completed))
  (define meta2 (make-safe-result-metadata "text B" "s1" 'completed))
  (check-false (equal? (hash-ref meta1 'content-digest) (hash-ref meta2 'content-digest))))
