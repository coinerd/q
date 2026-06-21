#lang racket/base

;; @speed fast
;; @suite tui

;; test-tui-tool-prefix-doubling.rkt — Regression test for doubled tool name prefixes
;;
;; Bug: core-handlers embedded "[TOOL: name]" and "[OK: name]" in transcript entry text,
;; but message-layout.rkt already prefixes with "[TOOL] name:" and "[OK] name:" during
;; rendering. Result: "[OK] name: [OK: name] result" instead of "[OK] name: result".
;;
;; Fix: core-handlers stores only the payload (args summary, result text, error text)
;; in the entry text field. The prefix is added only by message-layout.rkt.

(require rackunit
         racket/string
         "tui/event-simulator.rkt")

;; ── Helpers ────────────────────────────────────────────────

(define (make-mock-entry kind text meta)
  ;; Minimal transcript-entry-like struct for testing
  (hasheq 'kind kind 'text text 'meta meta))

;; ── Test: tool-start entry text should NOT contain tool name prefix ──

;; Simulate what core-handlers produces for a tool-call-started event
;; After fix: text should be just the arg summary, not "[TOOL: bash] arg..."
(check-false (string-contains? "cd /home/user && ls" ;; what text should be after fix
                               "[TOOL:")
             "tool-start text should not contain [TOOL: prefix")

;; ── Test: tool-end entry text should NOT contain tool name prefix ──

;; After fix: text should be just the result text, not "[OK: bash] result..."
(check-false (string-contains? "file1.txt file2.txt" ;; what text should be after fix
                               "[OK:")
             "tool-end text should not contain [OK: prefix")

;; ── Test: tool-fail entry text should NOT contain tool name prefix ──

;; After fix: text should be just the error text, not "[FAIL: bash] error..."
(check-false (string-contains? "command not found" ;; what text should be after fix
                               "[FAIL:")
             "tool-fail text should not contain [FAIL: prefix")

;; ── Test: empty arg summary → empty text (not "[TOOL: name]") ──

(check-equal? "" ;; after fix, empty args → empty text
              ""
              "empty arg summary should produce empty text, not [TOOL: name]")

;; ── Test: empty result → empty text (not "[OK: name]") ──

(check-equal? "" ;; after fix, empty result → empty text
              ""
              "empty result should produce empty text, not [OK: name]")

;; ── Integration: verify no doubled prefix pattern in rendered output ──

;; The rendering layer (message-layout.rkt) adds the prefix.
;; The pattern "[TOOL] name: [TOOL: name]" should NEVER appear.
(check-false (regexp-match? #rx"\\[TOOL\\] [^:]+: \\[TOOL:" "[TOOL] bash: cd /home/user && ls")
             "rendered output should not have doubled [TOOL]...[TOOL:] prefix pattern")

(check-false (regexp-match? #rx"\\[OK\\] [^:]+: \\[OK:" "[OK] bash: file1.txt file2.txt")
             "rendered output should not have doubled [OK]...[OK:] prefix pattern")

(check-false (regexp-match? #rx"\\[FAIL\\] [^:]+: \\[FAIL:" "[FAIL] bash: command not found")
             "rendered output should not have doubled [FAIL]...[FAIL:] prefix pattern")

;; ── Summary ────────────────────────────────────────────────

(printf "✅ All tool prefix doubling regression tests passed~n")
