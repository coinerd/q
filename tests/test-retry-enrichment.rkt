#lang racket/base

;; test-retry-enrichment.rkt — Tests for v0.14.2 Wave 2: enriched /retry
;;
;; Tests that get-last-turn-tool-summary extracts tool results from the
;; last turn in the transcript for inclusion in retry prompts.

(require rackunit
         racket/string
         "../tui/state.rkt")

;; Helper: build a ui-state with given transcript entries
(define (state-with-entries . entries)
  (for/fold ([st (initial-ui-state)]) ([e (in-list entries)])
    (add-transcript-entry st e)))

;; ============================================================
;; get-last-turn-tool-summary tests
;; ============================================================

(test-case "no entries → #f"
  (check-false (get-last-turn-tool-summary (initial-ui-state))))

(test-case "user message only, no tools → #f"
  (define st (state-with-entries (make-entry 'user "hello" 1000 (hash))))
  (check-false (get-last-turn-tool-summary st)))

(test-case "user + one tool-end → summary with tool name and result"
  (define st
    (state-with-entries (make-entry 'user "read the file" 1000 (hash))
                        (make-entry 'tool-end
                                    "[OK: read_file] 185 lines"
                                    1001
                                    (hasheq 'name "read_file" 'result "185 lines of Python code"))))
  (define summary (get-last-turn-tool-summary st))
  (check-true (string? summary) (format "expected string, got: ~a" summary))
  (check-true (string-contains? summary "read_file")
              (format "summary should contain tool name: ~a" summary))
  (check-true (string-contains? summary "185 lines")
              (format "summary should contain result: ~a" summary)))

(test-case "user + multiple tools → summary joined with semicolons"
  (define st
    (state-with-entries (make-entry 'user "analyze" 1000 (hash))
                        (make-entry 'tool-end
                                    "[OK: read_file] 185 lines"
                                    1001
                                    (hasheq 'name "read_file" 'result "185 lines"))
                        (make-entry 'tool-end
                                    "[OK: list_fonts] DejaVu"
                                    1002
                                    (hasheq 'name "list_fonts" 'result "DejaVu family"))))
  (define summary (get-last-turn-tool-summary st))
  (check-true (string-contains? summary "read_file"))
  (check-true (string-contains? summary "list_fonts"))
  (check-true (string-contains? summary ";")
              (format "multiple tools should be joined with ';': ~a" summary)))

(test-case "assistant text between user and tools → still picks up tools"
  (define st
    (state-with-entries
     (make-entry 'user "do stuff" 1000 (hash))
     (make-entry 'assistant "I will help" 1001 (hash))
     (make-entry 'tool-end "[OK: bash] done" 1002 (hasheq 'name "bash" 'result "success"))))
  (define summary (get-last-turn-tool-summary st))
  (check-true (string? summary))
  (check-true (string-contains? summary "bash")))

(test-case "tool-fail entries are excluded from summary"
  (define st
    (state-with-entries (make-entry 'user "do stuff" 1000 (hash))
                        (make-entry 'tool-fail
                                    "[FAIL: bash] error"
                                    1001
                                    (hasheq 'name "bash" 'result "permission denied"))))
  (check-false (get-last-turn-tool-summary st)))

(test-case "long result is truncated to 100 chars"
  (define long-result (make-string 200 #\x))
  (define st
    (state-with-entries
     (make-entry 'user "analyze" 1000 (hash))
     (make-entry 'tool-end "[OK: tool] ..." 1001 (hasheq 'name "tool" 'result long-result))))
  (define summary (get-last-turn-tool-summary st))
  (check-true (string? summary))
  ;; The result part should be truncated (100 chars + "…")
  (check-true (< (string-length summary) 150)
              (format "summary should be truncated: len=~a" (string-length summary))))

(test-case "two turns — only picks up tools from last turn"
  (define st
    (state-with-entries
     (make-entry 'user "first prompt" 1000 (hash))
     (make-entry 'tool-end "[OK: old_tool] old result" 1001 (hasheq 'name "old_tool" 'result "old"))
     (make-entry 'user "second prompt" 2000 (hash))
     (make-entry 'tool-end "[OK: new_tool] new result" 2001 (hasheq 'name "new_tool" 'result "new"))))
  (define summary (get-last-turn-tool-summary st))
  (check-true (string-contains? summary "new_tool"))
  (check-false (string-contains? summary "old_tool")
               (format "should not contain old tool: ~a" summary)))
