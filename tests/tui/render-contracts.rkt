#lang racket

;; tests/tui/render-contracts.rkt — Tier 1 + Tier 2 UX contract tests
;;
;; Tests that enforce styled-line invariants and catch visual corruption bugs.
;; Created for BUG-NEWLINE-BLEED fix (Wave 7 of v0.11.1).
;;
;; W7.1: TDD Red tests (initially failing until bug is fixed)
;; W7.3: Tier 1 UX contract tests (styled-line integrity)

(require rackunit
         rackunit/text-ui
         "../../../q/tui/render.rkt"
         "../../../q/tui/state.rkt"
         "../../../q/tui/input.rkt"
         "../../../q/tui/char-width.rkt")

;; ── Helpers ──────────────────────────────────────────────────────

;; Check that no styled-line in a list contains \n in any segment text
(define (styled-lines-contain-no-newlines? lines)
  (for*/and ([line (in-list lines)]
             [seg (in-list (styled-line-segments line))])
    (not (string-contains? (styled-segment-text seg) "\n"))))

;; Extract all text from a list of styled-lines
(define (styled-lines->text lines)
  (string-join (map styled-line->text lines) "\n"))

;; ══════════════════════════════════════════════════════════════════
;; W7.1: TDD Red Phase — Failing tests for newline bleed
;; ══════════════════════════════════════════════════════════════════

(define w71-red-tests
  (test-suite "W7.1: TDD Red — Newline bleed failing tests"

    ;; ── Test 1: format-entry tool-end sanitizes multi-line text ──
    (test-case "format-entry tool-end: multi-line result text has no newlines"
      ;; Simulates what happens when a tool returns multi-line content:
      ;; The state handler creates text like "[OK: read] (1| foo\n2| bar\n3| baz..."
      (define entry
        (make-entry 'tool-end
                    "[OK: read] (1| #!/usr/bin/env python3\n2| \"\"\"\n3| Training PDF"
                    1000
                    (hasheq 'tool-name 'read)))
      (define lines (format-entry entry 80))
      ;; After fix: no styled-line should contain \n in segment text
      (check-true (styled-lines-contain-no-newlines? lines)
                  "tool-end styled-lines must not contain literal newlines"))

    ;; ── Test 2: format-entry tool-fail sanitizes multi-line errors ──
    (test-case "format-entry tool-fail: multi-line error text has no newlines"
      (define entry
        (make-entry 'tool-fail
                    "[FAIL: bash] Error: command failed\nstderr output\nexit code 1"
                    1000
                    (hasheq 'tool-name 'bash)))
      (define lines (format-entry entry 80))
      (check-true (styled-lines-contain-no-newlines? lines)
                  "tool-fail styled-lines must not contain literal newlines"))

    ;; ── Test 3: tool-end with single newline gets sanitized ──
    (test-case "format-entry tool-end: single newline is replaced"
      (define entry
        (make-entry 'tool-end "[OK: read] first line\nsecond line" 1000 (hasheq 'tool-name 'read)))
      (define lines (format-entry entry 80))
      (check-true (styled-lines-contain-no-newlines? lines)
                  "single newline in tool-end must be sanitized"))

    ;; ── Test 4: styled-line->text produces no newlines for tool results ──
    (test-case "styled-line->text: tool result text is single-line"
      (define entry
        (make-entry 'tool-end
                    "[OK: read] (1| hello\n2| world\n3| end)"
                    1000
                    (hasheq 'tool-name 'read)))
      (define lines (format-entry entry 80))
      (for ([line (in-list lines)])
        (define text (styled-line->text line))
        (check-false (string-contains? text "\n")
                     (format "styled-line->text must not contain \\n: ~v" text))))))

;; ══════════════════════════════════════════════════════════════════
;; W7.3: Tier 1 UX Contract Tests — styled-line integrity
;; ══════════════════════════════════════════════════════════════════

(define w73-contract-tests
  (test-suite "W7.3: Tier 1 — Styled-line contract tests"

    ;; ── Contract 1: render-status-bar produces exactly 1 styled-line ──
    (test-case "render-status-bar: produces exactly 1 styled-line"
      (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
      (define line (render-status-bar state 80))
      (check-pred styled-line? line "render-status-bar returns a styled-line"))

    ;; ── Contract 2: render-input-line produces exactly 1 styled-line ──
    (test-case "render-input-line: produces exactly 1 styled-line"
      (define ist (initial-input-state))
      (define line (render-input-line ist 80))
      (check-pred styled-line? line "render-input-line returns a styled-line"))

    ;; ── Contract 3: long assistant text fits terminal width ──
    (test-case "format-entry assistant: long text lines fit terminal width"
      (define long-text (make-string 500 #\X))
      (define entry (make-entry 'assistant long-text 1000 (hash)))
      (define lines (format-entry entry 80))
      ;; After word-wrapping, each line should fit within width
      (for ([line (in-list lines)]
            [i (in-naturals)])
        (define w (string-visible-width (styled-line->text line)))
        (check-true (<= w 80) (format "line ~a width ~a exceeds 80" i w))))

    ;; ── Contract 4: styled-line->ansi output has no raw newlines ──
    (test-case "styled-line->ansi: no raw newlines in ANSI output"
      (define line (styled-line (list (styled-segment "Hello, world!" '(bold green)))))
      (define ansi (styled-line->ansi line))
      (check-false (string-contains? ansi "\n") "styled-line->ansi output must not contain \\n"))

    ;; ── Contract 5: all entry types produce valid styled-lines ──
    (test-case "format-entry: all entry types produce non-empty styled-lines"
      (define entries
        (list (make-entry 'text "User message" 1000 (hash))
              (make-entry 'tool-start "[TOOL: read] file.txt" 1000 (hasheq 'tool-name 'read))
              (make-entry 'tool-end "[OK: read] content" 1000 (hasheq 'tool-name 'read))
              (make-entry 'tool-fail "[FAIL: bash] error" 1000 (hasheq 'tool-name 'bash))
              (make-entry 'system "Session started" 1000 (hash))
              (make-entry 'error "Something went wrong" 1000 (hash))))
      (for ([entry (in-list entries)]
            [i (in-naturals)])
        (define lines (format-entry entry 80))
        ;; Some entries may produce empty lists (e.g., whitespace-only assistant)
        ;; But if they produce lines, each must have at least one segment
        (for ([line (in-list lines)])
          (check-true
           (not (null? (styled-line-segments line)))
           (format "entry type ~a line ~a has segments" (transcript-entry-kind entry) i)))))))

;; ══════════════════════════════════════════════════════════════════
;; Run all test suites
;; ══════════════════════════════════════════════════════════════════

(run-tests w71-red-tests)
(run-tests w73-contract-tests)
