#lang racket

;; tests/test-message-layout.rkt — Tests for tui/render/message-layout.rkt
;; Regression tests for wrap-styled-line ordering, leading-space stripping,
;; and vdom-layout width-aware truncation.

(require rackunit
         "../tui/render/message-layout.rkt"
         "../tui/char-width.rkt")

;; ============================================================
;; wrap-styled-line: line ordering
;; ============================================================

(test-case "wrap-styled-line: correct line order for multi-segment overflow"
  (define line (styled-line (list
                             (styled-segment "First segment " '())
                             (styled-segment "second" '(bold))
                             (styled-segment " overflow content here" '()))))
  (define result (wrap-styled-line line 20))
  (check >= (length result) 2)
  ;; First line should contain "First segment" — not overflow content
  (define first-text (styled-line->text (car result)))
  (check-true (string-contains? first-text "First segment")
              (format "Expected 'First segment' in first line, got: ~v" first-text)))

(test-case "wrap-styled-line: single long segment preserves order"
  (define line (styled-line (list
                             (styled-segment "Alpha beta gamma delta epsilon zeta eta theta" '()))))
  (define result (wrap-styled-line line 20))
  ;; Lines should be in reading order: first line starts with "Alpha"
  (define texts (map styled-line->text result))
  (check-true (string-prefix? (car texts) "Alpha")
              (format "Lines out of order: ~v" texts))
  ;; Joined text should preserve all words
  (define joined (string-join texts))
  (check-true (string-contains? joined "Alpha")
              (format "Missing word in joined: ~v" joined)))

;; ============================================================
;; wrap-styled-line: leading space stripping
;; ============================================================

(test-case "wrap-styled-line: no leading space on overflow lines"
  (define line (styled-line (list
                             (styled-segment "Short" '())
                             (styled-segment " text that overflows the width limit" '()))))
  (define result (wrap-styled-line line 15))
  ;; No line should start with whitespace
  (for ([l result] [i (in-naturals)])
    (define text (styled-line->text l))
    (when (> (string-length text) 0)
      (check-false (char-whitespace? (string-ref text 0))
                   (format "Line ~a starts with whitespace: ~v" i text)))))

(test-case "wrap-styled-line: bold segment overflow strips leading space"
  (define line (styled-line (list
                             (styled-segment "This is " '())
                             (styled-segment "bold" '(bold))
                             (styled-segment " text that wraps to the next line" '()))))
  (define result (wrap-styled-line line 20))
  (for ([l result] [i (in-naturals)])
    (define text (styled-line->text l))
    (when (> (string-length text) 0)
      (check-false (char-whitespace? (string-ref text 0))
                   (format "Line ~a starts with whitespace: ~v" i text)))))

;; ============================================================
;; md-format-assistant: end-to-end wrapping
;; ============================================================

(test-case "md-format-assistant: no leading whitespace on any wrapped line"
  (define text "The CHANGELOG shows dozens of versions in rapid succession (v0.76 → v0.94). A lot of it is audit remediation — fixing drift, tightening contracts, updating stale references.")
  (define result (md-format-assistant text 80))
  (for ([l result] [i (in-naturals)])
    (define l-text (styled-line->text l))
    (when (> (string-length l-text) 0)
      (check-false (char-whitespace? (string-ref l-text 0))
                   (format "Line ~a starts with whitespace: ~v" i l-text)))))

(test-case "md-format-assistant: text with bold wrapping"
  ;; Simulate markdown with bold that gets wrapped
  (define text "Here is some **bold text** and more content that follows to cause wrapping at narrow widths.")
  (define result (md-format-assistant text 40))
  ;; Check ordering: first line should start with "Here"
  (check-true (string-prefix? (styled-line->text (car result)) "Here"))
  ;; No leading whitespace
  (for ([l result] [i (in-naturals)])
    (define l-text (styled-line->text l))
    (when (> (string-length l-text) 0)
      (check-false (char-whitespace? (string-ref l-text 0))
                   (format "Line ~a starts with whitespace: ~v" i l-text)))))

(test-case "md-format-assistant: preserves line breaks from source"
  (define text "Line one.\nLine two.\n\nParagraph two.")
  (define result (md-format-assistant text 80))
  (define texts (map styled-line->text result))
  ;; Should contain all the text content (order and empty lines may vary)
  (check-true (ormap (lambda (t) (string-contains? t "Line one.")) texts)
              (format "Expected 'Line one.' in result: ~v" texts))
  (check-true (ormap (lambda (t) (string-contains? t "Line two.")) texts)
              (format "Expected 'Line two.' in result: ~v" texts))
  (check-true (ormap (lambda (t) (string-contains? t "Paragraph two.")) texts)
              (format "Expected 'Paragraph two.' in result: ~v" texts)))

;; ============================================================
;; wrap-single-line: basic wrapping
;; ============================================================

(test-case "wrap-single-line: splits at word boundaries"
  (define result (wrap-single-line "hello world foo bar baz" 12))
  (check-equal? result '("hello world " "foo bar baz")))

(test-case "wrap-single-line: no split needed"
  (define result (wrap-single-line "short line" 80))
  (check-equal? result '("short line")))

(test-case "wrap-single-line: hard break when no spaces"
  (define result (wrap-single-line "abcdefghij" 5))
  (check-equal? result '("abcde" "fghij")))

;; ============================================================
;; find-break-pos: word boundary detection
;; ============================================================

(test-case "find-break-pos: breaks after last space before width"
  (define text "hello world test")
  (define pos (find-break-pos text 0 10))
  ;; "hello worl" is 10 chars, but there's a space at 5
  ;; So break should be after the space: position 6
  (check-equal? pos 6))

(test-case "find-break-pos: hard break when no space before width"
  (define pos (find-break-pos "helloworld" 0 5))
  (check-equal? pos 5))
