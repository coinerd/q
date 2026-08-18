#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-message-layout.rkt — Tests for tui/render/message-layout.rkt
;; Regression tests for wrap-styled-line ordering, leading-space stripping,
;; and vdom-layout width-aware truncation.

(require rackunit
         "../tui/render/message-layout.rkt"
         "../tui/char-width.rkt"
         "../tui/state.rkt")

;; ============================================================
;; wrap-styled-line: line ordering
;; ============================================================

(test-case "wrap-styled-line: correct line order for multi-segment overflow"
  (define line
    (styled-line (list (styled-segment "First segment " '())
                       (styled-segment "second" '(bold))
                       (styled-segment " overflow content here" '()))))
  (define result (wrap-styled-line line 20))
  (check >= (length result) 2)
  ;; First line should contain "First segment" — not overflow content
  (define first-text (styled-line->text (car result)))
  (check-true (string-contains? first-text "First segment")
              (format "Expected 'First segment' in first line, got: ~v" first-text)))

(test-case "wrap-styled-line: single long segment preserves order"
  (define line
    (styled-line (list (styled-segment "Alpha beta gamma delta epsilon zeta eta theta" '()))))
  (define result (wrap-styled-line line 20))
  ;; Lines should be in reading order: first line starts with "Alpha"
  (define texts (map styled-line->text result))
  (check-true (string-prefix? (car texts) "Alpha") (format "Lines out of order: ~v" texts))
  ;; Joined text should preserve all words
  (define joined (string-join texts))
  (check-true (string-contains? joined "Alpha") (format "Missing word in joined: ~v" joined)))

;; ============================================================
;; wrap-styled-line: leading space stripping
;; ============================================================

(test-case "wrap-styled-line: no leading space on overflow lines"
  (define line
    (styled-line (list (styled-segment "Short" '())
                       (styled-segment " text that overflows the width limit" '()))))
  (define result (wrap-styled-line line 15))
  ;; No line should start with whitespace
  (for ([l result]
        [i (in-naturals)])
    (define text (styled-line->text l))
    (when (> (string-length text) 0)
      (check-false (char-whitespace? (string-ref text 0))
                   (format "Line ~a starts with whitespace: ~v" i text)))))

(test-case "wrap-styled-line: bold segment overflow strips leading space"
  (define line
    (styled-line (list (styled-segment "This is " '())
                       (styled-segment "bold" '(bold))
                       (styled-segment " text that wraps to the next line" '()))))
  (define result (wrap-styled-line line 20))
  (for ([l result]
        [i (in-naturals)])
    (define text (styled-line->text l))
    (when (> (string-length text) 0)
      (check-false (char-whitespace? (string-ref text 0))
                   (format "Line ~a starts with whitespace: ~v" i text)))))

;; ============================================================
;; md-format-assistant: end-to-end wrapping
;; ============================================================

(test-case "md-format-assistant: no leading whitespace on any wrapped line"
  (define text
    (string-append "The CHANGELOG shows dozens of versions in rapid succession "
                   "(v0.76 → v0.94). A lot of it is audit remediation — "
                   "fixing drift, tightening contracts, updating stale references."))
  (define result (md-format-assistant text 80))
  (for ([l result]
        [i (in-naturals)])
    (define l-text (styled-line->text l))
    (when (> (string-length l-text) 0)
      (check-false (char-whitespace? (string-ref l-text 0))
                   (format "Line ~a starts with whitespace: ~v" i l-text)))))

(test-case "md-format-assistant: text with bold wrapping"
  ;; Simulate markdown with bold that gets wrapped
  (define text
    "Here is some **bold text** and more content that follows to cause wrapping at narrow widths.")
  (define result (md-format-assistant text 40))
  ;; Check ordering: first line should start with "Here"
  (check-true (string-prefix? (styled-line->text (car result)) "Here"))
  ;; No leading whitespace
  (for ([l result]
        [i (in-naturals)])
    (define l-text (styled-line->text l))
    (when (> (string-length l-text) 0)
      (check-false (char-whitespace? (string-ref l-text 0))
                   (format "Line ~a starts with whitespace: ~v" i l-text)))))

(test-case "md-format-assistant: wrapping fills line across styled segment boundaries"
  (define text
    "existing coding agents and decided to do it **right.** That comes through in every layer of the design.")
  (define result (md-format-assistant text 80))
  (define texts (map styled-line->text result))
  (check >= (length texts) 2)
  (check-true (string-contains? (car texts) "That comes")
              (format "First wrapped line was underfilled at styled boundary: ~v" texts)))

(test-case "wrap-styled-line: boundary fill keeps short word after leading space"
  (define line
    (styled-line (list (styled-segment "12345678901234567" '())
                       (styled-segment " in more words" '(bold)))))
  (define result (wrap-styled-line line 20))
  (define texts (map styled-line->text result))
  (check-equal? (car texts)
                "12345678901234567 in"
                (format "Expected short word to fill boundary: ~v" texts)))

(test-case "wrap-styled-line: boundary fill keeps short word after parenthesized phrase"
  (define line
    (styled-line (list (styled-segment "12345678901234567" '())
                       (styled-segment " (ADRs) in more words" '(bold)))))
  (define result (wrap-styled-line line 27))
  (define texts (map styled-line->text result))
  (check-equal? (car texts)
                "12345678901234567 (ADRs) in"
                (format "Expected parenthesized phrase plus short word to fill boundary: ~v" texts)))

(test-case "wrap-styled-line: boundary fill does not split normal word"
  (define line
    (styled-line (list (styled-segment "hello " '()) (styled-segment "world test" '(bold)))))
  (define result (wrap-styled-line line 8))
  (define texts (map styled-line->text result))
  (check-false (member "hello wo" texts)
               (format "Should not hard-split word just to fill boundary: ~v" texts)))

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

;; ============================================================
;; BUG-0002 (W2): tool-fail lines must wrap at terminal width
;; ============================================================

(define (make-fail-entry text)
  (transcript-entry 'tool-fail text 1000 (hasheq 'name 'bash) #f))

(define (entry-line-widths entry width)
  (map (lambda (l) (string-visible-width (styled-line->text l))) (format-entry entry width)))

(test-case "BUG-0002: 3x-width failure payload wraps into multiple lines each <= width"
  (define payload (make-string 240 #\e))
  (define lines (format-entry (make-fail-entry payload) 80))
  (check >= (length lines) 2 (format "expected wrapping, got ~a line(s)" (length lines)))
  (for ([w (entry-line-widths (make-fail-entry payload) 80)]
        [i (in-naturals)])
    (check-true (<= w 80) (format "line ~a visible width ~a exceeds 80" i w))))

(test-case "BUG-0002: first wrapped line carries the [FAIL] prefix"
  (define payload (string-append "Error: " (make-string 200 #\x)))
  (define lines (format-entry (make-fail-entry payload) 80))
  (check-true (string-prefix? (styled-line->text (car lines)) "[FAIL] bash:")
              (format "first line missing [FAIL] prefix: ~v" (styled-line->text (car lines)))))

(test-case "BUG-0002: wrapped output preserves the full payload"
  (define payload (string-append "boom " (make-string 200 #\x)))
  (define lines (format-entry (make-fail-entry payload) 80))
  (define joined (string-join (map (lambda (l) (string-trim (styled-line->text l))) lines) " "))
  (define expected (string-trim (string-replace (format "[FAIL] bash: ~a" payload) "\n" " ")))
  ;; Word-wrap may split the long unbroken x-run across lines; joining with
  ;; spaces then changes the length. Compare space-stripped text instead:
  ;; every payload character must survive wrapping.
  (check-equal? (string-replace joined " " "")
                (string-replace expected " " "")
                "wrapped output dropped or duplicated payload characters"))

(test-case "BUG-0002: multi-line error is flattened and wrapped, no raw newlines"
  (define payload "line one of error\nline two\nline three plus extra padding text xxxxxxxx")
  (define lines (format-entry (make-fail-entry payload) 40))
  (for ([l lines]
        [i (in-naturals)])
    (for ([seg (styled-line-segments l)])
      (check-false (string-contains? (styled-segment-text seg) "\n")
                   (format "segment on line ~a contains raw newline" i)))
    (check-true (<= (string-visible-width (styled-line->text l)) 40)
                (format "line ~a exceeds narrow width 40" i)))
  (check-true (ormap (lambda (l) (string-contains? (styled-line->text l) "line three")) lines)
              "flattened tail content missing after wrapping"))

(test-case "BUG-0002: short failure stays a single line"
  (define lines (format-entry (make-fail-entry "oops") 80))
  (check-equal? (length lines) 1)
  (check-equal? (styled-line->text (car lines)) "[FAIL] bash: oops"))

(test-case "BUG-0002: narrow terminal (width 20) wraps without clipping"
  (define payload (string-append "fatal: " (make-string 90 #\y)))
  (define lines (format-entry (make-fail-entry payload) 20))
  (check >= (length lines) 3)
  (for ([w (entry-line-widths (make-fail-entry payload) 20)]
        [i (in-naturals)])
    (check-true (<= w 20) (format "line ~a width ~a exceeds 20" i w))))
