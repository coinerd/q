#lang racket/base

;; q/tui/char-width.rkt — Unicode-aware terminal column width
;;
;; Provides functions for computing the visual column width of characters
;; and strings in a terminal. CJK characters consume 2 columns, combining
;; marks and control characters consume 0, and everything else is 1.
;;
;; Reference: Unicode East Asian Width property (UAX #11)

(require racket/contract
         "../util/error-helpers.rkt")
(require racket/match
         racket/string)

(provide (contract-out
          [char-width (-> char? exact-nonnegative-integer?)]
          [string-visible-width (-> string? exact-nonnegative-integer?)]
          [visible-width (-> string? exact-nonnegative-integer?)]
          [truncate-to-visible-width (-> string? exact-nonnegative-integer? string?)]
          [display-col->string-offset
           (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          [grapheme-span-at (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          [grapheme-count (-> string? exact-nonnegative-integer?)]
          [string-grapheme-length (-> string? exact-nonnegative-integer?)]
          [substring-by-graphemes
           (->* (string? exact-nonnegative-integer?) (exact-nonnegative-integer?) string?)]))

;; char-width : Char → {0, 1, 2}
;; Returns the terminal column width of a single character.
(define (char-width c)
  (define cp (char->integer c))
  (match cp
    ;; Control characters (C0/C1): width 0
    [(? (lambda (v) (< v #x20))) 0]
    [#x7F 0]
    [(? (lambda (v) (<= #x80 v #x9F))) 0]

    ;; Combining Diacritical Marks (U+0300–U+036F): width 0
    [(? (lambda (v) (<= #x0300 v #x036F))) 0]

    ;; Combining Diacritical Marks Extended (U+1AB0–U+1AFF): width 0
    [(? (lambda (v) (<= #x1AB0 v #x1AFF))) 0]

    ;; Combining Diacritical Marks Supplement (U+1DC0–U+1DFF): width 0
    [(? (lambda (v) (<= #x1DC0 v #x1DFF))) 0]

    ;; Combining Diacritical Marks for Symbols (U+20D0–U+20FF): width 0
    [(? (lambda (v) (<= #x20D0 v #x20FF))) 0]

    ;; Combining Half Marks (U+FE20–U+FE2F): width 0
    [(? (lambda (v) (<= #xFE20 v #xFE2F))) 0]

    ;; Variation Selectors (U+FE00–U+FE0F): width 0
    [(? (lambda (v) (<= #xFE00 v #xFE0F))) 0]

    ;; Variation Selectors Supplement (U+E0100–U+E01EF): width 0
    [(? (lambda (v) (<= #xE0100 v #xE01EF))) 0]

    ;; Zero Width Joiner (U+200D), Zero Width Non-Joiner (U+200C),
    ;; Zero Width Space (U+200B), Word Joiner (U+2060): width 0
    [#x200B 0]
    [#x200C 0]
    [#x200D 0]
    [#x2060 0]

    ;; Left-to-Right / Right-to-Left marks: width 0
    [#x200E 0]
    [#x200F 0]
    [(? (lambda (v) (<= #x202A v #x202E))) 0]
    [(? (lambda (v) (<= #x2066 v #x2069))) 0]

    ;; Hangul Jamo (U+1100–U+11FF): width 2
    [(? (lambda (v) (<= #x1100 v #x11FF))) 2]

    ;; CJK Radicals Supplement (U+2E80–U+2EFF): width 2
    [(? (lambda (v) (<= #x2E80 v #x2EFF))) 2]

    ;; Kangxi Radicals (U+2F00–U+2FDF): width 2
    [(? (lambda (v) (<= #x2F00 v #x2FDF))) 2]

    ;; CJK Symbols and Punctuation (U+3000–U+303F): width 2
    [(? (lambda (v) (<= #x3000 v #x303F))) 2]

    ;; Hiragana (U+3040–U+309F): width 2
    [(? (lambda (v) (<= #x3040 v #x309F))) 2]

    ;; Katakana (U+30A0–U+30FF): width 2
    [(? (lambda (v) (<= #x30A0 v #x30FF))) 2]

    ;; Bopomofo (U+3100–U+312F): width 2
    [(? (lambda (v) (<= #x3100 v #x312F))) 2]

    ;; Hangul Compatibility Jamo (U+3130–U+318F): width 2
    [(? (lambda (v) (<= #x3130 v #x318F))) 2]

    ;; CJK Unified Ideographs (U+4E00–U+9FFF): width 2
    [(? (lambda (v) (<= #x4E00 v #x9FFF))) 2]

    ;; Hangul Syllables (U+AC00–U+D7AF): width 2
    [(? (lambda (v) (<= #xAC00 v #xD7AF))) 2]

    ;; CJK Compatibility Ideographs (U+F900–U+FAFF): width 2
    [(? (lambda (v) (<= #xF900 v #xFAFF))) 2]

    ;; Halfwidth and Fullwidth Forms (U+FF00–U+FFEF): width 2
    [(? (lambda (v) (<= #xFF01 v #xFF60))) 2] ; Fullwidth
    [(? (lambda (v) (<= #xFFE0 v #xFFE6))) 2] ; Fullwidth signs

    ;; Miscellaneous Symbols (U+2600–U+26FF): width 2 (emoji-like)
    [(? (lambda (v) (<= #x2600 v #x26FF))) 2]

    ;; Dingbats (U+2700–U+27BF): width 2
    [(? (lambda (v) (<= #x2700 v #x27BF))) 2]

    ;; CJK Unified Ideographs Extension A (U+3400–U+4DBF): width 2
    [(? (lambda (v) (<= #x3400 v #x4DBF))) 2]

    ;; CJK Unified Ideographs Extension B–I (U+20000–U+323AF): width 2
    [(? (lambda (v) (<= #x20000 v #x323AF))) 2]

    ;; CJK Compatibility Ideographs Supplement (U+2F800–U+2FA1F): width 2
    [(? (lambda (v) (<= #x2F800 v #x2FA1F))) 2]

    ;; Default: width 1 (ASCII, Latin, Cyrillic, etc.)
    [_ 1]))

;; string-visible-width : String → Natural
;; Returns the total visual column width of a string.
(define (string-visible-width s)
  (for/sum ([c (in-string s)]) (char-width c)))

;; Alias for convenience
(define visible-width string-visible-width)

;; truncate-to-visible-width : String Natural → String
;; Truncates a string to fit within max-cols visible columns.
;; Returns the longest prefix whose visible width is <= max-cols.
(define (truncate-to-visible-width s max-cols)
  (let loop ([i 0]
             [col 0])
    (cond
      [(>= i (string-length s)) s]
      [(>= col max-cols) (substring s 0 i)]
      [else
       (define c (string-ref s i))
       (define w (char-width c))
       (cond
         [(> (+ col w) max-cols) (substring s 0 i)]
         [else (loop (add1 i) (+ col w))])])))

;; ============================================================
;; Grapheme cluster utilities (UAX #29)
;; ============================================================

;; Fallback grapheme span for Racket < 8.12 (no string-grapheme-span).
;; Covers the common cases: base char + combining marks, base + VS16,
;; flag pairs (regional indicators), and ZWJ sequences.
;; Returns the codepoint span of the grapheme starting at position i.
(define (fallback-grapheme-span s i)
  (define len (string-length s))
  (cond
    [(>= i len) 0]
    [else
     (define c0 (char->integer (string-ref s i)))
     (define (combining? j)
       (and (< j len)
            (let ([cp (char->integer (string-ref s j))])
              (or (and (>= cp #x0300) (<= cp #x036F))
                  (and (>= cp #x1AB0) (<= cp #x1AFF))
                  (and (>= cp #x1DC0) (<= cp #x1DFF))
                  (and (>= cp #x20D0) (<= cp #x20FF))
                  (and (>= cp #xFE20) (<= cp #xFE2F))
                  (and (>= cp #xFE00) (<= cp #xFE0F))
                  (and (>= cp #xE0100) (<= cp #xE01EF))
                  (= cp #x200B)
                  (= cp #x200C)
                  (= cp #x200D)
                  (= cp #x2060)
                  (= cp #x200E)
                  (= cp #x200F)))))
     ;; Regional indicator pair (flag emoji)
     (define (regional-indicator? cp)
       (and (>= cp #x1F1E6) (<= cp #x1F1FF)))
     (let loop ([j (add1 i)])
       (cond
         [(>= j len) (- j i)]
         [(combining? j) (loop (add1 j))]
         ;; ZWJ followed by another char (emoji ZWJ sequence)
         [(and (= (char->integer (string-ref s (sub1 j))) #x200D) (< j len)) (loop (add1 j))]
         ;; Second regional indicator
         [(and (= (- j i) 1)
               (regional-indicator? c0)
               (regional-indicator? (char->integer (string-ref s j))))
          (loop (add1 j))]
         [else (- j i)]))]))

;; Use Racket's string-grapheme-span if available (8.12+), else fallback.
;; dynamic-require avoids compile-time unbound identifier on Racket < 8.12
(define grapheme-span-impl
  (with-safe-fallback #f (dynamic-require 'racket/string 'string-grapheme-span)))

(define (grapheme-span-at-impl s i)
  (if (>= i (string-length s))
      0
      (if grapheme-span-impl
          (grapheme-span-impl s i)
          (fallback-grapheme-span s i))))

;; grapheme-span-at : String Natural → Natural
;; Returns the codepoint span of the grapheme cluster starting at position i.
;; If i is past the end, returns 0.
(define grapheme-span-at grapheme-span-at-impl)

;; grapheme-count : String → Natural
;; Returns the number of grapheme clusters in the string.
(define (grapheme-count s)
  (let loop ([i 0]
             [count 0])
    (cond
      [(>= i (string-length s)) count]
      [else
       (define span (grapheme-span-at-impl s i))
       (loop (+ i span) (+ count 1))])))

;; Alias
(define string-grapheme-length grapheme-count)

;; substring-by-graphemes : String Natural [Natural] → String
;; Returns substring by grapheme cluster indices.
;; start-gc and end-gc are grapheme cluster offsets (0-based).
(define (substring-by-graphemes s start-gc [end-gc #f])
  (define start-offset (grapheme-offset->string-offset s start-gc))
  (define end-offset
    (if end-gc
        (grapheme-offset->string-offset s end-gc)
        (string-length s)))
  (substring s start-offset end-offset))

;; grapheme-offset->string-offset : String Natural → Natural
;; Convert a grapheme cluster offset to a string (codepoint) offset.
;; Returns the string position of the start of the Nth grapheme.
(define (grapheme-offset->string-offset s gc)
  (let loop ([i 0]
             [n 0])
    (cond
      [(>= n gc) i]
      [(>= i (string-length s)) (string-length s)]
      [else
       (define span (grapheme-span-at-impl s i))
       (loop (+ i span) (+ n 1))])))

;; prev-grapheme-start : String Natural → Natural
;; Given a cursor position (codepoint offset), find the start of the
;; previous grapheme cluster. Returns 0 if already at start.
(define (prev-grapheme-start s pos)
  (if (zero? pos)
      0
      ;; Walk from beginning to find the grapheme boundary before pos
      (let loop ([i 0])
        (define span (grapheme-span-at-impl s i))
        (define next (+ i span))
        (cond
          [(>= next pos) i]
          [(>= next (string-length s)) i]
          [else (loop next)]))))

;; next-grapheme-start : String Natural → Natural
;; Given a cursor position, find the start of the next grapheme cluster.
;; Returns (string-length s) if at end.
(define (next-grapheme-start s pos)
  (if (>= pos (string-length s))
      (string-length s)
      (+ pos (grapheme-span-at-impl s pos))))

(provide (contract-out
          [prev-grapheme-start (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          [next-grapheme-start (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]))

;; ============================================================
;; Display column utilities
;; ============================================================

;; display-col->string-offset : String Natural → Natural
;; Given a display column (0-based), find the corresponding string offset.
;; CJK chars consume 2 display columns but 1 string position.
;; Returns (string-length s) if col is past the end.
(define (display-col->string-offset s col)
  (let loop ([i 0]
             [display-pos 0])
    (cond
      [(>= i (string-length s)) (string-length s)]
      [(>= display-pos col) i]
      [else
       (define w (char-width (string-ref s i)))
       (define next-pos (+ display-pos w))
       (if (> next-pos col)
           i ;; col falls inside this wide char — snap to its start
           (loop (+ i 1) next-pos))])))
