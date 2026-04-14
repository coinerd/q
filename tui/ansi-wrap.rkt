#lang racket/base

;; q/tui/ansi-wrap.rkt — ANSI-aware word wrapping
;;
;; Wraps text that may contain ANSI/SGR escape sequences.
;; Escape sequences have zero visual width and should not be broken.
;;
;; NOTE: The main render pipeline uses styled-line structs (not raw ANSI
;; strings), so this module is not called from render.rkt. It is kept as a
;; utility for extensions, external consumers, or future ANSI-text paths.
;; Verified to compile and pass tests (#469).

(require racket/string
         "char-width.rkt")

(provide wrap-ansi-line
         wrap-ansi-text
         ansi-visible-width
         find-ansi-break-pos)

;; ============================================================
;; ANSI-aware visible width
;; ============================================================

;; ansi-visible-width : String → Natural
;; Returns the visual column width, skipping ANSI escape sequences.
(define (ansi-visible-width s)
  (let loop ([i 0] [col 0])
    (cond
      [(>= i (string-length s)) col]
      [else
       (define c (string-ref s i))
       (cond
         ;; Start of escape sequence — skip to the end
         [(eq? c (integer->char 27))
          (define end (skip-ansi-sequence s i))
          (loop end col)]
         [else
          (loop (add1 i) (+ col (char-width c)))])])))

;; Skip a complete ANSI escape sequence starting at position i.
;; Returns the index after the sequence.
;; Handles CSI sequences (ESC [ ... final-byte).
(define (skip-ansi-sequence s i)
  (cond
    [(>= (add1 i) (string-length s)) (string-length s)]
    [(eq? (string-ref s (add1 i)) #\[)
     ;; CSI sequence: ESC [ <params> <final-byte>
     (let loop ([j (+ i 2)])
       (cond
         [(>= j (string-length s)) (string-length s)]
         [else
          (define c (string-ref s j))
          (if (ansi-final-byte? c)
              (add1 j)
              (loop (add1 j)))]))]
    ;; Non-CSI escape (ESC X): skip 2 bytes
    [else (+ i 2)]))

;; Check if a byte is a CSI final byte (0x40–0x7E)
(define (ansi-final-byte? c)
  (define cp (char->integer c))
  (and (>= cp #x40) (<= cp #x7E)))

;; ============================================================
;; ANSI-aware wrapping
;; ============================================================

;; wrap-ansi-line : String Natural → (Listof String)
;; Wraps a single line that may contain ANSI escape sequences.
;; Preserves escape sequences intact — never breaks inside one.
(define (wrap-ansi-line line max-width)
  (if (<= (ansi-visible-width line) max-width)
      (list line)
      (let loop ([remaining line]
                 [acc '()])
        (define vw (ansi-visible-width remaining))
        (cond
          [(<= vw max-width) (reverse (cons remaining acc))]
          [else
           (define break-pos (find-ansi-break-pos remaining max-width))
           (cond
             [(<= break-pos 0)
              ;; Cannot break — emit the whole thing as one line
              (reverse (cons remaining acc))]
             [else
              (loop (substring remaining break-pos)
                    (cons (substring remaining 0 break-pos) acc))])]))))

;; find-ansi-break-pos : String Natural → Natural
;; Finds a break position in a string that may contain ANSI sequences.
;; Returns a character index (not column index) to break at.
;; Prefers breaking at whitespace. Skips escape sequences as zero-width.
(define (find-ansi-break-pos text max-width)
  (let loop ([i 0]
             [col 0]
             [last-space-idx #f]
             [last-space-col 0])
    (cond
      [(>= i (string-length text)) (string-length text)]
      ;; Exceeded visible width
      [(>= col max-width)
       (cond
         [last-space-idx last-space-idx]
         ;; No space found — break at current position
         [else i])]
      [else
       (define c (string-ref text i))
       (cond
         ;; Escape sequence — skip entirely
         [(eq? c (integer->char 27))
          (define end (skip-ansi-sequence text i))
          (loop end col last-space-idx last-space-col)]
         ;; Whitespace — record potential break point
         [(char-whitespace? c)
          (loop (add1 i) (+ col (char-width c)) (add1 i) (+ col (char-width c)))]
         ;; Regular character
         [else
          (define w (char-width c))
          (define new-col (+ col w))
          (cond
            [(> new-col max-width)
             (cond
               [last-space-idx last-space-idx]
               ;; No space found — break at current position
               [else i])]
            [else
             (loop (add1 i) new-col last-space-idx last-space-col)])])])))

;; ============================================================
;; Convenience: wrap ANSI text with newline splitting
;; ============================================================

;; wrap-ansi-text : String Natural → (Listof String)
;; Splits on newlines first, then wraps each line.
(define (wrap-ansi-text text max-width)
  (if (<= max-width 0)
      (list text)
      (let ([lines (string-split text "\n")])
        (apply append
               (for/list ([line (in-list lines)])
                 (wrap-ansi-line line max-width))))))
