#lang racket/base

;; q/tui/cell-buffer.rkt — Native 2D cell buffer (replaces tui-ubuf)
;;
;; A cell buffer is a 2D array of cells, each storing:
;;   character (char?), fg (0-255), bg (0-255), bold, underline, italic, blink
;;
;; Uses flat vectors for performance: cell at (col, row) is at index (+ (* row cols) col).
;; Thread-safe for single-writer (render loop) usage.

(require racket/contract
         "char-width.rkt")

;; ============================================================
;; Cell representation
;; ============================================================

;; A cell is a vector: #(char fg bg bold? underline? italic? blink?)
;; Index constants for clarity
(define CELL-CHAR-IDX 0)
(define CELL-FG-IDX 1)
(define CELL-BG-IDX 2)
(define CELL-BOLD-IDX 3)
(define CELL-UNDERLINE-IDX 4)
(define CELL-ITALIC-IDX 5)
(define CELL-BLINK-IDX 6)
(define CELL-SIZE 7)

;; Default cell: space, fg=7 (white), bg=0 (black), no attributes
(define (make-default-cell)
  (vector #\space 7 0 #f #f #f #f))

;; ============================================================
;; Cell buffer struct
;; ============================================================

(struct cell-buffer (cols rows cells dirty?) #:mutable)

;; ============================================================
;; Position contract (BUG-0100 / R100)
;; ============================================================

;; Raised when a cell position is outside the buffer on EITHER axis.
;; Descends from exn:fail:contract so generic exn:fail? handlers still
;; catch it; the payload carries the offending coordinates AND the actual
;; buffer dimensions for the diagnostic.
(struct exn:fail:cell-buffer-bounds exn:fail:contract (op col row cols rows) #:transparent)

;; Validate BOTH axes independently against the ACTUAL buffer dimensions
;; BEFORE any flattening. A flattened index alone cannot distinguish
;; (cols, r) from (0, r+1): on a 5x3 buffer, (col=5,row=0) lands on flat
;; index 5, which is cell (0,1); (col=-1,row=1) lands on index 4 == (4,0).
;; Coordinates are therefore validated per-axis and an index is never
;; computed from unvalidated values. Violations raise a descriptive
;; contract error — never alias, never silently clamp.
(define (check-cell-position who buf col row)
  (define cols (cell-buffer-cols buf))
  (define rows (cell-buffer-rows buf))
  (unless (and (exact-integer? col)
               (exact-integer? row)
               (>= col 0)
               (< col cols)
               (>= row 0)
               (< row rows))
    (raise
     (exn:fail:cell-buffer-bounds
      (format "~a: position (col=~a, row=~a) is outside buffer dimensions (cols=~a, rows=~a); ~a"
              who
              col
              row
              cols
              rows
              (cond
                [(not (exact-integer? col)) "col must be an exact integer"]
                [(not (exact-integer? row)) "row must be an exact integer"]
                [(or (< col 0) (>= col cols))
                 (format "col must be in [0,~a); refusing flattened-index aliasing" cols)]
                [else
                 (format "row must be in [0,~a); refusing flattened-index aliasing" rows)]))
      (current-continuation-marks)
      who
      col
      row
      cols
      rows))))

;; Both-axis bounds predicate against the ACTUAL buffer dimensions.
;; Used by the render loop to guard save/paint/restore/blink and
;; hardware/IME cursor positioning before any physical cell access.
(define (cell-buffer-in-bounds? buf col row)
  (and (exact-integer? col)
       (exact-integer? row)
       (>= col 0)
       (< col (cell-buffer-cols buf))
       (>= row 0)
       (< row (cell-buffer-rows buf))))

;; Create a new cell buffer filled with default cells.
;; Zero-sized surfaces (0 cols or 0 rows) are valid and produce an empty
;; cells vector; callers must treat them as "no visible cell" (deferred
;; cursor/frame). Negative dimensions are a contract error.
(define (make-cell-buffer cols rows)
  (unless (exact-integer? cols)
    (raise-argument-error 'make-cell-buffer "exact-integer? (>= 0)" cols))
  (unless (exact-integer? rows)
    (raise-argument-error 'make-cell-buffer "exact-integer? (>= 0)" rows))
  (when (or (negative? cols) (negative? rows))
    (raise
     (exn:fail:contract
      (format "make-cell-buffer: negative dimensions (cols=~a, rows=~a); zero-size is allowed, negative is not"
              cols
              rows)
      (current-continuation-marks))))
  (define cells (make-vector (* cols rows)))
  (for ([i (in-range (vector-length cells))])
    (vector-set! cells i (make-default-cell)))
  (cell-buffer cols rows cells #t))

;; ============================================================
;; Cell access and mutation
;; ============================================================

(define (cell-buffer-ref buf col row)
  (check-cell-position 'cell-buffer-ref buf col row)
  (define idx (+ (* row (cell-buffer-cols buf)) col))
  (vector-ref (cell-buffer-cells buf) idx))

(define (cell-buffer-set! buf
                          col
                          row
                          #:char [ch #\space]
                          #:fg [fg 7]
                          #:bg [bg 0]
                          #:bold [bold #f]
                          #:underline [underline #f]
                          #:italic [italic #f]
                          #:blink [blink #f])
  (check-cell-position 'cell-buffer-set! buf col row)
  (define idx (+ (* row (cell-buffer-cols buf)) col))
  (vector-set! (cell-buffer-cells buf) idx (vector ch fg bg bold underline italic blink))
  (set-cell-buffer-dirty?! buf #t))

;; Clear entire buffer to default cells
(define (cell-buffer-clear! buf)
  (for ([i (in-range (vector-length (cell-buffer-cells buf)))])
    (vector-set! (cell-buffer-cells buf) i (make-default-cell)))
  (set-cell-buffer-dirty?! buf #t))

;; Resize buffer (creates new buffer, copies existing cells that fit)
(define (cell-buffer-resize! buf new-cols new-rows)
  (define old-cols (cell-buffer-cols buf))
  (define old-rows (cell-buffer-rows buf))
  (define old-cells (cell-buffer-cells buf))
  (define new-cells (make-vector (* new-cols new-rows)))
  ;; Fill with defaults
  (for ([i (in-range (vector-length new-cells))])
    (vector-set! new-cells i (make-default-cell)))
  ;; Copy cells that fit
  (define copy-rows (min old-rows new-rows))
  (define copy-cols (min old-cols new-cols))
  (for* ([r (in-range copy-rows)]
         [c (in-range copy-cols)])
    (define old-idx (+ (* r old-cols) c))
    (define new-idx (+ (* r new-cols) c))
    (vector-set! new-cells new-idx (vector-ref old-cells old-idx)))
  (set-cell-buffer-cols! buf new-cols)
  (set-cell-buffer-rows! buf new-rows)
  (set-cell-buffer-cells! buf new-cells)
  (set-cell-buffer-dirty?! buf #t))

;; ============================================================
;; Cell attribute accessors
;; ============================================================

(define (cell-char cell)
  (vector-ref cell CELL-CHAR-IDX))
(define (cell-fg cell)
  (vector-ref cell CELL-FG-IDX))
(define (cell-bg cell)
  (vector-ref cell CELL-BG-IDX))
(define (cell-bold? cell)
  (vector-ref cell CELL-BOLD-IDX))
(define (cell-underline? cell)
  (vector-ref cell CELL-UNDERLINE-IDX))
(define (cell-italic? cell)
  (vector-ref cell CELL-ITALIC-IDX))
(define (cell-blink? cell)
  (vector-ref cell CELL-BLINK-IDX))

;; ============================================================
;; Continuation cell support
;; ============================================================

;; Continuation cell marker: cells that are the 2nd+ column of a wide character.
;; These cells should be skipped during delta rendering.
(define CONTINUATION-CHAR #\nul)

;; Check if a cell is a continuation cell (2nd+ column of a wide char).
(define (continuation-cell? cell)
  (eq? (cell-char cell) CONTINUATION-CHAR))

;; ============================================================
;; Cell comparison (for diffing)
;; ============================================================

(define (cell-equal? a b)
  (cond
    ;; Both continuation cells with same style → equal
    [(and (continuation-cell? a) (continuation-cell? b))
     (and (= (cell-fg a) (cell-fg b))
          (= (cell-bg a) (cell-bg b))
          (eq? (cell-bold? a) (cell-bold? b))
          (eq? (cell-underline? a) (cell-underline? b))
          (eq? (cell-italic? a) (cell-italic? b))
          (eq? (cell-blink? a) (cell-blink? b)))]
    ;; Both regular cells with same char and style → equal
    [(and (not (continuation-cell? a)) (not (continuation-cell? b)))
     (and (eq? (cell-char a) (cell-char b))
          (= (cell-fg a) (cell-fg b))
          (= (cell-bg a) (cell-bg b))
          (eq? (cell-bold? a) (cell-bold? b))
          (eq? (cell-underline? a) (cell-underline? b))
          (eq? (cell-italic? a) (cell-italic? b))
          (eq? (cell-blink? a) (cell-blink? b)))]
    ;; Mixed (one continuation, one not) → not equal
    [else #f]))

;; ============================================================
;; Snapshot — deep-copy all cells into a new buffer
;; ============================================================

;; Create an independent copy of the entire cell buffer.
;; Each 7-element cell vector is shallow-copied (sufficient
;; since cells contain only immutable scalars: char, int, bool).
(define (cell-buffer-snapshot buf)
  (define cols (cell-buffer-cols buf))
  (define rows (cell-buffer-rows buf))
  (define snap (make-cell-buffer cols rows))
  (for* ([r (in-range rows)]
         [c (in-range cols)])
    (define idx (+ (* r cols) c))
    (vector-set! (cell-buffer-cells snap)
                 idx
                 (let ([src (vector-ref (cell-buffer-cells buf) idx)])
                   (vector (vector-ref src 0)
                           (vector-ref src 1)
                           (vector-ref src 2)
                           (vector-ref src 3)
                           (vector-ref src 4)
                           (vector-ref src 5)
                           (vector-ref src 6)))))
  snap)

;; ============================================================
;; Row helpers
;; ============================================================

;; Extract row as a string of characters
(define (cell-buffer-row-string buf row)
  (define cols (cell-buffer-cols buf))
  (define chars (make-string cols))
  (for ([c (in-range cols)])
    (string-set! chars c (cell-char (cell-buffer-ref buf c row))))
  chars)

;; ============================================================
;; putstring! — write a string with attributes to the buffer
;; ============================================================

;; Compatible with tui-ubuf signature:
;;   (ubuf-putstring! ubuf x y str #:fg #:bg #:bold #:underline #:italic #:blink)
(define (cell-buffer-putstring! buf
                                col
                                row
                                str
                                #:fg [fg 7]
                                #:bg [bg 0]
                                #:bold [bold #f]
                                #:underline [underline #f]
                                #:italic [italic #f]
                                #:blink [blink #f])
  (define cols (cell-buffer-cols buf))
  (define rows (cell-buffer-rows buf))
  (define len (string-length str))
  (for ([i (in-range len)])
    (define c (+ col i))
    (when (and (< c cols) (< row rows) (>= c 0) (>= row 0))
      (cell-buffer-set! buf
                        c
                        row
                        #:char (string-ref str i)
                        #:fg fg
                        #:bg bg
                        #:bold bold
                        #:underline underline
                        #:italic italic
                        #:blink blink))))

;; ============================================================
;; Width-aware putstring — aligns cells with terminal display columns
;; ============================================================

;; Write a string to the cell buffer, advancing column by display width.
;; Width-2+ characters (emoji, CJK) occupy consecutive cells:
;;   - Base cell at col N stores the actual character
;;   - Continuation cells at col N+1 .. N+W-1 store CONTINUATION-CHAR
;; Width-0 characters (combining marks) are absorbed into the previous cell.
(define (cell-buffer-width-aware-putstring! buf
                                            col
                                            row
                                            str
                                            #:fg [fg 7]
                                            #:bg [bg 0]
                                            #:bold [bold #f]
                                            #:underline [underline #f]
                                            #:italic [italic #f]
                                            #:blink [blink #f])
  (define cols (cell-buffer-cols buf))
  (define rows (cell-buffer-rows buf))
  (define len (string-length str))
  (define display-col col)
  (for ([i (in-range len)])
    (define ch (string-ref str i))
    (define w (char-width ch))
    (cond
      ;; Width-0: combining mark / variation selector — skip cell position
      ;; (base char already written to current display-col)
      [(= w 0) (void)]
      ;; Buffer overflow protection
      [(>= display-col cols) (void)]
      [else
       ;; Write the character to the base cell
       (when (and (< display-col cols) (< row rows) (>= display-col 0) (>= row 0))
         (cell-buffer-set! buf
                           display-col
                           row
                           #:char ch
                           #:fg fg
                           #:bg bg
                           #:bold bold
                           #:underline underline
                           #:italic italic
                           #:blink blink))
       ;; Mark subsequent cells as continuation
       (for ([j (in-range 1 w)])
         (define cont-col (+ display-col j))
         (when (and (< cont-col cols) (< row rows) (>= cont-col 0) (>= row 0))
           (cell-buffer-set! buf
                             cont-col
                             row
                             #:char CONTINUATION-CHAR
                             #:fg fg
                             #:bg bg
                             #:bold bold
                             #:underline underline
                             #:italic italic
                             #:blink blink)))
       (set! display-col (+ display-col w))])))

;; ============================================================
;; Logical end-of-line insertion sentinel → physical cursor projection
;; (BUG-0100 / R100)
;; ============================================================
;;
;; The shared composer layout reports the END-OF-LINE INSERTION POINT as
;; cursor-col == row-width (a logical sentinel): there is no CELL at
;; column == width — insertion happens after the last visible column.
;; That logical coordinate must never reach the cell buffer: flattened,
;; it aliases into the next row (or overflows on the last row).
;; Renderers project it onto the row's last REAL visible cell
;; (end-of-line placement) before painting, blinking, or positioning any
;; cursor. Logical offsets, grapheme navigation, and the composer
;; boundary expectation (col == width stays logical) are unchanged — the
;; projection lives only at the physical cell boundary and is shared by
;; the painting and cursor paths.

;; Is this logical column the end-of-line insertion sentinel for a row of
;; the given display width?
(define (logical-cursor-eol-sentinel? logical-col logical-row-width)
  (and (exact-integer? logical-col)
       (exact-integer? logical-row-width)
       (= logical-col logical-row-width)))

;; Project a logical cursor (col, row), computed for a visual row of
;; display width `logical-row-width`, onto a physical cell of a
;; cols x rows buffer. Returns (values col row) naming a REAL visible
;; cell, or (values #f #f) when no physical cell exists: zero-size
;; surface, cursor outside the buffer, or layout/buffer geometry so
;; inconsistent that the logical position has no visible cell. Refuses —
;; never clamps — so callers defer cursor drawing explicitly.
(define (project-logical-cursor logical-col logical-row logical-row-width cols rows)
  (cond
    [(or (not (exact-integer? logical-col))
         (not (exact-integer? logical-row))
         (not (exact-integer? logical-row-width))
         (< cols 1)
         (< rows 1)
         (< logical-row 0)
         (>= logical-row rows)
         (< logical-col 0)
         (> logical-col logical-row-width))
     (values #f #f)]
    [(= logical-col logical-row-width)
     ;; End-of-line insertion sentinel → the row's last visible cell,
     ;; which must exist in the ACTUAL buffer (refuse otherwise).
     (define last-col (sub1 logical-row-width))
     (if (< last-col cols) (values last-col logical-row) (values #f #f))]
    [else
     (if (< logical-col cols) (values logical-col logical-row) (values #f #f))]))

;; ============================================================
;; Contracts and exports
;; ============================================================

;; Struct and constructor
(provide (struct-out cell-buffer)
         make-cell-buffer

         ;; Position contract (R100)
         (struct-out exn:fail:cell-buffer-bounds)
         check-cell-position
         cell-buffer-in-bounds?

         ;; Logical EOL sentinel → physical cursor projection (R100)
         logical-cursor-eol-sentinel?
         project-logical-cursor
         cell-buffer-ref
         cell-buffer-set!
         cell-buffer-clear!
         cell-buffer-resize!

         ;; Cell attribute accessors
         cell-char
         cell-fg
         cell-bg
         cell-bold?
         cell-underline?
         cell-italic?
         cell-blink?

         ;; Cell comparison
         cell-equal?

         ;; Snapshot
         cell-buffer-snapshot

         ;; Row helpers
         cell-buffer-row-string

         ;; putstring (tui-ubuf compatible)
         cell-buffer-putstring!

         ;; Width-aware putstring (emoji/CJK safe)
         cell-buffer-width-aware-putstring!
         continuation-cell?
         CONTINUATION-CHAR

         ;; Constants
         CELL-SIZE)
