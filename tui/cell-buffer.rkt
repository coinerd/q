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

;; Create a new cell buffer filled with default cells
(define (make-cell-buffer cols rows)
  (define cells (make-vector (* cols rows)))
  (for ([i (in-range (vector-length cells))])
    (vector-set! cells i (make-default-cell)))
  (cell-buffer cols rows cells #t))

;; ============================================================
;; Cell access and mutation
;; ============================================================

(define (cell-buffer-ref buf col row)
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
         (when (and (< cont-col cols) (< row rows))
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
;; Contracts and exports
;; ============================================================

;; Struct and constructor
(provide (struct-out cell-buffer)
         make-cell-buffer

         ;; Cell access
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
