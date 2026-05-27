#lang racket/base

;; q/tui/cell-buffer.rkt — Native 2D cell buffer (replaces tui-ubuf)
;;
;; A cell buffer is a 2D array of cells, each storing:
;;   character (char?), fg (0-255), bg (0-255), bold, underline, italic, blink
;;
;; Uses flat vectors for performance: cell at (col, row) is at index (+ (* row cols) col).
;; Thread-safe for single-writer (render loop) usage.

(require racket/contract)

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
;; Cell comparison (for diffing)
;; ============================================================

(define (cell-equal? a b)
  (and (eq? (cell-char a) (cell-char b))
       (= (cell-fg a) (cell-fg b))
       (= (cell-bg a) (cell-bg b))
       (eq? (cell-bold? a) (cell-bold? b))
       (eq? (cell-underline? a) (cell-underline? b))
       (eq? (cell-italic? a) (cell-italic? b))
       (eq? (cell-blink? a) (cell-blink? b))))

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

         ;; Row helpers
         cell-buffer-row-string

         ;; putstring (tui-ubuf compatible)
         cell-buffer-putstring!

         ;; Constants
         CELL-SIZE)
