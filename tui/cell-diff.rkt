#lang racket/base

;; q/tui/cell-diff.rkt — Cell-level incremental diff between two cell buffers
;;
;; Computes minimal deltas between two cell buffers for efficient rendering.
;; Uses row-hash shortcut: hash each row, only deep-diff cells in changed rows.
;; Returns list of cell-delta structs.

(require racket/contract
         racket/list
         "cell-buffer.rkt")

;; ============================================================
;; Cell delta struct
;; ============================================================

(struct cell-delta (col row old-cell new-cell) #:transparent)

;; ============================================================
;; Row hashing — fast change detection
;; ============================================================

;; Hash a single row by mixing cell data with a non-commutative function.
;; Not cryptographically secure — just needs to detect any change.
;; Uses h = h * 31 + v to avoid XOR-cancellation with even-width rows
;; of identical cells (a real bug observed with 4-column buffers).
(define (row-hash buf row)
  (define cols (cell-buffer-cols buf))
  (define h 0)
  (for ([c (in-range cols)])
    (define cell (cell-buffer-ref buf c row))
    ;; Combine char code + fg + bg + flags into a fixnum hash
    (define ch (char->integer (cell-char cell)))
    (define v
      (bitwise-xor ch
                   (arithmetic-shift (cell-fg cell) 8)
                   (arithmetic-shift (cell-bg cell) 16)
                   (if (cell-bold? cell) 1 0)
                   (if (cell-underline? cell) 2 0)
                   (if (cell-italic? cell) 4 0)
                   (if (cell-blink? cell) 8 0)))
    ;; Non-commutative mix: prime multiplication + addition.
    ;; Pure XOR fails when an even number of identical values cancel to 0.
    (set! h (+ (* 31 h) v)))
  h)

;; ============================================================
;; Cell diff — main algorithm
;; ============================================================

;; Compute cell-level diff between two cell buffers.
;; Returns list of cell-delta structs for changed cells.
;; Optimization: skip rows with matching hashes.
(define (diff-cell-buffers prev curr)
  (cond
    ;; No previous buffer — full diff (all cells changed)
    [(not prev)
     (define cols (cell-buffer-cols curr))
     (define rows (cell-buffer-rows curr))
     (for*/list ([r (in-range rows)]
                 [c (in-range cols)])
       (cell-delta c r #f (cell-buffer-ref curr c r)))]
    ;; Different dimensions — full diff
    [(or (not (= (cell-buffer-cols prev) (cell-buffer-cols curr)))
         (not (= (cell-buffer-rows prev) (cell-buffer-rows curr))))
     (define cols (cell-buffer-cols curr))
     (define rows (cell-buffer-rows curr))
     (for*/list ([r (in-range rows)]
                 [c (in-range cols)])
       (define old-cell
         (if (and (< c (cell-buffer-cols prev)) (< r (cell-buffer-rows prev)))
             (cell-buffer-ref prev c r)
             #f))
       (cell-delta c r old-cell (cell-buffer-ref curr c r)))]
    [else
     ;; Same dimensions — incremental diff with row-hash shortcut
     (define cols (cell-buffer-cols curr))
     (define rows (cell-buffer-rows curr))
     (apply append
            (for/list ([r (in-range rows)]
                       #:when (not (= (row-hash prev r) (row-hash curr r))))
              ;; Row changed — diff individual cells
              (for/list ([c (in-range cols)]
                         #:unless (cell-equal? (cell-buffer-ref prev c r) (cell-buffer-ref curr c r)))
                (cell-delta c r (cell-buffer-ref prev c r) (cell-buffer-ref curr c r)))))]))

;; ============================================================
;; Delta statistics
;; ============================================================

;; Count changed rows in a delta list
(define (delta-changed-rows deltas)
  (length (remove-duplicates (map cell-delta-row deltas))))

;; Count total changed cells
(define (delta-count deltas)
  (length deltas))

;; Check if delta represents a full redraw
(define (delta-full-redraw? deltas cols rows)
  (= (length deltas) (* cols rows)))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide cell-delta
         cell-delta?
         struct:cell-delta
         cell-delta-col
         cell-delta-row
         cell-delta-old-cell
         cell-delta-new-cell
         diff-cell-buffers
         row-hash
         delta-changed-rows
         delta-count
         delta-full-redraw?)
