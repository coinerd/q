#lang racket/base

;; q/tui/cell-diff-render.rkt — Converts cell deltas to minimal ANSI output
;;
;; Takes a list of cell-delta structs and a cell buffer, produces
;; minimal ANSI escape sequences with cursor positioning + SGR changes.
;; Groups consecutive changes into single CSI sequences where possible.
;; Uses DEC 2026 (Synchronized Output) when available.

(require racket/contract
         racket/list
         racket/string
         "cell-buffer.rkt"
         "cell-diff.rkt"
         "terminal.rkt")

;; ============================================================
;; SGR helpers
;; ============================================================

;; Build an SGR sequence string for a cell's attributes
(define (cell->sgr cell)
  (define bold? (cell-bold? cell))
  (define underline? (cell-underline? cell))
  (define fg (cell-fg cell))
  (define bg (cell-bg cell))
  (string-append "\x1b[0"
                 (if bold? ";1" "")
                 (if underline? ";4" "")
                 (format ";38;5;~a" fg)
                 (format ";48;5;~a" (if (= bg 0) 16 bg))
                 "m"))

;; ============================================================
;; Terminal render guards
;; ============================================================

(define AUTOWRAP-OFF "\x1b[?7l")
(define AUTOWRAP-ON "\x1b[?7h")

(define (call-with-terminal-render-guards out sync? thunk)
  (dynamic-wind (lambda ()
                  (when sync?
                    (terminal-sync-begin!))
                  ;; Disable auto-wrap to prevent pending-wrap corruption when output reaches
                  ;; the terminal's last column.
                  (display AUTOWRAP-OFF out))
                thunk
                (lambda ()
                  ;; Always restore auto-wrap, even if rendering raises.
                  (display AUTOWRAP-ON out)
                  (when sync?
                    (terminal-sync-end!)))))

;; ============================================================
;; Render deltas to output port
;; ============================================================

;; F-TUI-03 (v0.99.16 W2): Check if a cell is a default/blank cell.
;; Used to determine whether to emit ESC[K after a row's deltas.
(define (default-cell? cell)
  (and (char=? (cell-char cell) #\space)
       (= (cell-fg cell) 7)
       (= (cell-bg cell) 0)
       (not (cell-bold? cell))
       (not (cell-underline? cell))
       (not (cell-italic? cell))
       (not (cell-blink? cell))))

;; Render cell deltas to an output port as minimal ANSI sequences.
;; Deltas should be sorted by position (row-major order) for optimal grouping.
;; F-TUI-03 (v0.99.16 W2): After each row's last delta, if the last cell
;; is a default/blank cell, emit ESC[K to clear residual content to the
;; right. This prevents display corruption when row content is shortened
;; and the terminal's actual state has drifted from the snapshot.
(define (render-deltas-to-port! deltas buf out #:sync? [sync? #t])
  (call-with-terminal-render-guards
   out
   sync?
   (lambda ()
     (define prev-sgr (box #f))
     ;; Filter out continuation cells — they are rendered as part of their base char
     (define real-deltas
       (filter (lambda (d) (not (continuation-cell? (cell-delta-new-cell d)))) deltas))
     ;; F-TUI-03 (v0.99.16 W2): Group deltas by row for row-end ESC[K emission.
     ;; Deltas from diff-cell-buffers are already sorted by row then column.
     (define row-groups (group-by cell-delta-row real-deltas))
     (for ([row-group (in-list row-groups)])
       ;; Render all batches in this row group
       (let loop ([remaining row-group])
         (cond
           [(null? remaining) (void)]
           [else
            (define d (car remaining))
            (define col (cell-delta-col d))
            (define row (cell-delta-row d))
            (define new-cell (cell-delta-new-cell d))
            (define sgr (cell->sgr new-cell))
            ;; Position cursor: CSI row+1 ; col+1 H
            (display (format "\x1b[~a;~aH" (add1 row) (add1 col)) out)
            ;; Apply SGR only if changed
            (unless (equal? sgr (unbox prev-sgr))
              (display sgr out)
              (set-box! prev-sgr sgr))
            ;; Collect consecutive cells with same SGR
            (define chars (list (cell-char new-cell)))
            (let gather ([rest (cdr remaining)]
                         [acc chars]
                         [next-col (add1 col)])
              (cond
                ;; Emit batch — last batch in this row group
                [(null? rest) (display (list->string (reverse acc)) out)]
                [else
                 (define nd (car rest))
                 (define nd-col (cell-delta-col nd))
                 (define nd-cell (cell-delta-new-cell nd))
                 (define nd-sgr (cell->sgr nd-cell))
                 (cond
                   [(and (= nd-col next-col) (equal? nd-sgr sgr))
                    ;; Consecutive column, same style — batch continues
                    (gather (cdr rest) (cons (cell-char nd-cell) acc) (add1 next-col))]
                   [else
                    ;; Batch ends — emit accumulated chars
                    (display (list->string (reverse acc)) out)
                    (loop rest)])]))]))
       ;; F-TUI-03 (v0.99.16 W2): After the last delta in this row, if the
       ;; last cell is a default/blank cell, emit ESC[K to clear any residual
       ;; content to the right (defensive against snapshot drift).
       (when (default-cell? (cell-delta-new-cell (last row-group)))
         (display "\x1b[K" out))))))

;; ============================================================
;; Full buffer render (for first frame or resize)
;; ============================================================

;; Render entire cell buffer to output port.
(define (render-buffer-to-port! buf out #:sync? [sync? #t])
  (define cols (cell-buffer-cols buf))
  (define rows (cell-buffer-rows buf))
  (call-with-terminal-render-guards
   out
   sync?
   (lambda ()
     ;; Home cursor
     (display "\x1b[H" out)
     (define prev-sgr #f)
     (for ([row (in-range rows)])
       (when (> row 0)
         ;; Use cursor positioning instead of newline to avoid wrap issues
         (display (format "\x1b[~a;1H" (add1 row)) out))
       (for ([col (in-range cols)])
         (define cell (cell-buffer-ref buf col row))
         (cond
           ;; Skip continuation cells — they are part of the base char's display width
           [(continuation-cell? cell) (void)]
           [else
            (define sgr (cell->sgr cell))
            (unless (equal? sgr prev-sgr)
              (display sgr out)
              (set! prev-sgr sgr))
            (display (string (cell-char cell)) out)]))
       ;; Reset SGR to prevent inverse/bold bleed, then re-apply the
       ;; row's ambient background so ESC[K clears with the correct color.
       ;; The ambient bg is taken from the first non-continuation cell.
       (let ambient-bg ([c 0])
         (if (and (< c cols) (continuation-cell? (cell-buffer-ref buf c row)))
             (ambient-bg (add1 c))
             (let ([row-bg (if (< c cols)
                               (cell-bg (cell-buffer-ref buf c row))
                               0)])
               (display "\x1b[0m" out)
               (set! prev-sgr #f)
               ;; Re-apply row's ambient background after reset
               (display (format "\x1b[48;5;~am" (if (= row-bg 0) 16 row-bg)) out)
               (display "\x1b[K" out))))))))

;; ============================================================
;; Smart render: auto-select full vs incremental
;; ============================================================

;; If deltas represent > 50% of all cells, do a full render instead.
;; v0.99.58 W4-1 (P4-C): Parameterized for performance tuning.
(define current-full-render-threshold (make-parameter 0.5))
(define FULL-RENDER-THRESHOLD 0.5) ;; kept as value alias

(define (render-smart! prev-buf curr-buf out #:sync? [sync? #t])
  (define cols (cell-buffer-cols curr-buf))
  (define rows (cell-buffer-rows curr-buf))
  (define total-cells (* cols rows))
  (define deltas (diff-cell-buffers prev-buf curr-buf))
  (define n (length deltas))
  (cond
    [(or (not prev-buf) (> n (* total-cells (current-full-render-threshold))))
     (render-buffer-to-port! curr-buf out #:sync? sync?)]
    [else (render-deltas-to-port! deltas curr-buf out #:sync? sync?)]))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide (contract-out
          [render-deltas-to-port!
           (->* ((listof cell-delta?) cell-buffer? output-port?) (#:sync? boolean?) void?)]
          [render-buffer-to-port! (->* (cell-buffer? output-port?) (#:sync? boolean?) void?)]
          [render-smart!
           (->* ((or/c cell-buffer? #f) cell-buffer? output-port?) (#:sync? boolean?) void?)])
         cell->sgr
         current-full-render-threshold
         FULL-RENDER-THRESHOLD)
