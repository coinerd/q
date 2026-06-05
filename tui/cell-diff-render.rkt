#lang racket/base

;; q/tui/cell-diff-render.rkt — Converts cell deltas to minimal ANSI output
;;
;; Takes a list of cell-delta structs and a cell buffer, produces
;; minimal ANSI escape sequences with cursor positioning + SGR changes.
;; Groups consecutive changes into single CSI sequences where possible.
;; Uses DEC 2026 (Synchronized Output) when available.

(require racket/contract
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

;; Render cell deltas to an output port as minimal ANSI sequences.
;; Deltas should be sorted by position (row-major order) for optimal grouping.
(define (render-deltas-to-port! deltas buf out #:sync? [sync? #t])
  (call-with-terminal-render-guards
   out
   sync?
   (lambda ()
     (define prev-sgr #f)
     ;; Filter out continuation cells — they are rendered as part of their base char
     (define real-deltas
       (filter (lambda (d) (not (continuation-cell? (cell-delta-new-cell d)))) deltas))
     ;; Batch consecutive deltas in same row with same SGR into one cursor move + string
     (let loop ([remaining real-deltas])
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
          (unless (equal? sgr prev-sgr)
            (display sgr out)
            (set! prev-sgr sgr))
          ;; Collect consecutive cells in same row with same SGR
          (define chars (list (cell-char new-cell)))
          (let gather ([rest (cdr remaining)]
                       [acc chars]
                       [next-col (add1 col)])
            (cond
              [(null? rest)
               ;; Emit batch — last batch in entire delta list
               (display (list->string (reverse acc)) out)
               ;; Clear to end of line to erase any old trailing characters
               (display "\x1b[K" out)
               (loop rest)]
              [else
               (define nd (car rest))
               (define nd-col (cell-delta-col nd))
               (define nd-row (cell-delta-row nd))
               (define nd-cell (cell-delta-new-cell nd))
               (define nd-sgr (cell->sgr nd-cell))
               (cond
                 [(and (= nd-row row) (= nd-col next-col) (equal? nd-sgr sgr))
                  ;; Same row, consecutive column, same style — batch continues
                  (gather (cdr rest) (cons (cell-char nd-cell) acc) (add1 next-col))]
                 [else
                  ;; Batch ends — emit accumulated chars
                  (display (list->string (reverse acc)) out)
                  ;; Clear to end of line to erase old trailing characters
                  ;; This applies when:
                  ;;  - switching to a different row
                  ;;  - gap in same row (non-consecutive column)
                  ;;  - same row, consecutive column, but different SGR
                  (display "\x1b[K" out)
                  (loop rest)])]))]))
     ;; Reset SGR at end
     (display "\x1b[0m" out))))

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
       ;; Clear to end of line — ensures old trailing characters are erased
       (display "\x1b[K" out))
     (display "\x1b[0m" out))))

;; ============================================================
;; Smart render: auto-select full vs incremental
;; ============================================================

;; If deltas represent > 50% of all cells, do a full render instead.
(define FULL-RENDER-THRESHOLD 0.5)

(define (render-smart! prev-buf curr-buf out #:sync? [sync? #t])
  (define cols (cell-buffer-cols curr-buf))
  (define rows (cell-buffer-rows curr-buf))
  (define total-cells (* cols rows))
  (define deltas (diff-cell-buffers prev-buf curr-buf))
  (define n (length deltas))
  (cond
    [(or (not prev-buf) (> n (* total-cells FULL-RENDER-THRESHOLD)))
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
         FULL-RENDER-THRESHOLD)
