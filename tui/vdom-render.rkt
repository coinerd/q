#lang racket/base

;; q/tui/vdom-render.rkt — VDOM render engine
;;
;; Converts styled-lines (from vdom-layout) to cell-buffer writes.
;; This replaces the direct draw-styled-line! calls in renderer.rkt
;; when using the vdom pipeline.
;;
;; Pipeline: vnode → vdom-layout → styled-lines → vdom-render → cell-buffer

(require racket/contract
         racket/list
         racket/string
         "vdom.rkt"
         "vdom-layout.rkt"
         "cell-buffer.rkt"
         "render/message-layout.rkt"
         "renderer.rkt"
         "char-width.rkt")

;; ============================================================
;; Render styled-lines to cell buffer
;; ============================================================

;; Render a list of styled-lines to the cell buffer starting at row 0.
;; Each line is one row. Lines are padded to width with spaces.
(define (render-styled-lines-to-buffer! lines buf width #:start-row [start-row 0])
  (for ([line (in-list lines)]
        [row-offset (in-naturals)])
    (render-styled-line-to-buffer! line buf width (+ start-row row-offset))))

;; Render a single styled-line to the cell buffer at the given row.
(define (render-styled-line-to-buffer! line buf width row)
  (define cols (cell-buffer-cols buf))
  (define effective-width (min width cols))
  (define col 0)
  (define remaining-width effective-width)
  (for ([seg (in-list (styled-line-segments line))])
    #:break (<= remaining-width 0)
    (define text (string-replace (styled-segment-text seg) "\n" " "))
    (define styles (styled-segment-style seg))
    ;; Truncate text to remaining width
    (define visible-text
      (if (> (string-visible-width text) remaining-width)
          (truncate-to-visible-width text remaining-width)
          text))
    (when (and (> (string-length visible-text) 0) (< col cols))
      ;; Apply styles to cell-buffer via putstring with style keywords
      (define-values (kws vals) (style->ubuf-kws styles))
      (keyword-apply cell-buffer-width-aware-putstring! kws vals (list buf col row visible-text)))
    (set! col (+ col (string-visible-width visible-text)))
    (set! remaining-width (- remaining-width (string-visible-width visible-text))))
  ;; Pad remaining width with spaces (clear rest of line)
  (when (> remaining-width 0)
    (cell-buffer-putstring! buf col row (make-string remaining-width #\space))))

;; Truncate text to fit within given visible width
(define (truncate-to-visible-width text max-width)
  (define len (string-length text))
  (let loop ([i 0]
             [width 0])
    (cond
      [(>= i len) text]
      [(>= width max-width) (substring text 0 i)]
      [else
       (define ch (string-ref text i))
       (define cw (char-width ch))
       (if (> (+ width cw) max-width)
           (substring text 0 i)
           (loop (add1 i) (+ width cw)))])))

;; ============================================================
;; Section rendering: component vnodes → cell-buffer at position
;; ============================================================

;; Render a list of vnodes to the cell buffer at a specific start-row,
;; clipping to max-rows lines. Pads with blank lines if fewer than max-rows.
;; This is the key bridge between vdom components and the cell buffer.
(define (render-vdom-section-to-buffer! vnodes buf width start-row [max-rows #f])
  (define styled-lines (apply append (map (lambda (v) (vdom-layout v width)) vnodes)))
  (define clipped
    (if max-rows
        (if (> (length styled-lines) max-rows)
            (take-right styled-lines max-rows)
            styled-lines)
        styled-lines))
  (define pad-count
    (if max-rows
        (- max-rows (length clipped))
        0))
  ;; Render padding
  (for ([i (in-range pad-count)])
    (render-styled-line-to-buffer! (plain-line "") buf width (+ start-row i)))
  ;; Render content lines
  (for ([line (in-list clipped)]
        [i (in-naturals)])
    (render-styled-line-to-buffer! line buf width (+ start-row pad-count i)))
  (void))

;; ============================================================
;; Full pipeline: vnode → cell-buffer
;; ============================================================

;; Render a vnode tree directly to a cell buffer.
(define (render-vdom-to-buffer! v buf width #:start-row [start-row 0])
  (define lines (vdom-layout v width))
  (render-styled-lines-to-buffer! lines buf width #:start-row start-row))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide (contract-out
          [render-styled-lines-to-buffer!
           (->* ((listof styled-line?) cell-buffer? exact-nonnegative-integer?)
                (#:start-row exact-nonnegative-integer?)
                void?)]
          [render-styled-line-to-buffer!
           (-> styled-line? cell-buffer? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
          [render-vdom-to-buffer!
           (->* (vnode? cell-buffer? exact-nonnegative-integer?)
                (#:start-row exact-nonnegative-integer?)
                void?)]
          [render-vdom-section-to-buffer!
           (->* ((listof vnode?) cell-buffer? exact-nonnegative-integer? exact-nonnegative-integer?)
                (exact-nonnegative-integer?)
                void?)])
         truncate-to-visible-width)
