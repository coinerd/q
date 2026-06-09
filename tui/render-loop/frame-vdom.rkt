#lang racket/base

;; q/tui/render-loop/frame-vdom.rkt — VDOM-based frame rendering
;;
;; Renders a complete TUI frame to the cell buffer using the vdom pipeline.
;; Extracted from tui-render-loop.rkt (v0.96.16, AX1-1).
;; STABILITY: internal

(require racket/list
         "../cell-buffer.rkt"
         "../vdom-render.rkt"
         (prefix-in vdom-comp: "../vdom-components.rkt")
         (only-in "../component.rkt" component-render component-state-ref component-state-update)
         (only-in "../render.rkt" render-transcript apply-selection-highlight plain-line)
         (only-in "../input.rkt" input-visible-window)
         (only-in "../state.rkt"
                  ui-state-scroll-offset
                  ui-state-selection
                  selection-state-anchor
                  selection-state-end
                  ui-state-active-overlay
                  overlay-state-content
                  get-widget-lines-above)
         (only-in "../layout.rkt"
                  layout-header
                  layout-transcript
                  layout-input
                  layout-region-width
                  layout-region-height
                  layout-region-y))

(provide render-frame-vdom!
         clip-visible-lines
         compute-pad-count
         clip-overlay-content)

;; ============================================================
;; Pure render helpers
;; ============================================================

(define (clip-visible-lines lines height)
  (if (> (length lines) height)
      (take-right lines height)
      lines))

(define (compute-pad-count lines height)
  (max 0 (- height (length lines))))

(define (clip-overlay-content content height)
  (if (> (length content) height)
      (take-right content height)
      content))

;; ============================================================
;; VDOM frame rendering
;; ============================================================

(define (render-frame-vdom! ubuf ui-state input-st layout #:component-registry [comp-registry #f])
  (define header-region (layout-header layout))
  (define transcript-region (layout-transcript layout))
  (define input-region (layout-input layout))
  (define cols (layout-region-width header-region))
  (define rows (+ (layout-region-y input-region) (layout-region-height input-region)))
  (define header-row (layout-region-y header-region))
  (define transcript-start-row (layout-region-y transcript-region))
  (define transcript-height (layout-region-height transcript-region))
  (define status-y (layout-region-y input-region))
  (define input-y (min (sub1 rows) (add1 status-y)))

  (define (get-comp id make-fn)
    (if comp-registry
        (hash-ref comp-registry id (lambda () (make-fn)))
        (make-fn)))

  (cell-buffer-clear! ubuf)

  (when header-row
    (define header-comp (get-comp 'header-vdom vdom-comp:make-header-vdom-component))
    (define header-vnodes (component-render header-comp ui-state cols))
    (render-vdom-section-to-buffer! header-vnodes ubuf cols header-row 1))

  (define trans-comp (and comp-registry (hash-ref comp-registry 'transcript-vdom #f)))
  (define trans-frame-count
    (if trans-comp
        (add1 (component-state-ref trans-comp 'render-count 0))
        1))
  (when trans-comp
    (component-state-update trans-comp 'render-count trans-frame-count)
    (component-state-update trans-comp 'last-width cols)
    (component-state-update trans-comp 'last-scroll-offset (ui-state-scroll-offset ui-state)))
  (define-values (trans-lines-raw ui-state*) (render-transcript ui-state transcript-height cols))
  (define visible-lines-raw (clip-visible-lines trans-lines-raw transcript-height))
  (define pad-count (compute-pad-count trans-lines-raw transcript-height))
  (define sel (ui-state-selection ui-state))
  (define sel-anchor (selection-state-anchor sel))
  (define sel-end (selection-state-end sel))
  (define trans-lines
    (if (and sel-anchor sel-end)
        (apply-selection-highlight visible-lines-raw
                                   sel-anchor
                                   sel-end
                                   transcript-start-row
                                   pad-count)
        visible-lines-raw))
  (define trans-pad-vnodes
    (for/list ([_ (in-range pad-count)])
      (vdom-comp:styled-line->vnode (plain-line ""))))
  (define trans-content-vnodes (vdom-comp:styled-lines->vnodes trans-lines))
  (render-vdom-section-to-buffer! (append trans-pad-vnodes trans-content-vnodes)
                                  ubuf
                                  cols
                                  transcript-start-row)

  (define widget-lines (get-widget-lines-above ui-state))
  (when (> (length widget-lines) 0)
    (define widget-vnodes (vdom-comp:styled-lines->vnodes widget-lines))
    (for ([vn (in-list widget-vnodes)]
          [i (in-naturals)])
      (define widget-y (+ transcript-start-row transcript-height i))
      (when (< widget-y status-y)
        (render-vdom-to-buffer! vn ubuf cols #:start-row widget-y))))

  (define status-comp (get-comp 'status-bar-vdom vdom-comp:make-status-bar-vdom-component))
  (define status-vnodes (component-render status-comp ui-state cols))
  (render-vdom-section-to-buffer! status-vnodes ubuf cols status-y 1)

  (define input-comp (vdom-comp:make-input-vdom-component/istate input-st))
  (define input-vnodes (component-render input-comp ui-state cols))
  (render-vdom-section-to-buffer! input-vnodes ubuf cols input-y 1)

  (define overlay (ui-state-active-overlay ui-state))
  (when overlay
    (define ov-content (overlay-state-content overlay))
    (define ov-lines (clip-overlay-content ov-content transcript-height))
    (define ov-vnodes
      (for/list ([line (in-list ov-lines)]
                 [_ (in-naturals)])
        #:break (>= _ transcript-height)
        (vdom-comp:styled-line->vnode line)))
    (for ([i (in-range transcript-height)])
      (cell-buffer-putstring! ubuf 0 (+ transcript-start-row i) (make-string cols #\space) #:bg 8))
    (when (pair? ov-vnodes)
      (render-vdom-section-to-buffer! ov-vnodes ubuf cols transcript-start-row (length ov-vnodes))))

  (define-values (_visible-text _scroll-offset cursor-display-col)
    (input-visible-window input-st cols))
  (values cursor-display-col input-y ui-state* '()))
