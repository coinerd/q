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
         (only-in "../input.rkt" input-state-buffer input-state-cursor)
         (only-in "../input/state-types.rkt" current-input-prompt-width)
         (only-in "../../ui-core/ui-diagnostics.rkt" ui-diagnostic!)
         (only-in "../char-width.rkt" string-visible-width)
         (only-in "../../ui-core/composer-layout.rkt"
                  compute-composer-layout
                  composer-layout-lines
                  composer-layout-cursor-row
                  composer-layout-cursor-col
                  composer-layout-row-count
                  composer-layout-width
                  composer-visual-line-text)
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
                  layout-region-y
                  composer-text-area-width))

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
  ;; R100 (BUG-0100): the ACTUAL cell-buffer dimensions are the authority
  ;; for every physical row limit. The layout can exceed a tiny terminal
  ;; (its height is clamped to a minimum), so painting/cursor limits that
  ;; were derived from the layout alone could address rows past the real
  ;; buffer. All row limits below now use the buffer's own axes.
  (define buf-cols (cell-buffer-cols ubuf))
  (define buf-rows (cell-buffer-rows ubuf))
  (define rows buf-rows)
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

  (define overlay (ui-state-active-overlay ui-state))
  (when overlay
    ;; R100: the overlay's row-only limit is bounded by the ACTUAL buffer
    ;; axes, not just the layout's transcript height.
    (define overlay-row-limit (min transcript-height (max 0 (- rows transcript-start-row))))
    (define ov-content (overlay-state-content overlay))
    (define ov-lines (clip-overlay-content ov-content overlay-row-limit))
    (define ov-vnodes
      (for/list ([line (in-list ov-lines)])
        (vdom-comp:styled-line->vnode line)))
    (for ([i (in-range overlay-row-limit)])
      (cell-buffer-putstring! ubuf 0 (+ transcript-start-row i) (make-string cols #\space) #:bg 8))
    (when (pair? ov-vnodes)
      (render-vdom-section-to-buffer! ov-vnodes ubuf cols transcript-start-row overlay-row-limit)))

  ;; ── Multiline composer (W3, default-on since W5) ────────────────
  ;; composer-layout is the SINGLE authoritative source for both the
  ;; painted visual lines AND the cursor cell: the cursor coordinates
  ;; returned below come from this same layout — never recomputed.
  ;; R100 repair: the legacy "q> " prompt (dropped in f68aebb1) is restored.
  ;; Like the pre-composer input-visible-window contract, the prompt
  ;; occupies current-input-prompt-width columns of the first composer text
  ;; row; the shared layout wraps — and the cursor projects — in the
  ;; REMAINING width (composer-text-area-width). All painting offsets and
  ;; the cursor offset derive from that single width decision here.
  (define input-width (max 1 (layout-region-width input-region)))
  (define prompt-width (current-input-prompt-width))
  (define composer-width (composer-text-area-width input-width prompt-width))
  (define show-prompt? (> input-width prompt-width))
  (define text-col-offset (if show-prompt? prompt-width 0))
  (define clayout
    (compute-composer-layout (input-state-buffer input-st)
                             (input-state-cursor input-st)
                             composer-width
                             string-visible-width))
  (define vlines (composer-layout-lines clayout))
  (define cursor-row (composer-layout-cursor-row clayout))
  (define cursor-col (composer-layout-cursor-col clayout))
  ;; Rows available on screen from the composer's first text row down.
  (define avail-rows (max 1 (- rows input-y)))
  ;; Internal vertical viewport: keep the cursor's visual line visible.
  (define v-offset (max 0 (- (add1 cursor-row) avail-rows)))
  ;; Paint the prompt on the first composer text row (before text). The
  ;; scroll indicator, when the viewport scrolled, takes the prompt's
  ;; trailing cell (or col 0 on surfaces too narrow for the prompt).
  (when show-prompt?
    (cell-buffer-putstring! ubuf 0 input-y "q> "))
  (for ([vline (in-list (list-tail vlines v-offset))]
        [i (in-naturals)]
        #:break (>= i avail-rows))
    (cell-buffer-putstring! ubuf
                            text-col-offset
                            (+ input-y i)
                            (composer-visual-line-text vline)))
  ;; Clear indicator when the viewport scrolled past the top.
  (when (> v-offset 0)
    (cell-buffer-putstring! ubuf
                            (if show-prompt? (sub1 prompt-width) 0)
                            input-y
                            "↑"))
  (define abs-cursor-row (+ input-y (- cursor-row v-offset)))
  ;; R100 (BUG-0100): the logical cursor — which may carry the END-OF-LINE
  ;; INSERTION SENTINEL (col == visual width, e.g. exact-width composer
  ;; input) — is projected onto a REAL visible cell of the ACTUAL buffer
  ;; via the shared projection. When no physical cell exists (zero-size
  ;; surface, inconsistent geometry), the frame completes WITHOUT a cursor
  ;; (values #f #f) plus a bounded diagnostic: never a clamp, never an
  ;; unchecked deref downstream.
  ;; The prompt offset is part of the physical geometry: the projection
  ;; runs against the text area's column window (refusals stay refusals),
  ;; and the prompt width is added back to yield the buffer-relative cell.
  (define-values (phys-cursor-col phys-cursor-row)
    (project-logical-cursor cursor-col
                            abs-cursor-row
                            (composer-layout-width clayout)
                            (max 0 (- buf-cols text-col-offset))
                            buf-rows))
  (define phys-col+prompt
    (and phys-cursor-col (+ phys-cursor-col text-col-offset)))
  (if (and phys-col+prompt phys-cursor-row)
      (values phys-col+prompt phys-cursor-row ui-state* '())
      (begin
        (ui-diagnostic!
         'renderer.cursor-deferred
         (format "~ax~a" buf-cols buf-rows)
         (format "logical cursor (~a,~a) width ~a has no physical cell; frame continues without software cursor"
                 cursor-col
                 abs-cursor-row
                 (composer-layout-width clayout)))
        (values #f #f ui-state* '()))))
