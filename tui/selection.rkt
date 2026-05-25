#lang racket/base

;; q/tui/selection.rkt — Selection math, mouse handling, tree overlay
;;
;; Extracted from tui-keybindings.rkt (W16) to reduce hotspot complexity.
;; Pure event handling: selection extraction, mouse dispatch, tree browser.

(require racket/match
         racket/string
         racket/list
         "context.rkt"
         "state.rkt"
         "input.rkt"
         "render.rkt"
         "layout.rkt"
         "clipboard.rkt"
         "char-width.rkt"
         "terminal.rkt"
         "tree-view.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; Selection math
;; ============================================================

;; Extract plain text from the current selection.
;; Uses rendered lines to map screen coordinates to text.
(define (selection-text ctx state)
  ;; Bounds validation (#1121): guard against out-of-range indices
  (with-handlers ([exn:fail? (lambda (e) "")])
    (define sel (ui-state-selection state))
    (define anchor (selection-state-anchor sel))
    (define end (selection-state-end sel))
    (and
     anchor
     end
     (let ()
       ;; Normalize so start <= end
       (define-values (start-col start-row end-col end-row) (normalize-selection-range anchor end))
       ;; Get rendered lines for the transcript area
       (define-values (cols rows) (tui-screen-size))
       (define layout (compute-layout rows cols))
       (define transcript-region (layout-transcript layout))
       (define trans-y (layout-region-y transcript-region))
       (define trans-height (layout-region-height transcript-region))
       (define-values (all-lines _state*) (render-transcript state trans-height cols))
       ;; BUG-57: Compute pad-count for correct coordinate mapping
       (define visible-lines
         (if (> (length all-lines) trans-height)
             (take-right all-lines trans-height)
             all-lines))
       (define pad-count (- trans-height (length visible-lines)))
       ;; Map screen rows to line indices (account for padding)
       (define start-idx (max 0 (- start-row trans-y pad-count)))
       (define end-idx (min (sub1 (length visible-lines)) (- end-row trans-y pad-count)))
       (if (> start-idx end-idx)
           ""
           (string-join
            (for/list ([i (in-range start-idx (add1 end-idx))])
              (define line (list-ref visible-lines i))
              (define text (styled-line->text line))
              (match (list i start-idx end-idx)
                [(list s s s)
                 ;; Single line: extract column range (display-col→string-offset)
                 (substring text
                            (min (display-col->string-offset text start-col) (string-length text))
                            (min (display-col->string-offset text (add1 end-col))
                                 (string-length text)))]
                ;; First line: from start-col to end
                [(list s s _)
                 (substring text
                            (min (display-col->string-offset text start-col) (string-length text)))]
                ;; Last line: from 0 to end-col
                [(list _ e e)
                 (substring text
                            0
                            (min (display-col->string-offset text (add1 end-col))
                                 (string-length text)))]
                [_ text]))
            "\n"))))))

;; ============================================================
;; Mouse handling
;; ============================================================

;; Handle a mouse event.
;; msg-data is (list type x y) or (list type button x y)
;; Returns 'continue.
(define (handle-mouse ctx msg-data)
  ;; Error guard (#1121): catch any exception from mouse handling
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "TUI: mouse handler error: ~a" (exn-message e))
                               'continue)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define mouse-type (car msg-data))
    (mark-dirty! ctx)
    (case mouse-type
      [(scroll-up)
       (set-box! (tui-ctx-ui-state-box ctx) (scroll-up state 3))
       'continue]
      [(scroll-down)
       (set-box! (tui-ctx-ui-state-box ctx) (scroll-down state 3))
       'continue]
      [(click)
       ;; Start selection: set anchor at click position
       (define button (cadr msg-data))
       (define x (caddr msg-data))
       (define y (cadddr msg-data))
       (when (= button 0) ;; left click only
         (set-box! (tui-ctx-ui-state-box ctx) (set-selection-anchor state x y)))
       'continue]
      [(drag)
       ;; Update selection end during drag
       (define x (cadr msg-data))
       (define y (caddr msg-data))
       (when (has-selection? state)
         (set-box! (tui-ctx-ui-state-box ctx) (set-selection-end state x y)))
       'continue]
      [(release)
       ;; Copy selection to clipboard (platform tool + OSC 52 fallback).
       (when (has-selection? state)
         (define text (selection-text ctx state))
         (when (and text (not (string=? text "")))
           (copy-text! text)))
       'continue]
      [else 'continue])))

;; ============================================================
;; Tree browser overlay key handling (G1.1)
;; ============================================================

(define (handle-tree-overlay-key ctx keycode)
  ;; Handle key events when tree-browser overlay is active.
  ;; Returns 'handled (consumed), 'dismiss (close overlay), or 'pass (not ours).
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define ov (ui-state-active-overlay state))
  (define tbs (and ov (overlay-state-extra ov)))
  (match keycode
    [(? (lambda (k)
          (not (and ov (eq? (overlay-state-type ov) 'tree-browser) (tree-browser-state? tbs)))))
     'pass]
    ;; Escape — dismiss overlay
    [(or 'escape #\e)
     (set-box! (tui-ctx-ui-state-box ctx) (dismiss-overlay state))
     (mark-dirty! ctx)
     'handled]
    ;; Down arrow — move selection down
    [(or 'down 'kp-down)
     (define new-idx
       (tree-next-node (tree-browser-state-rendered-lines tbs) (tree-browser-state-selected-idx tbs)))
     (update-tree-overlay! ctx state ov tbs new-idx (tree-browser-state-folded-set tbs))
     'handled]
    ;; Up arrow — move selection up
    [(or 'up 'kp-up)
     (define new-idx
       (tree-prev-node (tree-browser-state-rendered-lines tbs) (tree-browser-state-selected-idx tbs)))
     (update-tree-overlay! ctx state ov tbs new-idx (tree-browser-state-folded-set tbs))
     'handled]
    ;; Enter — toggle fold or navigate to entry
    [(or 'return 'kp-return 'enter 'kp-enter #\return)
     (define nodes (tree-browser-state-nodes tbs))
     (define idx (tree-browser-state-selected-idx tbs))
     (match idx
       [(? (lambda (i) (and (>= i 0) (< i (length nodes)))))
        (define entry (list-ref nodes idx))
        (define entry-id (list-ref entry 0))
        (define new-folded (tree-toggle-fold (tree-browser-state-folded-set tbs) entry-id))
        (update-tree-overlay! ctx state ov tbs idx new-folded)]
       [_ (void)])
     'handled]
    ;; f — fold/unfold
    [#\f
     (define nodes (tree-browser-state-nodes tbs))
     (define idx (tree-browser-state-selected-idx tbs))
     (match idx
       [(? (lambda (i) (and (>= i 0) (< i (length nodes)))))
        (define entry (list-ref nodes idx))
        (define entry-id (list-ref entry 0))
        (define new-folded (tree-toggle-fold (tree-browser-state-folded-set tbs) entry-id))
        (update-tree-overlay! ctx state ov tbs idx new-folded)]
       [_ (void)])
     'handled]
    ;; q — dismiss
    [#\q
     (set-box! (tui-ctx-ui-state-box ctx) (dismiss-overlay state))
     (mark-dirty! ctx)
     'handled]
    [_ 'pass]))

(define (update-tree-overlay! ctx state ov tbs new-idx new-folded)
  ;; Re-render tree with updated selection/fold state and update overlay
  (define nodes (tree-browser-state-nodes tbs))
  (define-values (cols rows) (tui-screen-size))
  (define active-leaf-id #f) ;; Could look up from session index
  (define rendered (render-session-tree-folded nodes active-leaf-id cols new-folded))
  ;; Build styled lines with selection highlight
  (define styled-lines
    (for/list ([line (in-list rendered)]
               [i (in-naturals)])
      (if (= i new-idx)
          (list (cons (format "► ~a" line) '()))
          (list (cons (format "  ~a" line) '())))))
  (define new-tbs (tree-browser-state nodes new-idx new-folded rendered))
  (define new-ov (struct-copy overlay-state ov [content styled-lines] [extra new-tbs]))
  (set-box! (tui-ctx-ui-state-box ctx) (struct-copy ui-state state [active-overlay new-ov]))
  (mark-dirty! ctx))

(provide selection-text
         handle-mouse
         handle-tree-overlay-key
         update-tree-overlay!)
