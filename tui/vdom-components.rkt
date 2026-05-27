#lang racket/base

;; q/tui/vdom-components.rkt — Pre-built vdom components for TUI zones
;;
;; Typed components that return vnode trees instead of styled-lines.
;; Each component encapsulates the rendering logic for a specific TUI zone.
;; These can be used alongside or as replacements for the styled-line
;; components in renderer.rkt.

(require racket/contract
         racket/string
         racket/list
         "vdom.rkt"
         "vdom-layout.rkt"
         "component.rkt"
         "state.rkt"
         "render/message-layout.rkt"
         "render/diff-render.rkt"
         "render/status-line.rkt"
         (only-in "input.rkt" initial-input-state)
         "palette.rkt"
         "layout.rkt")

;; ============================================================
;; Transcript component — renders conversation messages
;; ============================================================

(define (make-transcript-vdom-component)
  (make-q-component (lambda (st width)
                      ;; Production: use render-transcript to get styled-lines, convert to vnodes.
                      ;; transcript-height is a large value; the caller clips to visible area.
                      (define-values (styled-lines _st) (render-transcript st 1000 width))
                      (define vnodes (styled-lines->vnodes styled-lines))
                      vnodes)
                    #:id 'transcript-vdom
                    #:vdom? #t))

;; Convert styled-lines to a list of vnode rows (one vhbox per line).
(define (styled-lines->vnodes lines)
  (for/list ([line (in-list lines)])
    (styled-line->vnode line)))

;; Convert a single styled-line to a vhbox of vtext segments.
(define (styled-line->vnode line)
  (define segs (styled-line-segments line))
  (cond
    [(null? segs) (vhbox (list (vtext "" '())))]
    [(= (length segs) 1)
     (define seg (car segs))
     (vhbox (list (vtext (styled-segment-text seg) (styled-segment-style seg))))]
    [else
     (vhbox (for/list ([seg (in-list segs)])
              (vtext (styled-segment-text seg) (styled-segment-style seg))))]))

;; ============================================================
;; Status bar component — renders model info, status line
;; ============================================================

(define (make-status-bar-vdom-component)
  (make-q-component (lambda (st width)
                      ;; Production: use render-status-bar from status-line.rkt
                      (define styled-line-result (render-status-bar st width))
                      (list (styled-line->vnode styled-line-result)))
                    #:id 'status-bar-vdom
                    #:vdom? #t))

;; ============================================================
;; Input box component — renders the input area
;; ============================================================

(define (make-input-box-vdom-component)
  (make-q-component (lambda (st width)
                      (list (styled-line->vnode (render-input-line (initial-input-state) width))))
                    #:id 'input-box-vdom
                    #:vdom? #t))

;; Production input component factory — captures input-state from caller.
(define (make-input-vdom-component/istate input-st)
  (make-q-component (lambda (st width) (list (styled-line->vnode (render-input-line input-st width))))
                    #:id 'input-box-vdom
                    #:vdom? #t))

;; ============================================================
;; Header component — renders the top header bar
;; ============================================================

(define (make-header-vdom-component)
  (make-q-component (lambda (st width)
                      ;; Production: matches renderer.rkt header row — " q " padded to full width
                      ;; with inverse video (fg=0 bg=7)
                      (define header-text (format " q ~a" (make-string (max 0 (- width 3)) #\space)))
                      (list (vhbox (list (vtext header-text '(inverse))))))
                    #:id 'header-vdom
                    #:vdom? #t))

;; ============================================================
;; Overlay component — renders a popup/dialog overlay
;; ============================================================

(define (make-overlay-vdom-component content-comp #:anchor anchor-comp #:col [col 0] #:row [row 0])
  (make-q-component (lambda (st width)
                      ;; Production: renders overlay from ui-state if active.
                      ;; content-comp renders the overlay content,
                      ;; anchor-comp renders the background (typically transcript).
                      (define content-vnodes ((q-component-render-fn content-comp) st width))
                      (define anchor-vnodes ((q-component-render-fn anchor-comp) st width))
                      (define content-node
                        (if (= (length content-vnodes) 1)
                            (car content-vnodes)
                            (vvbox content-vnodes)))
                      (define anchor-node
                        (if (= (length anchor-vnodes) 1)
                            (car anchor-vnodes)
                            (vvbox anchor-vnodes)))
                      (list (voverlay content-node anchor-node col row)))
                    #:id 'overlay-vdom
                    #:vdom? #t))

;; ============================================================
;; Compose all vdom components into a frame with layout positioning
;; ============================================================

;; Render a complete frame by composing section components with layout.
;; Each component renders to vnodes, positioned at the correct row offset.
;; Returns a list of (cons row-offset vnodes) pairs for the caller to
;; write to the cell buffer via render-vdom-section-to-buffer!.
(define (render-frame-components header-comp
                                 transcript-comp
                                 status-comp
                                 input-comp
                                 ui-state
                                 input-state
                                 layout)
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

  ;; Render each section component to vnodes
  (define header-vnodes
    (if header-comp
        ((q-component-render-fn header-comp) ui-state cols)
        '()))
  (define transcript-vnodes
    (if transcript-comp
        ((q-component-render-fn transcript-comp) ui-state cols)
        '()))
  (define status-vnodes
    (if status-comp
        ((q-component-render-fn status-comp) ui-state cols)
        '()))
  ;; Input component receives input-state via ui-state temporary slot
  (define input-vnodes
    (if input-comp
        ((q-component-render-fn input-comp) ui-state cols)
        '()))

  ;; Return sections as positioned groups: (vnodes start-row [max-height])
  (list (list header-vnodes header-row #f)
        (list transcript-vnodes transcript-start-row transcript-height)
        (list status-vnodes status-y #f)
        (list input-vnodes input-y #f)))

;; Backward-compatible frame component (no layout positioning — naive stacking)
(define (make-vdom-frame-component header-comp transcript-comp status-comp input-comp)
  (make-q-component (lambda (st width)
                      (define header-lines
                        (if header-comp
                            ((q-component-render-fn header-comp) st width)
                            '()))
                      (define transcript-lines
                        (if transcript-comp
                            ((q-component-render-fn transcript-comp) st width)
                            '()))
                      (define status-lines
                        (if status-comp
                            ((q-component-render-fn status-comp) st width)
                            '()))
                      (define input-lines
                        (if input-comp
                            ((q-component-render-fn input-comp) st width)
                            '()))
                      ;; Combine all sections into a single vvbox
                      (list (vvbox (append header-lines transcript-lines status-lines input-lines))))
                    #:id 'frame-vdom
                    #:vdom? #t))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide (contract-out [make-transcript-vdom-component (-> q-component?)]
                       [styled-lines->vnodes (-> (listof styled-line?) (listof vnode?))]
                       [styled-line->vnode (-> styled-line? vnode?)]
                       [make-status-bar-vdom-component (-> q-component?)]
                       [make-input-box-vdom-component (-> q-component?)]
                       [make-input-vdom-component/istate (-> any/c q-component?)]
                       [make-header-vdom-component (-> q-component?)]
                       [make-overlay-vdom-component
                        (->* (q-component? #:anchor q-component?)
                             (#:col exact-nonnegative-integer? #:row exact-nonnegative-integer?)
                             q-component?)]
                       [make-vdom-frame-component
                        (-> (or/c q-component? #f)
                            (or/c q-component? #f)
                            (or/c q-component? #f)
                            (or/c q-component? #f)
                            q-component?)]
                       [render-frame-components
                        (-> (or/c q-component? #f)
                            (or/c q-component? #f)
                            (or/c q-component? #f)
                            (or/c q-component? #f)
                            any/c
                            any/c
                            hash?
                            (listof list?))]))
