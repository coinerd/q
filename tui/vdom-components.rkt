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

(define (make-transcript-vdom-component #:height [transcript-height 1000])
  (make-q-component (lambda (st width)
                      ;; Production: render transcript, clip to height, apply selection.
                      (define-values (styled-lines _st)
                        (render-transcript st transcript-height width))
                      (define visible-lines
                        (if (> (length styled-lines) transcript-height)
                            (take-right styled-lines transcript-height)
                            styled-lines))
                      ;; Apply selection highlight
                      (define sel (ui-state-selection st))
                      (define sel-anchor (selection-state-anchor sel))
                      (define sel-end (selection-state-end sel))
                      ;; Note: selection coordinates need pad-count adjustment,
                      ;; but since we clip to height, pad-count = 0 from component perspective.
                      (define trans-lines
                        (if (and sel-anchor sel-end)
                            (apply-selection-highlight visible-lines sel-anchor sel-end 0 0)
                            visible-lines))
                      ;; Pad to fill transcript-height
                      (define pad-count (- transcript-height (length trans-lines)))
                      (define pad-lines
                        (for/list ([_ (in-range pad-count)])
                          (styled-line->vnode (plain-line ""))))
                      (define content-vnodes (styled-lines->vnodes trans-lines))
                      (append pad-lines content-vnodes))
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

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide (contract-out [make-transcript-vdom-component
                        (->* () (#:height exact-nonnegative-integer?) q-component?)]
                       [styled-lines->vnodes (-> (listof styled-line?) (listof vnode?))]
                       [styled-line->vnode (-> styled-line? vnode?)]
                       [make-status-bar-vdom-component (-> q-component?)]
                       [make-input-box-vdom-component (-> q-component?)]
                       [make-input-vdom-component/istate (-> any/c q-component?)]
                       [make-header-vdom-component (-> q-component?)]
                       [make-overlay-vdom-component
                        (->* (q-component? #:anchor q-component?)
                             (#:col exact-nonnegative-integer? #:row exact-nonnegative-integer?)
                             q-component?)]))
