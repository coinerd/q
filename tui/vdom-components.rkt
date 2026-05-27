#lang racket/base

;; q/tui/vdom-components.rkt — Pre-built vdom components for TUI zones
;;
;; Typed components that return vnode trees instead of styled-lines.
;; Each component encapsulates the rendering logic for a specific TUI zone.
;; These can be used alongside or as replacements for the styled-line
;; components in renderer.rkt.

(require racket/contract
         racket/string
         "vdom.rkt"
         "vdom-layout.rkt"
         "component.rkt"
         "state.rkt"
         "render/message-layout.rkt"
         "palette.rkt")

;; ============================================================
;; Transcript component — renders conversation messages
;; ============================================================

(define (make-transcript-vdom-component)
  (make-q-component (lambda (st width)
                      ;; For now, return a placeholder vvbox.
                      ;; Full implementation will iterate messages and build vnodes.
                      (list (vtext "Transcript" '(dim))))
                    #:id 'transcript-vdom
                    #:vdom? #t))

;; ============================================================
;; Status bar component — renders model info, status line
;; ============================================================

(define (make-status-bar-vdom-component)
  (make-q-component
   (lambda (st width)
     (define model-name (or (ui-state-model-name st) "no-model"))
     (define mode (ui-state-mode st))
     (define status-text (format " ~a | ~a " model-name mode))
     (define fill-w (max 0 (- width (string-length status-text) 1)))
     (list (vhbox (list (vtext status-text '(bold)) (vfill* fill-w) (vtext " " '())))))
   #:id 'status-bar-vdom
   #:vdom? #t))

;; ============================================================
;; Input box component — renders the input area
;; ============================================================

(define (make-input-box-vdom-component)
  (make-q-component (lambda (st width)
                      ;; Input area is managed separately (keystroke-driven, not in ui-state).
                      ;; This provides an empty vdom placeholder for the input zone.
                      (list (vtext "> " '(green))))
                    #:id 'input-box-vdom
                    #:vdom? #t))

;; ============================================================
;; Header component — renders the top header bar
;; ============================================================

(define (make-header-vdom-component)
  (make-q-component
   (lambda (st width)
     (define version-text " q ")
     (define right-text (format " ~a " (or (ui-state-model-name st) "")))
     (define fill-w (max 0 (- width (string-length version-text) (string-length right-text))))
     (list (vhbox
            (list (vtext version-text '(bold cyan)) (vfill* fill-w) (vtext right-text '(dim))))))
   #:id 'header-vdom
   #:vdom? #t))

;; ============================================================
;; Overlay component — renders a popup/dialog overlay
;; ============================================================

(define (make-overlay-vdom-component content-comp #:anchor anchor-comp #:col [col 0] #:row [row 0])
  (make-q-component (lambda (st width)
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
;; Compose all vdom components into a frame
;; ============================================================

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
                       [make-status-bar-vdom-component (-> q-component?)]
                       [make-input-box-vdom-component (-> q-component?)]
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
                            q-component?)]))
