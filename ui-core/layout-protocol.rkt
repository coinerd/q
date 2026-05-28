#lang racket/base

;; q/ui-core/layout-protocol.rkt — Shared layout definitions for TUI + GUI
;;
;; Defines layout geometry structs that both TUI and GUI backends can use.
;; Abstracts away the specific rendering details while sharing layout constraints.

(require racket/contract)

(provide gui-layout?
         (contract-out [make-gui-layout
                        (->* ()
                             (#:width (or/c exact-positive-integer? #f)
                                      #:height (or/c exact-positive-integer? #f)
                                      #:sidebar-width (or/c exact-nonnegative-integer? #f)
                                      #:status-height (or/c exact-nonnegative-integer? #f)
                                      #:input-height (or/c exact-nonnegative-integer? #f)
                                      #:min-width (or/c exact-positive-integer? #f)
                                      #:min-height (or/c exact-positive-integer? #f))
                             gui-layout?)]
                       [default-gui-layout (-> gui-layout?)]
                       [layout-content-area
                        (-> gui-layout?
                            (values exact-nonnegative-integer? exact-nonnegative-integer?))])
         gui-layout
         gui-layout-width
         gui-layout-height
         gui-layout-sidebar-width
         gui-layout-status-height
         gui-layout-input-height
         gui-layout-min-width
         gui-layout-min-height)

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct gui-layout (width height sidebar-width status-height input-height min-width min-height)
  #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-gui-layout #:width [width #f]
                         #:height [height #f]
                         #:sidebar-width [sidebar-width 0]
                         #:status-height [status-height 1]
                         #:input-height [input-height 3]
                         #:min-width [min-width 80]
                         #:min-height [min-height 24])
  (gui-layout width height sidebar-width status-height input-height min-width min-height))

;; ──────────────────────────────
;; Default layout
;; ──────────────────────────────
(define (default-gui-layout)
  (gui-layout 120 40 0 1 3 80 24))

;; ──────────────────────────────
;; Content area (total minus status bar and input)
;; ──────────────────────────────
(define (layout-content-area layout)
  (define w (or (gui-layout-width layout) 120))
  (define h (or (gui-layout-height layout) 40))
  (define content-w (- w (gui-layout-sidebar-width layout)))
  (define content-h (- h (gui-layout-status-height layout) (gui-layout-input-height layout)))
  (values (max 0 content-w) (max 0 content-h)))
