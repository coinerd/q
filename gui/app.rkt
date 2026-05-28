#lang racket/base

;; q/gui/app.rkt — Top-level GUI application layout
;;
;; Defines the application struct that holds the full GUI state:
;; bridge, theme, layout, and view components. Provides the
;; render function that composes all views into a single layout.

(require racket/contract
         racket/match
         "../ui-core/observable-bridge.rkt"
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt")

(provide gui-app?
         (contract-out [make-gui-app
                        (->* (gui-state-bridge? ui-theme? gui-layout?)
                             (#:status-text (or/c string? #f)
                                            #:input-text (or/c string? #f)
                                            #:model-name (or/c string? #f))
                             gui-app?)]
                       [app-ref (-> gui-app? symbol? any/c)]
                       [app-update (-> gui-app? symbol? any/c gui-app?)]
                       [render-app (-> gui-app? (listof any/c))]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct gui-app
        (bridge ; gui-state-bridge?
         theme ; ui-theme?
         layout ; gui-layout?
         status-text ; (or/c string? #f)
         input-text ; (or/c string? #f)
         model-name ; (or/c string? #f)
         focused-view ; symbol? — 'transcript, 'input, 'sidebar
         messages) ; (listof hash?)
  #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-gui-app bridge
                      theme
                      layout
                      #:status-text [status-text "Ready"]
                      #:input-text [input-text ""]
                      #:model-name [model-name #f])
  (gui-app bridge theme layout status-text input-text model-name 'input '()))

;; ──────────────────────────────
;; Access by key
;; ──────────────────────────────
(define (app-ref app key)
  (case key
    [(bridge) (gui-app-bridge app)]
    [(theme) (gui-app-theme app)]
    [(layout) (gui-app-layout app)]
    [(status-text) (gui-app-status-text app)]
    [(input-text) (gui-app-input-text app)]
    [(model-name) (gui-app-model-name app)]
    [(focused-view) (gui-app-focused-view app)]
    [(messages) (gui-app-messages app)]
    [else #f]))

;; ──────────────────────────────
;; Functional update
;; ──────────────────────────────
(define (app-update app key value)
  (case key
    [(bridge) (struct-copy gui-app app [bridge value])]
    [(theme) (struct-copy gui-app app [theme value])]
    [(layout) (struct-copy gui-app app [layout value])]
    [(status-text) (struct-copy gui-app app [status-text value])]
    [(input-text) (struct-copy gui-app app [input-text value])]
    [(model-name) (struct-copy gui-app app [model-name value])]
    [(focused-view) (struct-copy gui-app app [focused-view value])]
    [(messages) (struct-copy gui-app app [messages value])]
    [else app]))

;; ──────────────────────────────
;; Render: compose view descriptors
;; ──────────────────────────────
;; Returns a list of view descriptors (hashes) that a concrete
;; renderer (gui-easy, web, etc.) interprets.
(define (render-app app)
  (define theme (gui-app-theme app))
  (define layout (gui-app-layout app))
  (define-values (content-w content-h) (layout-content-area layout))
  ;; Status bar at top
  (list (hash 'view
              'status-bar
              'text
              (or (gui-app-status-text app) "Ready")
              'model
              (gui-app-model-name app)
              'bg
              (theme-ref theme 'background)
              'fg
              (theme-ref theme 'foreground)
              'accent
              (theme-ref theme 'accent))
        ;; Transcript area (middle)
        (hash 'view
              'transcript
              'messages
              (gui-app-messages app)
              'width
              content-w
              'height
              (max 1 (- content-h 1))
              'bg
              (theme-ref theme 'background)
              'fg
              (theme-ref theme 'foreground)
              'focused
              (eq? (gui-app-focused-view app) 'transcript))
        ;; Input area at bottom
        (hash 'view
              'input-area
              'text
              (or (gui-app-input-text app) "")
              'width
              content-w
              'bg
              (theme-ref theme 'background)
              'fg
              (theme-ref theme 'foreground)
              'accent
              (theme-ref theme 'accent)
              'focused
              (eq? (gui-app-focused-view app) 'input))))
