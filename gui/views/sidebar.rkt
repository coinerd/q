#lang racket/base

;; q/gui/views/sidebar.rkt — Sidebar view for GUI
;;
;; Collapsible sidebar with sections for navigation, session list,
;; and extension widgets. Produces backend-agnostic view descriptors.

(require racket/contract
         racket/list
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-sidebar
                        (->* (ui-theme?)
                             (#:width exact-nonnegative-integer?
                                      #:collapsed boolean?
                                      #:sections (listof hash?)
                                      #:active-section (or/c symbol? #f)
                                      #:focused boolean?)
                             hash?)]
                       [sidebar-section (-> symbol? string? (listof hash?) hash?)]
                       [sidebar-item (->* [symbol? string?] [(or/c symbol? #f)] hash?)]))

;; ──────────────────────────────
;; Constructors
;; ──────────────────────────────
(define (sidebar-item id label [icon #f])
  (hash 'id id 'label label 'icon icon))

(define (sidebar-section id label items)
  (hash 'id id 'label label 'items items))

;; ──────────────────────────────
;; Render sidebar
;; ──────────────────────────────
(define (render-sidebar theme
                        #:width [width 30]
                        #:collapsed [collapsed #f]
                        #:sections [sections '()]
                        #:active-section [active-section #f]
                        #:focused [focused #f])
  (define effective-width (if collapsed 2 width))
  (hash 'view
        'sidebar
        'width
        effective-width
        'collapsed
        collapsed
        'sections
        sections
        'active-section
        active-section
        'focused
        focused
        'section-count
        (length sections)
        'item-count
        (apply + (map (lambda (s) (length (hash-ref s 'items '()))) sections))
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)
        'accent
        (theme-ref theme 'accent)))
