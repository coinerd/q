#lang racket/base

;; q/gui/components/rich-transcript-view.rkt — Rich transcript using text% + editor-canvas%
;;
;; Replaces the canvas% + draw-text approach with a text% editor that
;; supports native text selection, auto-wrap, and rich formatting.
;; All racket/gui classes loaded via dynamic-require for headless safety.

(require racket/contract
         racket/string
         racket/class
         "../../ui-core/theme-protocol.rkt")

(provide role->label
         role->color
         hex->color-object
         (contract-out
          [make-role-label-delta (-> string? (or/c string? #f) hash?)]
          [make-content-delta (-> (or/c string? #f) hash?)]
          [render-message-descriptor (-> hash? ui-theme? hash?)]
          [messages->render-plan (-> any/c ui-theme? (listof hash?))]))

;; ──────────────────────────────
;; Pure helpers (headless-testable)
;; ──────────────────────────────

(define (role->label role)
  (case (string->symbol (or role ""))
    [(user) "You"]
    [(assistant) "Assistant"]
    [(system) "System"]
    [(tool) "Tool"]
    [else (string-titlecase (or role "Unknown"))]))

(define (role->color role theme)
  (case (string->symbol (or role ""))
    [(user) (theme-ref theme 'accent)]
    [(assistant) (theme-ref theme 'foreground)]
    [(system) (theme-ref theme 'muted)]
    [(tool) (theme-ref theme 'warning)]
    [else (theme-ref theme 'muted)]))

(define (hex->color-object hex-str)
  (define cleaned (string-trim (or hex-str "#000000") "#" #:left? #t))
  (define r (string->number (substring cleaned 0 2) 16))
  (define g (string->number (substring cleaned 2 4) 16))
  (define b (string->number (substring cleaned 4 6) 16))
  (hash 'r r 'g g 'b b 'hex (or hex-str "#000000")))

(define (make-role-label-delta label color-hex)
  (hash 'type 'role-label
        'label label
        'color color-hex
        'bold #t
        'family 'modern
        'size 12))

(define (make-content-delta color-hex)
  (hash 'type 'content
        'color color-hex
        'bold #f
        'family 'modern
        'size 12))

;; ──────────────────────────────
;; Render plan: message → styled segments
;; ──────────────────────────────

(define (render-message-descriptor msg theme)
  (define role (hash-ref msg 'role "system"))
  (define text (hash-ref msg 'text ""))
  (define label (role->label role))
  (define role-color (role->color role theme))
  (define content-color (theme-ref theme 'foreground))
  (hash 'role role
        'text text
        'segments
        (list (hash 'type 'role-label
                    'text (string-append label ": ")
                    'style (make-role-label-delta label role-color))
              (hash 'type 'content
                    'text (string-append text "\n\n")
                    'style (make-content-delta content-color)))))

(define (messages->render-plan msgs theme)
  (for/list ([m (in-list (if (list? msgs) msgs '()))])
    (render-message-descriptor m theme)))
