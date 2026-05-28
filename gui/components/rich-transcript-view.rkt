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
         (contract-out [make-role-label-delta (-> string? (or/c string? #f) hash?)]
                       [make-content-delta (-> (or/c string? #f) hash?)]))

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
  ;; Returns a hash representation for headless testing.
  ;; In GUI mode, this would create a color% object.
  (define cleaned (string-trim hex-str "#" #:left? #t))
  (define r (string->number (substring cleaned 0 2) 16))
  (define g (string->number (substring cleaned 2 4) 16))
  (define b (string->number (substring cleaned 4 6) 16))
  (hash 'r r 'g g 'b b 'hex hex-str))

(define (make-role-label-delta label color-hex)
  (hash 'type 'role-label 'label label 'color color-hex 'bold #t))

(define (make-content-delta color-hex)
  (hash 'type 'content 'color color-hex 'bold #f))
