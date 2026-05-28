#lang racket/base

;; q/gui/views/error-boundary.rkt — Error boundary views for GUI
;;
;; Provides error boundary wrappers that catch rendering errors
;; and display fallback views instead of crashing.

(require racket/contract
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-error-fallback (->* (ui-theme?) [(or/c string? #f)] hash?)]
                       [with-error-boundary (-> procedure? procedure? any)]))

;; ──────────────────────────────
;; Error fallback view
;; ──────────────────────────────
(define (render-error-fallback theme [message #f])
  (hash 'view
        'error-fallback
        'type
        'error
        'message
        (or message "An error occurred while rendering this view")
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'error)))

;; ──────────────────────────────
;; Error boundary wrapper
;; ──────────────────────────────
(define (with-error-boundary render-fn fallback-fn)
  (lambda args
    (with-handlers ([exn:fail? (lambda (e) (apply fallback-fn args))])
      (apply render-fn args))))
