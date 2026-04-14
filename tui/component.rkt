#lang racket/base

;;; tui/component.rkt — Minimal component abstraction for q TUI
;;;
;;; A component is a pure render function + cache, adapted to q's
;;; immutable state model (NOT pi's mutable approach).
;;;
;;; Contract:
;;;   render-fn    : (ui-state width → (listof styled-line))
;;;   invalidate-fn: (→ void)  — called when state changes
;;;   cache-box    : (boxof (or/c #f (cons width (listof styled-line))))
;;;   id           : symbol — unique identifier for debugging

(require racket/contract
         racket/list
         "render.rkt"
         "state.rkt")

(provide
 ;; Struct
 (struct-out q-component)
 ;; Constructor
 make-q-component
 ;; Operations
 component-render
 component-invalidate!
 component-compose
 ;; Cache query
 component-cached-width)

;; ═══════════════════════════════════════════════════════════════════
;; Struct
;; ═══════════════════════════════════════════════════════════════════

(struct q-component (render-fn    ; ui-state width → (listof styled-line)
                     invalidate-fn ; → void
                     cache-box    ; (boxof (or/c #f (cons width lines)))
                     id)          ; symbol
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Constructor
;; ═══════════════════════════════════════════════════════════════════

(define (make-q-component render-fn
                           #:id [id 'anonymous]
                           #:invalidate-fn [invalidate-fn void])
  (q-component render-fn invalidate-fn (box #f) id))

;; ═══════════════════════════════════════════════════════════════════
;; Operations
;; ═══════════════════════════════════════════════════════════════════

;; Render a component with caching by width.
;; If cached and width matches, return cached lines.
;; Otherwise call render-fn, cache result, return it.
(define (component-render comp state width)
  (define cache (unbox (q-component-cache-box comp)))
  (cond
    [(and cache (= (car cache) width))
     ;; Cache hit — same width, return cached lines
     (cdr cache)]
    [else
     ;; Cache miss — compute, cache, return
     (define lines ((q-component-render-fn comp) state width))
     (set-box! (q-component-cache-box comp) (cons width lines))
     lines]))

;; Invalidate component cache.
(define (component-invalidate! comp)
  (set-box! (q-component-cache-box comp) #f)
  ((q-component-invalidate-fn comp)))

;; Render multiple components sequentially, concatenate results.
(define (component-compose components state width)
  (append* (for/list ([comp (in-list components)])
             (component-render comp state width))))

;; Query the cached width (or #f if not cached).
(define (component-cached-width comp)
  (define cache (unbox (q-component-cache-box comp)))
  (and cache (car cache)))
