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

;; Struct
(provide (struct-out q-component)
         ;; Constructor
         make-q-component
         ;; Operations
         component-render
         component-invalidate!
         component-compose
         ;; Cache query
         component-cached-width
         ;; Input handling
         component-handle-input
         input-consumed
         input-bubble
         input-action
         input-consumed?
         input-bubble?
         input-action?
         input-action-data
         ;; Focus
         focusable-components
         cycle-focus)

;; ═══════════════════════════════════════════════════════════════════
;; Struct
;; ═══════════════════════════════════════════════════════════════════

(struct q-component
        (render-fn ; ui-state width → (listof styled-line)
         invalidate-fn ; → void
         cache-box ; (boxof (or/c #f (cons width lines)))
         id ; symbol
         handle-input-fn ; (or/c #f (data ui-state → (values ui-state input-result)))
         wants-focus?) ; boolean — whether this component can receive focus
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Constructor
;; ═══════════════════════════════════════════════════════════════════

(define (make-q-component render-fn
                          #:id [id 'anonymous]
                          #:invalidate-fn [invalidate-fn void]
                          #:handle-input [handle-input-fn #f]
                          #:wants-focus? [wants-focus? #f])
  (q-component render-fn invalidate-fn (box #f) id handle-input-fn wants-focus?))

;; ═══════════════════════════════════════════════════════════════════
;; Operations
;; ═══════════════════════════════════════════════════════════════════

;; Render a component with caching by width.
;; If cached and width matches, return cached lines.
;; Otherwise call render-fn, cache result, return it.
(define (component-render comp state width)
  (define cache (unbox (q-component-cache-box comp)))
  (cond
    ;; Cache hit — same width, return cached lines
    [(and cache (= (car cache) width)) (cdr cache)]
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

;; ═══════════════════════════════════════════════════════════════════
;; Input result constructors
;; ═══════════════════════════════════════════════════════════════════

(define (input-consumed)
  '(consumed))
(define (input-bubble)
  '(bubble))
(define (input-action action-data)
  (list 'action action-data))

(define (input-consumed? r)
  (equal? r '(consumed)))
(define (input-bubble? r)
  (and (pair? r) (eq? (car r) 'bubble)))
(define (input-action? r)
  (and (pair? r) (eq? (car r) 'action)))
(define (input-action-data r)
  (cadr r))

;; ═══════════════════════════════════════════════════════════════════
;; Input dispatch
;; ═══════════════════════════════════════════════════════════════════

;; Dispatch input to a component. If component has no handle-input-fn,
;; return (values state 'bubble).
(define (component-handle-input comp data state)
  (define fn (q-component-handle-input-fn comp))
  (if fn
      (fn data state)
      (values state (input-bubble))))

;; ═══════════════════════════════════════════════════════════════════
;; Focus management
;; ═══════════════════════════════════════════════════════════════════

;; Find focusable components in a list
(define (focusable-components components)
  (filter (lambda (c) (q-component-wants-focus? c)) components))

;; Cycle focus to next focusable component
(define (cycle-focus components current-id [direction 1])
  (define focusable (focusable-components components))
  (if (null? focusable)
      #f
      (let* ([idx (for/first ([c focusable]
                              [i (in-naturals)]
                              #:when (eq? (q-component-id c) current-id))
                    i)]
             [current (or idx -1)]
             [next (modulo (+ current direction) (length focusable))])
        (q-component-id (list-ref focusable next)))))
