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
         "state.rkt"
         ;; LF-07 (v0.98.9 W0): Removed dead imports — render.rkt, vdom.rkt, render/message-layout.rkt
         ;; were imported but no identifiers from them were used in this module body.
         (only-in "../ui-core/render-hooks.rkt"
                  apply-render-hook-safe
                  render-hook?
                  render-hook-phase))

;; Struct
(provide q-component
         q-component?
         q-component-render-fn
         q-component-invalidate-fn
         q-component-cache-box
         q-component-id
         q-component-handle-input-fn
         q-component-wants-focus?
         q-component-vdom?
         q-component-state-box
         (contract-out
          [make-q-component
           (->* (procedure?)
                (#:id symbol?
                      #:invalidate-fn procedure?
                      #:handle-input (or/c procedure? #f)
                      #:wants-focus? boolean?
                      #:vdom? boolean?)
                q-component?)]
          [component-render (-> q-component? ui-state? exact-nonnegative-integer? (listof any/c))]
          [component-invalidate! (-> q-component? void?)]
          [component-compose
           (-> (listof q-component?) ui-state? exact-nonnegative-integer? (listof any/c))]
          [component-cached-width (-> q-component? (or/c exact-nonnegative-integer? #f))]
          [component-handle-input (-> q-component? any/c ui-state? (values ui-state? any/c))]
          [input-consumed (-> any/c)]
          [input-bubble (-> any/c)]
          [input-action (-> any/c any/c)]
          [input-consumed? (-> any/c boolean?)]
          [input-bubble? (-> any/c boolean?)]
          [input-action? (-> any/c boolean?)]
          [input-action-data (-> any/c any/c)]
          [focusable-components (-> (listof q-component?) (listof q-component?))]
          [cycle-focus
           (->* ((listof q-component?) (or/c symbol? #f)) (exact-integer?) (or/c symbol? #f))]
          [component-state-ref (->* (q-component? any/c) (any/c) any/c)]
          [component-state-update (-> q-component? any/c any/c void?)]))

;; ═══════════════════════════════════════════════════════════════════
;; Struct
;; ═══════════════════════════════════════════════════════════════════

(struct q-component
        (render-fn ; ui-state width → (listof styled-line) or (listof vnode?)
         invalidate-fn ; → void
         cache-box ; (boxof (or/c #f (cons width lines)))
         id ; symbol
         handle-input-fn ; (or/c #f (data ui-state → (values ui-state input-result)))
         wants-focus? ; boolean — whether this component can receive focus
         vdom? ; boolean — #t when render-fn returns vnodes
         state-box) ; (boxof hash?) — per-component local state
  #:transparent)

;; GAP-RH (v0.98.7 W2): Parameter for registered render hooks.
;; Default empty list — no hooks applied unless registered.
(define current-render-hooks (make-parameter '()))

;; MF-07 (v0.98.9 W2): Public API for extensions to register/unregister render hooks.
;; Without these helpers, extensions would need to know the internal parameter protocol.
(define (register-render-hook! hook)
  (current-render-hooks (cons hook (current-render-hooks))))

(define (unregister-render-hook! hook)
  (current-render-hooks (filter (lambda (h) (not (eq? h hook))) (current-render-hooks))))

(provide current-render-hooks
         (contract-out [register-render-hook! (-> render-hook? void?)]
                       [unregister-render-hook! (-> render-hook? void?)]))

;; ═══════════════════════════════════════════════════════════════════
;; Constructor
;; ═══════════════════════════════════════════════════════════════════

(define (make-q-component render-fn
                          #:id [id 'anonymous]
                          #:invalidate-fn [invalidate-fn void]
                          #:handle-input [handle-input-fn #f]
                          #:wants-focus? [wants-focus? #f]
                          #:vdom? [vdom? #f])
  (q-component render-fn invalidate-fn (box #f) id handle-input-fn wants-focus? vdom? (box (hash))))

;; ═══════════════════════════════════════════════════════════════════
;; Operations
;; ═══════════════════════════════════════════════════════════════════

;; Render a component with caching by width.
;; If cached and width matches, return cached lines.
;; Otherwise call render-fn, apply post-render hooks, cache result, return it.
;; GAP-RH (v0.98.7 W2): Apply post-render hooks after normal rendering.
;; apply-render-hook-safe catches all exceptions, returns input unchanged on error.
(define (component-render comp state width)
  (define cache (unbox (q-component-cache-box comp)))
  (cond
    ;; Cache hit — same width, return cached lines
    [(and cache (= (car cache) width)) (cdr cache)]
    [else
     ;; Cache miss — compute, apply hooks, cache, return
     (define lines ((q-component-render-fn comp) state width))
     ;; LF-02 (v0.98.9 W2): Apply pre-render hooks (symmetric with post-render).
     ;; Pre-render hooks transform the output before post-render hooks see it.
     (define pre-hooks
       (filter (lambda (h) (eq? (render-hook-phase h) 'pre-render)) (current-render-hooks)))
     (define pre-lines
       (if (null? pre-hooks)
           lines
           (for/fold ([acc lines]) ([hook (in-list pre-hooks)])
             (define-values (result ok?) (apply-render-hook-safe hook acc))
             (if ok? result acc))))
     ;; Apply post-render hooks if any are registered
     (define post-hooks
       (filter (lambda (h) (eq? (render-hook-phase h) 'post-render)) (current-render-hooks)))
     (define final-lines
       (if (null? post-hooks)
           pre-lines
           (for/fold ([acc pre-lines]) ([hook (in-list post-hooks)])
             (define-values (result ok?) (apply-render-hook-safe hook acc))
             (if ok? result acc))))
     (set-box! (q-component-cache-box comp) (cons width final-lines))
     final-lines]))

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
;; Component-local state bag
;; ═══════════════════════════════════════════════════════════════════

;; Retrieve a value from the component's local state.
(define (component-state-ref comp key [default #f])
  (hash-ref (unbox (q-component-state-box comp)) key default))

;; Store a value in the component's local state.
(define (component-state-update comp key value)
  (set-box! (q-component-state-box comp) (hash-set (unbox (q-component-state-box comp)) key value)))

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
