#lang racket/base

;; extensions/widget-lifecycle.rkt — Lifecycle widget protocol (#5253, #5254)
;;
;; Extends the q-component model with explicit mount/unmount callbacks
;; and a thread-safe registry for lifecycle widgets.
;;
;; A lifecycle widget provides four phases:
;;   mount   — called when widget is first displayed (init resources)
;;   render  — called each render cycle: (ui-state, width) → (listof styled-line)
;;   input   — called when widget has focus and receives key: (key, ui-state) → (values ui-state result)
;;   unmount — called when widget is removed (cleanup)

(require racket/contract
         racket/match
         racket/list
         "../tui/component.rkt"
         "../tui/state.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; Struct
;; ═══════════════════════════════════════════════════════════════════

(struct widget-lifecycle
        (id ; symbol — unique widget identifier
         mount-fn ; (→ void) — called on first display
         render-fn ; (ui-state width → (listof styled-line))
         input-fn ; (or/c #f (key ui-state → (values ui-state input-result)))
         unmount-fn ; (→ void) — called on removal
         mounted-box ; (boxof boolean) — tracks mount state
         )
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Constructor
;; ═══════════════════════════════════════════════════════════════════

(define (make-widget-lifecycle id
                               render-fn
                               #:mount [mount-fn void]
                               #:input [input-fn #f]
                               #:unmount [unmount-fn void])
  (widget-lifecycle id mount-fn render-fn input-fn unmount-fn (box #f)))

;; ═══════════════════════════════════════════════════════════════════
;; Lifecycle operations
;; ═══════════════════════════════════════════════════════════════════

;; Mount a widget — calls mount callback if not already mounted.
(define (widget-mount! w)
  (unless (unbox (widget-lifecycle-mounted-box w))
    ((widget-lifecycle-mount-fn w))
    (set-box! (widget-lifecycle-mounted-box w) #t)))

;; Render a widget — ensures mounted first, then delegates to render-fn.
(define (widget-render w state width)
  (widget-mount! w)
  ((widget-lifecycle-render-fn w) state width))

;; Handle input for a widget.
(define (widget-handle-input w key state)
  (define fn (widget-lifecycle-input-fn w))
  (if fn
      (fn key state)
      (values state (input-bubble))))

;; Unmount a widget — calls unmount callback if mounted.
(define (widget-unmount! w)
  (when (unbox (widget-lifecycle-mounted-box w))
    ((widget-lifecycle-unmount-fn w))
    (set-box! (widget-lifecycle-mounted-box w) #f)))

;; Check if a widget is mounted.
(define (widget-mounted? w)
  (unbox (widget-lifecycle-mounted-box w)))

;; Convert a lifecycle widget to a q-component for integration
;; with the existing component system.
(define (widget->component w)
  (make-q-component (lambda (state width) (widget-render w state width))
                    #:id (widget-lifecycle-id w)
                    #:handle-input (lambda (key state) (widget-handle-input w key state))
                    #:wants-focus? (and (widget-lifecycle-input-fn w) #t)))

;; ═══════════════════════════════════════════════════════════════════
;; Thread-safe lifecycle widget registry
;; ═══════════════════════════════════════════════════════════════════

(define lifecycle-widget-registry (box '()))
(define lifecycle-widget-lock (make-semaphore 1))

;; Register a lifecycle widget. If a widget with the same id exists,
;; unmounts the old widget OUTSIDE the lock, then registers the new one.
(define (register-lifecycle-widget! w)
  (define old-widget
    (call-with-semaphore
     lifecycle-widget-lock
     (lambda ()
       (define existing (unbox lifecycle-widget-registry))
       (define old
         (findf (lambda (ew) (eq? (widget-lifecycle-id ew) (widget-lifecycle-id w))) existing))
       (define filtered
         (filter (lambda (existing-w)
                   (not (eq? (widget-lifecycle-id existing-w) (widget-lifecycle-id w))))
                 existing))
       (set-box! lifecycle-widget-registry (cons w filtered))
       old)))
  ;; Unmount old widget OUTSIDE the lock to avoid deadlock / callback under lock
  (when old-widget
    (widget-unmount! old-widget)))

;; Unregister and unmount a lifecycle widget.
;; Unmount callback runs OUTSIDE the lock.
(define (unregister-lifecycle-widget! id)
  (define removed-widget
    (call-with-semaphore
     lifecycle-widget-lock
     (lambda ()
       (define existing (unbox lifecycle-widget-registry))
       (define to-remove (findf (lambda (w) (eq? (widget-lifecycle-id w) id)) existing))
       (set-box! lifecycle-widget-registry
                 (filter (lambda (w) (not (eq? (widget-lifecycle-id w) id))) existing))
       to-remove)))
  ;; Unmount OUTSIDE the lock
  (when removed-widget
    (widget-unmount! removed-widget)))

;; Lookup a lifecycle widget by id.
(define (lookup-lifecycle-widget id)
  (call-with-semaphore lifecycle-widget-lock
                       (lambda ()
                         (findf (lambda (w) (eq? (widget-lifecycle-id w) id))
                                (unbox lifecycle-widget-registry)))))

;; List all registered lifecycle widgets.
(define (list-lifecycle-widgets)
  (call-with-semaphore lifecycle-widget-lock (lambda () (unbox lifecycle-widget-registry))))

;; Render all lifecycle widgets, concatenating results.
(define (render-all-lifecycle-widgets state width)
  (define widgets (list-lifecycle-widgets))
  (append* (for/list ([w (in-list widgets)])
             (widget-render w state width))))

;; ═══════════════════════════════════════════════════════════════════
;; Focus management for lifecycle widgets
;; ═══════════════════════════════════════════════════════════════════

;; Get focusable lifecycle widgets (those with input-fn).
(define (focusable-lifecycle-widgets)
  (filter (lambda (w) (widget-lifecycle-input-fn w)) (list-lifecycle-widgets)))

;; Cycle focus across lifecycle widgets.
(define current-lifecycle-focus (make-parameter #f))

(define (cycle-lifecycle-focus [direction 1])
  (define focusable (focusable-lifecycle-widgets))
  (if (null? focusable)
      (current-lifecycle-focus #f)
      (let* ([current (current-lifecycle-focus)]
             [idx (for/first ([w focusable]
                              [i (in-naturals)]
                              #:when (eq? (widget-lifecycle-id w) current))
                    i)]
             [pos (or idx -1)]
             [next (modulo (+ pos direction) (length focusable))])
        (define next-id (widget-lifecycle-id (list-ref focusable next)))
        (current-lifecycle-focus next-id)
        next-id)))

;; Route input to the focused lifecycle widget.
(define (route-lifecycle-input key state)
  (define focus-id (current-lifecycle-focus))
  (if focus-id
      (let ([w (lookup-lifecycle-widget focus-id)])
        (if w
            (widget-handle-input w key state)
            (values state (input-bubble))))
      (values state (input-bubble))))

;; ═══════════════════════════════════════════════════════════════════
;; Provides
;; ═══════════════════════════════════════════════════════════════════

(provide (contract-out
          [make-widget-lifecycle
           (->* (symbol? (-> any/c exact-nonnegative-integer? (listof any/c)))
                (#:mount (-> void?)
                         #:input (or/c #f (-> any/c any/c (values any/c any/c)))
                         #:unmount (-> void?))
                widget-lifecycle?)]
          [widget-mount! (-> widget-lifecycle? void?)]
          [widget-render (-> widget-lifecycle? any/c exact-nonnegative-integer? (listof any/c))]
          [widget-handle-input (-> widget-lifecycle? any/c any/c (values any/c any/c))]
          [widget-unmount! (-> widget-lifecycle? void?)]
          [widget-mounted? (-> widget-lifecycle? boolean?)]
          [widget->component (-> widget-lifecycle? q-component?)]
          [register-lifecycle-widget! (-> widget-lifecycle? void?)]
          [unregister-lifecycle-widget! (-> symbol? void?)]
          [lookup-lifecycle-widget (-> symbol? (or/c widget-lifecycle? #f))]
          [list-lifecycle-widgets (-> (listof widget-lifecycle?))]
          [render-all-lifecycle-widgets (-> any/c exact-nonnegative-integer? (listof any/c))]
          [focusable-lifecycle-widgets (-> (listof widget-lifecycle?))]
          [cycle-lifecycle-focus (->* () (exact-integer?) (or/c symbol? #f))]
          [route-lifecycle-input (-> any/c any/c (values any/c any/c))])
         widget-lifecycle
         widget-lifecycle?
         widget-lifecycle-id
         current-lifecycle-focus)
