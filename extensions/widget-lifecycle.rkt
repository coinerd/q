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

;; Register a lifecycle widget.
(define (register-lifecycle-widget! w)
  (call-with-semaphore lifecycle-widget-lock
                       (lambda ()
                         (define existing (unbox lifecycle-widget-registry))
                         (define filtered
                           (filter (lambda (existing-w)
                                     (not (eq? (widget-lifecycle-id existing-w)
                                               (widget-lifecycle-id w))))
                                   existing))
                         (set-box! lifecycle-widget-registry (cons w filtered)))))

;; Unregister and unmount a lifecycle widget.
(define (unregister-lifecycle-widget! id)
  (call-with-semaphore
   lifecycle-widget-lock
   (lambda ()
     (define existing (unbox lifecycle-widget-registry))
     (define to-remove (findf (lambda (w) (eq? (widget-lifecycle-id w) id)) existing))
     (when to-remove
       (widget-unmount! to-remove))
     (set-box! lifecycle-widget-registry
               (filter (lambda (w) (not (eq? (widget-lifecycle-id w) id))) existing)))))

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

(provide widget-lifecycle
         widget-lifecycle?
         widget-lifecycle-id
         make-widget-lifecycle
         widget-mount!
         widget-render
         widget-handle-input
         widget-unmount!
         widget-mounted?
         widget->component
         register-lifecycle-widget!
         unregister-lifecycle-widget!
         lookup-lifecycle-widget
         list-lifecycle-widgets
         render-all-lifecycle-widgets
         focusable-lifecycle-widgets
         cycle-lifecycle-focus
         current-lifecycle-focus
         route-lifecycle-input)
