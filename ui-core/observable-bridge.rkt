#lang racket/base

;; q/ui-core/observable-bridge.rkt — Bridge between event-bus and GUI state
;;
;; Provides a thread-safe bridge that subscribes to an event-bus and
;; propagates events to a GUI state box (for gui-easy or other consumers).
;; The bridge decouples the agent event system from the GUI render loop.
;;
;; Architecture:
;;   event-bus → subscribe! → bridge callback → state-box update → GUI refresh
;;
;; The bridge is designed so gui-easy is NOT required at compile time.
;; It works with plain boxes for headless testing.

(require racket/contract
         racket/match
         "../agent/event-bus.rkt")

(provide gui-state-bridge?
         (contract-out [make-gui-state-bridge
                        (->* [box? any/c]
                             (#:filter (or/c procedure? #f) #:transform (or/c procedure? #f))
                             gui-state-bridge?)]
                       [bridge-subscribe! (-> gui-state-bridge? any/c exact-nonnegative-integer?)]
                       [bridge-dispose! (-> gui-state-bridge? void?)]
                       [bridge-state-ref (-> gui-state-bridge? any/c)]
                       [bridge-update-count (-> gui-state-bridge? exact-nonnegative-integer?)]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct gui-state-bridge
        (state-box ; (boxof any) — current state for GUI
         update-box ; (boxof exact-nonnegative-integer?) — update counter
         lock ; semaphore — thread safety
         filter-fn ; (or/c (-> any/c boolean?) #f)
         transform-fn ; (or/c (-> any/c any/c) #f)
         subscribers-box ; (boxof (listof exact-nonnegative-integer?))
         event-bus-box ; (boxof any/c) — event-bus or #f
         )
  #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-gui-state-bridge state-box
                               initial-state
                               #:filter [filter-fn #f]
                               #:transform [transform-fn #f])
  (set-box! state-box initial-state)
  (gui-state-bridge state-box
                    (box 0) ; update count
                    (make-semaphore 1) ; lock
                    filter-fn
                    transform-fn
                    (box '()) ; subscriber IDs
                    (box #f))) ; event-bus (set during subscribe)

;; ──────────────────────────────
;; Subscribe bridge to an event-bus
;; ──────────────────────────────
(define (bridge-subscribe! bridge bus)
  (define (handle-event evt)
    (call-with-semaphore (gui-state-bridge-lock bridge)
                         (lambda ()
                           ;; Apply filter
                           (when (or (not (gui-state-bridge-filter-fn bridge))
                                     ((gui-state-bridge-filter-fn bridge) evt))
                             ;; Apply transform
                             (define new-state
                               (if (gui-state-bridge-transform-fn bridge)
                                   ((gui-state-bridge-transform-fn bridge) evt)
                                   evt))
                             ;; Update state box
                             (set-box! (gui-state-bridge-state-box bridge) new-state)
                             ;; Increment update counter
                             (set-box! (gui-state-bridge-update-box bridge)
                                       (add1 (unbox (gui-state-bridge-update-box bridge))))))))
  ;; Subscribe to the bus using subscribe! from event-bus
  (define sub-id (subscribe! bus handle-event))
  (call-with-semaphore (gui-state-bridge-lock bridge)
                       (lambda ()
                         (set-box! (gui-state-bridge-subscribers-box bridge)
                                   (cons sub-id (unbox (gui-state-bridge-subscribers-box bridge))))
                         (set-box! (gui-state-bridge-event-bus-box bridge) bus)))
  sub-id)

;; ──────────────────────────────
;; Dispose: unsubscribe from bus
;; ──────────────────────────────
(define (bridge-dispose! bridge)
  (call-with-semaphore (gui-state-bridge-lock bridge)
                       (lambda ()
                         (define bus (unbox (gui-state-bridge-event-bus-box bridge)))
                         (when bus
                           ;; unsubscribe! imported directly
                           (for ([sub-id (in-list (unbox (gui-state-bridge-subscribers-box bridge)))])
                             (unsubscribe! bus sub-id)))
                         (set-box! (gui-state-bridge-subscribers-box bridge) '())
                         (set-box! (gui-state-bridge-event-bus-box bridge) #f))))

;; ──────────────────────────────
;; Accessors
;; ──────────────────────────────
(define (bridge-state-ref bridge)
  (unbox (gui-state-bridge-state-box bridge)))

(define (bridge-update-count bridge)
  (unbox (gui-state-bridge-update-box bridge)))
