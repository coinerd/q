#lang racket/base

;; extensions/gsd/events.rkt — Stable GSD Event Telemetry
;; STABILITY: evolving
;;
;; F6 fix: Stable event names + correlation IDs for GSD lifecycle.
;; All events follow: gsd.<category>.<action>

(require racket/match
         (only-in "../../util/errors.rkt" raise-extension-error)
         (only-in "../../agent/event-structs/base.rkt" typed-event?)
         (only-in "../../agent/event-emitter.rkt" event-struct->hasheq)
         (only-in "session-state.rkt"
                  gsd-session-ctx?
                  gsd-session-ctx-event-bus-box
                  gsd-default-ctx
                  gsd-ctx-event-bus))

(provide gsd-event-names
         emit-gsd-event!
         current-gsd-correlation-id
         ;; Event bus integration
         gsd-event-bus-box
         set-gsd-event-bus!
         ;; For testing: event collector
         make-event-collector
         collector-events
         ctx-emit-gsd-event!
         resolve-gsd-event-bus)

;; ============================================================
;; Event taxonomy
;; ============================================================

(define gsd-event-names
  '(gsd.command.received gsd.command.completed
                         gsd.transition.attempted
                         gsd.transition.succeeded
                         gsd.transition.failed
                         gsd.mode.changed
                         gsd.wave.started
                         gsd.wave.completed
                         gsd.wave.failed
                         gsd.wave.skipped
                         gsd.guard.blocked
                         gsd.guard.allowed
                         gsd.plan.parsed
                         gsd.plan.normalized
                         gsd.plan.validated
                         gsd.plan.archived
                         gsd.archive.failed))

;; ============================================================
;; Correlation ID parameter
;; ============================================================

(define current-gsd-correlation-id (make-parameter #f))

;; ============================================================
;; Event bus integration
;; ============================================================

;; Box holding an event emission procedure: (symbol? any/c . -> . void?)
;; Default: no-op. Callers can set this to wire into session event bus.
(define gsd-event-bus-box (box void))

(define (set-gsd-event-bus! proc)
  (set-box! gsd-event-bus-box proc))

(define (emit-gsd-event! event-name data)
  (unless (memq event-name gsd-event-names)
    (raise-extension-error (format "Unknown event: ~a" event-name) 'gsd 'emit-event))
  (define bus (unbox gsd-event-bus-box))
  (define wrapped
    (if (typed-event? data)
        (hasheq 'event
                event-name
                'correlation-id
                (current-gsd-correlation-id)
                'timestamp
                (current-inexact-milliseconds)
                'data
                (event-struct->hasheq data)
                '__typed
                #t)
        (hasheq 'event
                event-name
                'correlation-id
                (current-gsd-correlation-id)
                'timestamp
                (current-inexact-milliseconds)
                'data
                data)))
  (bus event-name wrapped))

;; ============================================================
;; Context-aware event emission (v0.57.1 W5)
;; ============================================================

;; Resolve event bus: prefer ctx-specific bus, fallback to global.
(define (resolve-gsd-event-bus ctx)
  (define ctx-bus (and ctx (gsd-ctx-event-bus ctx)))
  (or ctx-bus (unbox gsd-event-bus-box)))

;; Emit to a specific session context's event bus.
;; Falls back to global gsd-event-bus-box if ctx has no bus set.
(define (ctx-emit-gsd-event! ctx event-name data)
  (unless (memq event-name gsd-event-names)
    (raise-extension-error (format "Unknown event: ~a" event-name) 'gsd 'emit-event))
  (define bus (resolve-gsd-event-bus ctx))
  (define wrapped
    (if (typed-event? data)
        (hasheq 'event
                event-name
                'correlation-id
                (current-gsd-correlation-id)
                'timestamp
                (current-inexact-milliseconds)
                'data
                (event-struct->hasheq data)
                '__typed
                #t)
        (hasheq 'event
                event-name
                'correlation-id
                (current-gsd-correlation-id)
                'timestamp
                (current-inexact-milliseconds)
                'data
                data)))
  (bus event-name wrapped))

;; ============================================================
;; Event collector (for testing)
;; ============================================================

(define (make-event-collector)
  (define events-box (box '()))
  ;; Collector procedure — pass to set-gsd-event-bus!
  (values (lambda (event-name wrapped-event)
            ;; emit-gsd-event! already wraps with event/correlation-id/timestamp/data
            (set-box! events-box (append (unbox events-box) (list wrapped-event))))
          ;; Query procedure — get collected events
          (lambda () (unbox events-box))))

(define (collector-events query-proc)
  (query-proc))
