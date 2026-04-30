#lang racket/base

;; extensions/gsd/events.rkt — Stable GSD Event Telemetry
;; STABILITY: evolving
;;
;; F6 fix: Stable event names + correlation IDs for GSD lifecycle.
;; All events follow: gsd.<category>.<action>

(require racket/match)

(provide gsd-event-names
         emit-gsd-event!
         current-gsd-correlation-id
         ;; Event bus integration
         gsd-event-bus-box
         set-gsd-event-bus!
         ;; For testing: event collector
         make-event-collector
         collector-events)

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
    (error 'emit-gsd-event! "Unknown event: ~a" event-name))
  (define bus (unbox gsd-event-bus-box))
  (bus event-name
       (hasheq 'event
               event-name
               'correlation-id
               (current-gsd-correlation-id)
               'timestamp
               (current-inexact-milliseconds)
               'data
               data)))

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
