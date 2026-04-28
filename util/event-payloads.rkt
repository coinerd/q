#lang racket/base

;; util/event-payloads.rkt — Explicit struct types for event payloads
;;
;; Replaces ad-hoc hasheq payloads on critical event paths with typed
;; structs. Existing hasheq payloads continue to work — these are
;; optional wrappers that provide type safety and better error messages.
;;
;; Usage:
;;   (emit-event bus "session.start" (session-start-payload sid config 'new))
;;   (emit-event bus "tool.call" (tool-call-event-payload sid turn-id tool-name call-id))

(provide (struct-out session-start-payload)
         (struct-out session-end-payload)
         (struct-out tool-call-event-payload)
         (struct-out gsd-mode-payload)
         (struct-out session-switch-payload)
         payload->hash
         payload-session-id)

;; ============================================================
;; Session lifecycle payloads
;; ============================================================

(struct session-start-payload (session-id config reason) #:transparent)
(struct session-end-payload (session-id duration) #:transparent)
(struct session-switch-payload (session-id operation) #:transparent)

;; ============================================================
;; Tool call payloads
;; ============================================================

(struct tool-call-event-payload (session-id turn-id tool-name tool-call-id) #:transparent)

;; ============================================================
;; GSD mode change payloads
;; ============================================================

(struct gsd-mode-payload (old-mode new-mode) #:transparent)

;; ============================================================
;; Generic helpers
;; ============================================================

;; Convert any known payload struct to a hasheq (for serialization/legacy consumers).
(define (payload->hash p)
  (cond
    [(session-start-payload? p)
     (hasheq 'session-id
             (session-start-payload-session-id p)
             'config
             (session-start-payload-config p)
             'reason
             (session-start-payload-reason p))]
    [(session-end-payload? p)
     (hasheq 'session-id
             (session-end-payload-session-id p)
             'duration
             (session-end-payload-duration p))]
    [(session-switch-payload? p)
     (hasheq 'session-id
             (session-switch-payload-session-id p)
             'operation
             (session-switch-payload-operation p))]
    [(tool-call-event-payload? p)
     (hasheq 'session-id
             (tool-call-event-payload-session-id p)
             'turn-id
             (tool-call-event-payload-turn-id p)
             'tool-name
             (tool-call-event-payload-tool-name p)
             'tool-call-id
             (tool-call-event-payload-tool-call-id p))]
    [(gsd-mode-payload? p)
     (hasheq 'old-mode (gsd-mode-payload-old-mode p) 'new-mode (gsd-mode-payload-new-mode p))]
    [(hash? p) p]
    [else (hasheq 'payload p)]))

;; Extract session-id from any payload that has one.
(define (payload-session-id p)
  (cond
    [(session-start-payload? p) (session-start-payload-session-id p)]
    [(session-end-payload? p) (session-end-payload-session-id p)]
    [(session-switch-payload? p) (session-switch-payload-session-id p)]
    [(tool-call-event-payload? p) (tool-call-event-payload-session-id p)]
    [(hash? p) (hash-ref p 'session-id #f)]
    [else #f]))
