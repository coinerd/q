#lang typed/racket

;; util/event-payloads.rkt — Explicit struct types for event payloads
;; STABILITY: stable
;;
;; Replaces ad-hoc hasheq payloads on critical event paths with typed
;; structs. Existing hasheq payloads continue to work — these are
;; optional wrappers that provide type safety and better error messages.
;;
;; Migrated to Typed Racket in v0.22.6 W5 (RKT-01 pilot).
;;
;; ── TR BOUNDARY ──────────────────────────────────────────────
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR's boundary system. Struct
;; constructors enforce field types at call sites in untyped modules.
;; New consumers should import normally — no require/typed needed.
;; See ADR-0013 (planned) for TR migration strategy.
;; ──────────────────────────────────────────────────────────────

(provide session-start-payload
         session-start-payload?
         session-start-payload-session-id
         session-start-payload-config
         session-start-payload-reason
         session-end-payload
         session-end-payload?
         session-end-payload-session-id
         session-end-payload-duration
         tool-call-event-payload
         tool-call-event-payload?
         tool-call-event-payload-session-id
         tool-call-event-payload-turn-id
         tool-call-event-payload-tool-name
         tool-call-event-payload-tool-call-id
         gsd-mode-payload
         gsd-mode-payload?
         gsd-mode-payload-old-mode
         gsd-mode-payload-new-mode
         session-switch-payload
         session-switch-payload?
         session-switch-payload-session-id
         session-switch-payload-operation
         session-id-payload
         session-id-payload?
         session-id-payload-session-id
         error-payload
         error-payload?
         error-payload-error
         error-payload-error-type
         input-payload
         input-payload?
         input-payload-session-id
         input-payload-message
         payload->hash
         payload-session-id)

;; ============================================================
;; Session lifecycle payloads
;; ============================================================

(struct session-start-payload ([session-id : String] [config : Any] [reason : Symbol]) #:transparent)
(struct session-end-payload ([session-id : String] [duration : Any]) #:transparent)
(struct session-switch-payload ([session-id : String] [operation : Symbol]) #:transparent)

;; ============================================================
;; Tool call payloads
;; ============================================================

(struct tool-call-event-payload
        ([session-id : String] [turn-id : String] [tool-name : String] [tool-call-id : String])
  #:transparent)

;; ============================================================
;; Simple session-id payloads
;; ============================================================

(struct session-id-payload ([session-id : String]) #:transparent)

;; ============================================================
;; Error payloads
;; ============================================================

(struct error-payload ([error : Any] [error-type : Any]) #:transparent)

;; ============================================================
;; Input payloads
;; ============================================================

(struct input-payload ([session-id : String] [message : Any]) #:transparent)

;; ============================================================
;; GSD mode change payloads
;; ============================================================

(struct gsd-mode-payload ([old-mode : Symbol] [new-mode : Symbol]) #:transparent)

;; ============================================================
;; Generic helpers
;; ============================================================

;; Convert any known payload struct to a hasheq (for serialization/legacy consumers).
(: payload->hash (-> Any (HashTable Symbol Any)))
(define (payload->hash p)
  (match p
    [(? session-start-payload?)
     (hasheq '__type
             'session-start
             'session-id
             (session-start-payload-session-id p)
             'config
             (session-start-payload-config p)
             'reason
             (session-start-payload-reason p))]
    [(? session-end-payload?)
     (hasheq '__type
             'session-end
             'session-id
             (session-end-payload-session-id p)
             'duration
             (session-end-payload-duration p))]
    [(? session-switch-payload?)
     (hasheq '__type
             'session-switch
             'session-id
             (session-switch-payload-session-id p)
             'operation
             (session-switch-payload-operation p))]
    [(? tool-call-event-payload?)
     (hasheq '__type
             'tool-call
             'session-id
             (tool-call-event-payload-session-id p)
             'turn-id
             (tool-call-event-payload-turn-id p)
             'tool-name
             (tool-call-event-payload-tool-name p)
             'tool-call-id
             (tool-call-event-payload-tool-call-id p))]
    [(? session-id-payload?)
     (hasheq '__type 'session-id 'sessionId (session-id-payload-session-id p))]
    [(? error-payload?)
     (hasheq '__type 'error 'error (error-payload-error p) 'errorType (error-payload-error-type p))]
    [(? input-payload?)
     (hasheq '__type
             'input
             'session-id
             (input-payload-session-id p)
             'message
             (input-payload-message p))]
    [(? gsd-mode-payload?)
     (hasheq '__type
             'gsd-mode
             'old-mode
             (gsd-mode-payload-old-mode p)
             'new-mode
             (gsd-mode-payload-new-mode p))]
    [(? hash?) (cast p (HashTable Symbol Any))]
    [_ (hasheq 'payload p)]))

;; Extract session-id from any payload that has one.
(: payload-session-id (-> Any (Option String)))
(define (payload-session-id p)
  (match p
    [(? session-start-payload?) (session-start-payload-session-id p)]
    [(? session-end-payload?) (session-end-payload-session-id p)]
    [(? session-switch-payload?) (session-switch-payload-session-id p)]
    [(? tool-call-event-payload?) (tool-call-event-payload-session-id p)]
    [(? session-id-payload?) (session-id-payload-session-id p)]
    [(? input-payload?) (input-payload-session-id p)]
    [(? hash?) (cast (hash-ref (cast p (HashTable Symbol Any)) 'session-id #f) (Option String))]
    [_ #f]))
