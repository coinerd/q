#lang racket/base

;; util/event-codec.rkt — Bidirectional event payload codec
;;
;; Provides encode (payload→hash) and decode (hash→payload) for all known
;; event payload structs. Consolidates serialization logic in one place.
;;
;; STABILITY: stable

(require "event-payloads.rkt")
(provide payload->hash
         hash->payload
         payload-type-tag)

;; Forward to event-payloads.rkt (already exported there, re-exported here
;; for codec consolidation).

;; Determine the type tag of a payload (for codec dispatch).
(define (payload-type-tag p)
  (cond
    [(session-start-payload? p) 'session-start]
    [(session-end-payload? p) 'session-end]
    [(session-switch-payload? p) 'session-switch]
    [(tool-call-event-payload? p) 'tool-call]
    [(session-id-payload? p) 'session-id]
    [(error-payload? p) 'error]
    [(input-payload? p) 'input]
    [(gsd-mode-payload? p) 'gsd-mode]
    [(hash? p) 'raw-hash]
    [else 'unknown]))

;; Decode a hash back into the appropriate payload struct.
;; Returns the hash as-is if no struct type matches (passthrough).
;; v0.28.11: Uses __type tag for reliable decode, falls back to heuristic matching.
(define (hash->payload h)
  (cond
    [(not (hash? h)) h]
    ;; Type-tagged decode (reliable, v0.28.11+)
    [(hash-has-key? h '__type)
     (case (hash-ref h '__type)
       [(error) (error-payload (hash-ref h 'error) (hash-ref h 'errorType))]
       [(input) (input-payload (hash-ref h 'session-id) (hash-ref h 'message))]
       [(session-start)
        (session-start-payload (hash-ref h 'session-id) (hash-ref h 'config) (hash-ref h 'reason))]
       [(session-end) (session-end-payload (hash-ref h 'session-id) (hash-ref h 'duration))]
       [(session-switch) (session-switch-payload (hash-ref h 'session-id) (hash-ref h 'operation))]
       [(tool-call)
        (tool-call-event-payload (hash-ref h 'session-id)
                                 (hash-ref h 'turn-id)
                                 (hash-ref h 'tool-name)
                                 (hash-ref h 'tool-call-id))]
       [(gsd-mode) (gsd-mode-payload (hash-ref h 'old-mode) (hash-ref h 'new-mode))]
       [(session-id) (session-id-payload (hash-ref h 'sessionId))]
       [else h])]
    ;; Legacy heuristic decode (backward compatible with pre-v0.28.11 data)
    [(and (hash-has-key? h 'error) (hash-has-key? h 'errorType))
     (error-payload (hash-ref h 'error) (hash-ref h 'errorType))]
    [(and (hash-has-key? h 'session-id) (hash-has-key? h 'message))
     (input-payload (hash-ref h 'session-id) (hash-ref h 'message))]
    [(and (hash-has-key? h 'session-id) (hash-has-key? h 'config) (hash-has-key? h 'reason))
     (session-start-payload (hash-ref h 'session-id) (hash-ref h 'config) (hash-ref h 'reason))]
    [(and (hash-has-key? h 'session-id) (hash-has-key? h 'duration))
     (session-end-payload (hash-ref h 'session-id) (hash-ref h 'duration))]
    [(and (hash-has-key? h 'session-id) (hash-has-key? h 'operation))
     (session-switch-payload (hash-ref h 'session-id) (hash-ref h 'operation))]
    [(and (hash-has-key? h 'session-id)
          (hash-has-key? h 'turn-id)
          (hash-has-key? h 'tool-name)
          (hash-has-key? h 'tool-call-id))
     (tool-call-event-payload (hash-ref h 'session-id)
                              (hash-ref h 'turn-id)
                              (hash-ref h 'tool-name)
                              (hash-ref h 'tool-call-id))]
    [(and (hash-has-key? h 'old-mode) (hash-has-key? h 'new-mode))
     (gsd-mode-payload (hash-ref h 'old-mode) (hash-ref h 'new-mode))]
    [(hash-has-key? h 'sessionId) (session-id-payload (hash-ref h 'sessionId))]
    [else h]))
