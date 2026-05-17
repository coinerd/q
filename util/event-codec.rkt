#lang racket/base

;; util/event-codec.rkt — Bidirectional event payload codec
;;
;; Provides encode (payload→hash) and decode (hash→payload) for all known
;; event payload structs. Consolidates serialization logic in one place.
;;
;; STABILITY: stable

(require racket/contract
         racket/match
         "event-payloads.rkt"
         (only-in "event-macro.rkt" lookup-event-deserializer))
(provide (contract-out [payload->hash (-> any/c hash?)]
                       [hash->payload (-> any/c any/c)]
                       [payload-type-tag (-> any/c symbol?)]))

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

;; Try macro-registered deserializer for typed events.
;; Returns the decoded typed-event struct or #f if no deserializer found.
(define (try-macro-registry h)
  (define type-str (hash-ref h 'type #f))
  (if type-str
      (let ([deser (lookup-event-deserializer type-str)])
        (if deser
            (deser type-str
                   (hash-ref h 'timestamp 0)
                   (hash-ref h 'sessionId "")
                   (hash-ref h 'turnId #f)
                   h)
            #f))
      #f))

;; Decode a hash back into the appropriate payload struct.
;; Returns the hash as-is if no struct type matches (passthrough).
;; v0.28.11: Uses __type tag for reliable decode. No heuristic fallback (removed v0.40.1).
;; v0.42.2: Added macro-typed-event fallback via lookup-event-deserializer.
(define (hash->payload h)
  (cond
    [(not (hash? h)) h]
    ;; Type-tagged decode (reliable, v0.28.11+)
    [(hash-has-key? h '__type)
     (match (hash-ref h '__type)
       ['error (error-payload (hash-ref h 'error) (hash-ref h 'errorType))]
       ['input (input-payload (hash-ref h 'session-id) (hash-ref h 'message))]
       ['session-start
        (session-start-payload (hash-ref h 'session-id) (hash-ref h 'config) (hash-ref h 'reason))]
       ['session-end (session-end-payload (hash-ref h 'session-id) (hash-ref h 'duration))]
       ['session-switch (session-switch-payload (hash-ref h 'session-id) (hash-ref h 'operation))]
       ['tool-call
        (tool-call-event-payload (hash-ref h 'session-id)
                                 (hash-ref h 'turn-id)
                                 (hash-ref h 'tool-name)
                                 (hash-ref h 'tool-call-id))]
       ['gsd-mode (gsd-mode-payload (hash-ref h 'old-mode) (hash-ref h 'new-mode))]
       ['session-id (session-id-payload (hash-ref h 'sessionId))]
       [_ h])]
    ;; Macro-registered typed events (v0.42.2)
    [(hash-has-key? h 'type)
     (define result (try-macro-registry h))
     (or result h)]
    ;; No __type tag and no heuristic match -- return hash as-is (passthrough)
    [else h]))
