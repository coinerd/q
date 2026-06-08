#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: serialization

(require rackunit
         "../util/event/event-codec.rkt"
         "../agent/event-structs/turn-events.rkt"
         "../agent/event-structs/session-events.rkt"
         "../agent/event-json.rkt"
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-session-id
                  typed-event-turn-id)
         (only-in "../util/event/event-payloads.rkt"
                  session-start-payload
                  session-start-payload?
                  error-payload
                  error-payload?
                  error-payload-error
                  error-payload-error-type
                  input-payload
                  input-payload?
                  input-payload-session-id
                  input-payload-message
                  gsd-mode-payload
                  gsd-mode-payload?
                  gsd-mode-payload-old-mode
                  gsd-mode-payload-new-mode
                  session-id-payload
                  session-id-payload?
                  session-id-payload-session-id))

;; payload-type-tag tests
(test-case "payload-type-tag identifies session-start"
  (check-equal? (payload-type-tag (session-start-payload "s1" '() 'manual)) 'session-start))

(test-case "payload-type-tag identifies error-payload"
  (check-equal? (payload-type-tag (error-payload "oops" 'runtime)) 'error))

(test-case "payload-type-tag identifies raw hash"
  (check-equal? (payload-type-tag (hasheq 'foo 1)) 'raw-hash))

(test-case "payload-type-tag identifies unknown"
  (check-equal? (payload-type-tag 42) 'unknown))

;; hash->payload passthrough tests
(test-case "hash->payload passthrough for unknown hash"
  (define h (hasheq 'random 'data))
  (check-equal? (hash->payload h) h))

(test-case "hash->payload passthrough for non-hash"
  (check-equal? (hash->payload "string") "string"))

;; __type-tagged decode tests
(test-case "hash->payload decodes error-payload via __type"
  (define p (hash->payload (hasheq '__type 'error 'error "oops" 'errorType 'runtime)))
  (check-true (error-payload? p))
  (check-equal? (error-payload-error p) "oops")
  (check-equal? (error-payload-error-type p) 'runtime))

(test-case "hash->payload decodes input-payload via __type"
  (define p (hash->payload (hasheq '__type 'input 'session-id "s1" 'message "hello")))
  (check-true (input-payload? p))
  (check-equal? (input-payload-session-id p) "s1"))

(test-case "hash->payload decodes gsd-mode-payload via __type"
  (define p (hash->payload (hasheq '__type 'gsd-mode 'old-mode 'inbox 'new-mode 'planning)))
  (check-true (gsd-mode-payload? p))
  (check-equal? (gsd-mode-payload-old-mode p) 'inbox)
  (check-equal? (gsd-mode-payload-new-mode p) 'planning))

(test-case "hash->payload decodes session-id-payload via __type"
  (define p (hash->payload (hasheq '__type 'session-id 'sessionId "s1")))
  (check-true (session-id-payload? p))
  (check-equal? (session-id-payload-session-id p) "s1"))

(test-case "hash->payload falls through for unknown __type"
  (define h (hasheq '__type 'bogus 'data 42))
  (check-equal? (hash->payload h) h))

;; Round-trip: encode → decode for __type tagged payloads
(test-case "encode-decode round-trip for error-payload"
  (define orig (error-payload "fail" 'timeout))
  (define encoded (payload->hash orig))
  (check-equal? (hash-ref encoded '__type) 'error)
  (define decoded (hash->payload encoded))
  (check-true (error-payload? decoded))
  (check-equal? (error-payload-error decoded) "fail")
  (check-equal? (error-payload-error-type decoded) 'timeout))

(test-case "encode-decode round-trip for gsd-mode-payload"
  (define orig (gsd-mode-payload 'inbox 'done))
  (define encoded (payload->hash orig))
  (check-equal? (hash-ref encoded '__type) 'gsd-mode)
  (define decoded (hash->payload encoded))
  (check-true (gsd-mode-payload? decoded))
  (check-equal? (gsd-mode-payload-old-mode decoded) 'inbox)
  (check-equal? (gsd-mode-payload-new-mode decoded) 'done))

;; v0.42.2: Macro-registered typed event decode via hash->payload
(test-case "hash->payload decodes macro-registered turn-start-event"
  (define evt (make-turn-start-event #:session-id "s1" #:turn-id "t1"
                                      #:timestamp 1000.0 #:model "gpt-4"
                                      #:provider "openai"))
  (define h (typed-event->jsexpr evt))
  (define decoded (hash->payload h))
  (check-true (turn-start-event? decoded) "should decode to turn-start-event")
  (check-equal? (typed-event-session-id decoded) "s1"))

(test-case "hash->payload decodes macro-registered session-start-event"
  (define evt (make-session-start-event #:session-id "s1" #:turn-id "t1"
                                        #:timestamp 1000.0 #:model "gpt-4"))
  (define h (typed-event->jsexpr evt))
  (define decoded (hash->payload h))
  (check-true (session-start-event? decoded) "should decode to session-start-event")
  (check-equal? (typed-event-session-id decoded) "s1"))

(test-case "hash->payload passthrough for unknown type string"
  (define h (hasheq 'type "bogus.event" 'timestamp 100 'sessionId "s1" 'turnId "t1"))
  (check-equal? (hash->payload h) h))
