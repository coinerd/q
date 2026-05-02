#lang racket/base

(require rackunit
         "../util/event-codec.rkt"
         (only-in "../util/event-payloads.rkt"
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

;; hash->payload round-trip tests
(test-case "hash->payload decodes error-payload"
  (define p (hash->payload (hasheq 'error "oops" 'errorType 'runtime)))
  (check-true (error-payload? p))
  (check-equal? (error-payload-error p) "oops")
  (check-equal? (error-payload-error-type p) 'runtime))

(test-case "hash->payload decodes input-payload"
  (define p (hash->payload (hasheq 'session-id "s1" 'message "hello")))
  (check-true (input-payload? p))
  (check-equal? (input-payload-session-id p) "s1"))

(test-case "hash->payload decodes gsd-mode-payload"
  (define p (hash->payload (hasheq 'old-mode 'inbox 'new-mode 'planning)))
  (check-true (gsd-mode-payload? p))
  (check-equal? (gsd-mode-payload-old-mode p) 'inbox)
  (check-equal? (gsd-mode-payload-new-mode p) 'planning))

(test-case "hash->payload decodes session-id-payload"
  (define p (hash->payload (hasheq 'sessionId "s1")))
  (check-true (session-id-payload? p))
  (check-equal? (session-id-payload-session-id p) "s1"))

(test-case "hash->payload passthrough for unknown hash"
  (define h (hasheq 'random 'data))
  (check-equal? (hash->payload h) h))

(test-case "hash->payload passthrough for non-hash"
  (check-equal? (hash->payload "string") "string"))

;; Round-trip: encode → decode
(test-case "encode-decode round-trip for error-payload"
  (define orig (error-payload "fail" 'timeout))
  (define encoded (payload->hash orig))
  (define decoded (hash->payload encoded))
  (check-true (error-payload? decoded))
  (check-equal? (error-payload-error decoded) "fail")
  (check-equal? (error-payload-error-type decoded) 'timeout))

(test-case "encode-decode round-trip for gsd-mode-payload"
  (define orig (gsd-mode-payload 'inbox 'done))
  (define encoded (payload->hash orig))
  (define decoded (hash->payload encoded))
  (check-true (gsd-mode-payload? decoded))
  (check-equal? (gsd-mode-payload-old-mode decoded) 'inbox)
  (check-equal? (gsd-mode-payload-new-mode decoded) 'done))

;; v0.28.11: Type-tagged round-trip tests
(test-case "encode includes __type tag"
  (define encoded (payload->hash (error-payload "fail" 'timeout)))
  (check-equal? (hash-ref encoded '__type) 'error))

(test-case "type-tagged decode for input-payload"
  (define h (hasheq '__type 'input 'session-id "s1" 'message "hi"))
  (define p (hash->payload h))
  (check-true (input-payload? p))
  (check-equal? (input-payload-session-id p) "s1"))

(test-case "type-tagged decode for session-start"
  (define h (hasheq '__type 'session-start 'session-id "s1" 'config '() 'reason 'manual))
  (define p (hash->payload h))
  (check-true (session-start-payload? p)))

(test-case "type-tagged decode falls through for unknown __type"
  (define h (hasheq '__type 'bogus 'data 42))
  (check-equal? (hash->payload h) h))

(test-case "legacy heuristic still works without __type"
  (define h (hasheq 'error "oops" 'errorType 'runtime))
  (define p (hash->payload h))
  (check-true (error-payload? p))
  (check-equal? (error-payload-error p) "oops"))

(test-case "full round-trip with __type preserves data"
  (define orig (input-payload "sess42" "hello world"))
  (define encoded (payload->hash orig))
  (check-equal? (hash-ref encoded '__type) 'input)
  (define decoded (hash->payload encoded))
  (check-true (input-payload? decoded))
  (check-equal? (input-payload-session-id decoded) "sess42")
  (check-equal? (input-payload-message decoded) "hello world"))
