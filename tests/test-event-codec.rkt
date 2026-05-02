#lang racket/base

(require rackunit
         "../util/event-codec.rkt"
         "../util/event-payloads.rkt")

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
