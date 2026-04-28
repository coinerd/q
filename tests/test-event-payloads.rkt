#lang racket

;; tests/test-event-payloads.rkt — Tests for util/event-payloads.rkt

(require rackunit
         "../util/event-payloads.rkt")

;; ============================================================
;; Session lifecycle payloads
;; ============================================================

(test-case "session-start-payload"
  (define p (session-start-payload "s1" '#hasheq() 'new))
  (check-equal? (session-start-payload-session-id p) "s1")
  (check-equal? (session-start-payload-reason p) 'new)
  (check-pred session-start-payload? p))

(test-case "session-end-payload"
  (define p (session-end-payload "s1" 42.5))
  (check-equal? (session-end-payload-session-id p) "s1")
  (check-equal? (session-end-payload-duration p) 42.5))

(test-case "session-switch-payload"
  (define p (session-switch-payload "s2" 'resume))
  (check-equal? (session-switch-payload-session-id p) "s2")
  (check-equal? (session-switch-payload-operation p) 'resume))

;; ============================================================
;; Tool call payloads
;; ============================================================

(test-case "tool-call-event-payload"
  (define p (tool-call-event-payload "s1" "t1" "read" "call-1"))
  (check-equal? (tool-call-event-payload-session-id p) "s1")
  (check-equal? (tool-call-event-payload-turn-id p) "t1")
  (check-equal? (tool-call-event-payload-tool-name p) "read")
  (check-equal? (tool-call-event-payload-tool-call-id p) "call-1"))

;; ============================================================
;; GSD mode change payloads
;; ============================================================

(test-case "gsd-mode-payload"
  (define p (gsd-mode-payload 'idle 'planning))
  (check-equal? (gsd-mode-payload-old-mode p) 'idle)
  (check-equal? (gsd-mode-payload-new-mode p) 'planning))

;; ============================================================
;; Generic helpers
;; ============================================================

(test-case "payload->hash converts struct to hasheq"
  (define p (session-start-payload "s1" '#hasheq() 'new))
  (define h (payload->hash p))
  (check-equal? (hash-ref h 'session-id) "s1")
  (check-equal? (hash-ref h 'reason) 'new))

(test-case "payload->hash passes through existing hash"
  (define h (hasheq 'foo "bar"))
  (check-equal? (payload->hash h) h))

(test-case "payload-session-id extracts from struct"
  (check-equal? (payload-session-id (session-end-payload "s3" 10)) "s3")
  (check-equal? (payload-session-id (session-start-payload "s4" #f 'resume)) "s4"))

(test-case "payload-session-id extracts from hash"
  (check-equal? (payload-session-id (hasheq 'session-id "h1")) "h1")
  (check-false (payload-session-id (hasheq 'other "x"))))

(test-case "payload-session-id returns #f for unknown types"
  (check-false (payload-session-id "not a payload")))

(test-case "tool-call-event-payload -> hash roundtrip"
  (define p (tool-call-event-payload "s1" "t1" "write" "c1"))
  (define h (payload->hash p))
  (check-equal? (hash-ref h 'tool-name) "write")
  (check-equal? (hash-ref h 'tool-call-id) "c1"))

(test-case "gsd-mode-payload -> hash roundtrip"
  (define p (gsd-mode-payload 'planning 'executing))
  (define h (payload->hash p))
  (check-equal? (hash-ref h 'old-mode) 'planning)
  (check-equal? (hash-ref h 'new-mode) 'executing))
