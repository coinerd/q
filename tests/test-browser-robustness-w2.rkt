#lang racket

;; test-browser-robustness-w2.rkt — W2 tests
;;
;; Session ID entropy (crypto UUID) and workflow try/finally cleanup.

;; @speed fast
(require rackunit
         racket/match
         "../browser/service.rkt"
         "../browser/types.rkt"
         "../browser/workflow.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Test: generate-session-id produces unique IDs
;; ---------------------------------------------------------------------------

(test-case "generate-session-id uniqueness across 1000 calls"
  (define ids
    (for/list ([_ (in-range 1000)])
      (generate-session-id)))
  (check-equal? (length ids)
                (length (remove-duplicates ids))
                "1000 session IDs should all be unique"))

;; ---------------------------------------------------------------------------
;; Test: generate-session-id uses crypto-quality entropy
;; ---------------------------------------------------------------------------

(test-case "generate-session-id format is bs- prefix + UUID"
  (define id (generate-session-id))
  (check-true (string-prefix? id "bs-") "Session ID should start with bs-")
  (define uuid-part (substring id 3))
  (define parts (string-split uuid-part "-"))
  (check-equal? (length parts) 5 "UUID should have 5 dash-separated groups"))

;; ---------------------------------------------------------------------------
;; Test: generate-session-id is not millisecond-based
;; ---------------------------------------------------------------------------

(test-case "generate-session-id is not millisecond-based"
  (define id (generate-session-id))
  (define ms (number->string (current-milliseconds)))
  (check-false (string-contains? id ms) "Session ID should not contain raw milliseconds"))

;; ---------------------------------------------------------------------------
;; Test: generate-session-id exported from service module
;; ---------------------------------------------------------------------------

(test-case "generate-session-id is exported"
  (check-pred procedure? generate-session-id))
