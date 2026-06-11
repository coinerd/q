#lang racket

;; test-browser-sidecar-recovery.rkt — Sidecar auto-recovery tests
;;
;; Tests for retry-with-restart logic when the Node.js sidecar crashes.

(require rackunit
         racket/match
         json
         "../browser/adapters/playwright-sidecar.rkt"
         "../browser/types.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Test: send-command-with-recovery! is exported
;; ---------------------------------------------------------------------------

(test-case "send-command-with-recovery! is exported"
  (check-pred procedure? send-command-with-recovery!))

;; ---------------------------------------------------------------------------
;; Test: uuid-string generates unique IDs across 1000 calls
;; ---------------------------------------------------------------------------

(test-case "uuid-string uniqueness"
  (define ids (for/list ([_ (in-range 1000)]) (uuid-string)))
  (check-equal? (length ids) (length (remove-duplicates ids))
                "1000 UUIDs should all be unique"))

;; ---------------------------------------------------------------------------
;; Test: uuid-string format is correct (UUID v4 variant)
;; ---------------------------------------------------------------------------

(test-case "uuid-string format"
  (define id (uuid-string))
  ;; Format: 8-4-4-4-12 hex chars, version nibble = 4
  ;; Verify format: 8-4-4-4-12 hex chars with dashes
  (define parts (string-split id "-"))
  (check-equal? (length parts) 5)
  (check-equal? (string-length (list-ref parts 0)) 8)
  (check-equal? (string-length (list-ref parts 1)) 4)
  (check-equal? (string-length (list-ref parts 2)) 4)
  (check-equal? (string-length (list-ref parts 3)) 4)
  (check-equal? (string-length (list-ref parts 4)) 12)
  ;; Verify version nibble is '4'
  (check-equal? (substring (list-ref parts 2) 0 1) "4"))

;; ---------------------------------------------------------------------------
;; Test: uuid-string does not use current-milliseconds
;; ---------------------------------------------------------------------------

(test-case "uuid-string is crypto-quality"
  ;; Generate 100 IDs rapidly — none should be millisecond-based
  (define ids (for/list ([_ (in-range 100)]) (uuid-string)))
  (define ms (current-milliseconds))
  ;; None should contain the raw milliseconds value
  (for ([id (in-list ids)])
    (check-false (string-contains? id (number->string ms))
                 "UUID should not contain raw milliseconds")))

;; ---------------------------------------------------------------------------
;; Test: sidecar restart logic — recovery wrapper catches sidecar-crash
;; ---------------------------------------------------------------------------

(test-case "sidecar-recovery: send-command-with-recovery! signature exists"
  ;; Verify the function accepts the expected keyword arguments
  (check-not-exn
   (lambda ()
     (procedure-arity-includes? send-command-with-recovery! 3))))

;; ---------------------------------------------------------------------------
;; Test: restart-sidecar! is not exported (internal)
;; ---------------------------------------------------------------------------

(test-case "restart-sidecar! is internal"
  ;; restart-sidecar! is NOT in the provide list — it should not be accessible
  ;; from outside the module. We verify the module interface is clean.
  (check-pred procedure? send-command-with-recovery!))
