#lang racket

;; tests/test-tui-state.rkt — Tests for TUI state, focusing on error hint rendering

(require rackunit
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; Helper: extract last transcript entry text
;; ============================================================

(define (last-entry-text state)
  (define entries (ui-state-transcript state))
  (if (null? entries)
      #f
      (transcript-entry-text (last entries))))

(define (last-2-entry-texts state)
  (define entries (ui-state-transcript state))
  (if (< (length entries) 2)
      '()
      (map transcript-entry-text (take-right entries 2))))

;; ============================================================
;; Error recovery hint tests (v0.12.2 Wave B)
;; ============================================================

(test-case "B1: rate-limit without retries shows 'will retry automatically'"
  (define state (initial-ui-state))
  (define evt (make-event "runtime.error"
                           (current-inexact-milliseconds)
                           #f #f
                           (hasheq 'error "HTTP 429 rate limit"
                                   'errorType 'rate-limit)))
  (define next (apply-event-to-state state evt))
  (define texts (last-2-entry-texts next))
  (check-true (ormap (λ (t) (and t (string-contains? t "retry automatically"))) texts)
              (format "Expected 'retry automatically' in ~a" texts)))

(test-case "B1: rate-limit WITH retries shows 'persisted after N retries'"
  (define state (initial-ui-state))
  (define evt (make-event "runtime.error"
                           (current-inexact-milliseconds)
                           #f #f
                           (hasheq 'error "HTTP 429 rate limit (after 2 retries)"
                                   'errorType 'rate-limit
                                   'retries-attempted 2)))
  (define next (apply-event-to-state state evt))
  (define texts (last-2-entry-texts next))
  (check-true (ormap (λ (t) (and t (string-contains? t "persisted after 2 retries"))) texts)
              (format "Expected 'persisted after 2 retries' in ~a" texts))
  ;; Should NOT show the old message
  (check-false (ormap (λ (t) (and t (string-contains? t "retry automatically"))) texts)
               "Should NOT show 'retry automatically' when retries exhausted"))

(test-case "B1: timeout without retries shows basic hint"
  (define state (initial-ui-state))
  (define evt (make-event "runtime.error"
                           (current-inexact-milliseconds)
                           #f #f
                           (hasheq 'error "connection timed out"
                                   'errorType 'timeout)))
  (define next (apply-event-to-state state evt))
  (define texts (last-2-entry-texts next))
  (check-true (ormap (λ (t) (and t (string-contains? t "timed out"))) texts)
              (format "Expected 'timed out' in ~a" texts)))

(test-case "B1: timeout WITH retries shows 'after N retries'"
  (define state (initial-ui-state))
  (define evt (make-event "runtime.error"
                           (current-inexact-milliseconds)
                           #f #f
                           (hasheq 'error "connection timed out (after 1 retries)"
                                   'errorType 'timeout
                                   'retries-attempted 1)))
  (define next (apply-event-to-state state evt))
  (define texts (last-2-entry-texts next))
  (check-true (ormap (λ (t) (and t (string-contains? t "after 1 retries"))) texts)
              (format "Expected 'after 1 retries' in ~a" texts)))

(test-case "B1: auth error shows API key hint"
  (define state (initial-ui-state))
  (define evt (make-event "runtime.error"
                           (current-inexact-milliseconds)
                           #f #f
                           (hasheq 'error "401 Unauthorized"
                                   'errorType 'auth)))
  (define next (apply-event-to-state state evt))
  (define texts (last-2-entry-texts next))
  (check-true (ormap (λ (t) (and t (string-contains? t "API key"))) texts)
              (format "Expected 'API key' in ~a" texts)))
