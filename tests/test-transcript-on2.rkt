#lang racket

(require rackunit
         racket/list
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; v0.13.1: TUI Transcript O(n²) Fix — cons+reverse-on-read
;; ============================================================
;; These tests verify the transcript storage invariant:
;; - Raw ui-state-transcript stores newest-first (cons)
;; - transcript-entries accessor returns oldest-first (reversed)
;; - visible-entries returns oldest-first
;; - O(1) per insert instead of O(n)

;; Helper: make a test event
(define (make-test-event ev-type [payload (hash)])
  (event 1 ev-type (current-inexact-milliseconds) "test-session" "test-turn" payload))

;; Helper: make a simple entry for testing
(define (make-test-entry kind text)
  (transcript-entry kind text 0 (hash) #f))

;; ============================================================
;; Test 1: transcript-entries accessor exists and returns oldest-first
;; ============================================================
(test-case "transcript-entries returns entries in oldest-first order"
  (let* ([s0 (initial-ui-state)]
         [s1 (apply-event-to-state s0
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "first")))]
         [s2 (apply-event-to-state s1
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "second")))]
         [s3 (apply-event-to-state s2
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "third")))])
    ;; transcript-entries should return oldest-first: [first, second, third]
    (define entries (transcript-entries s3))
    (check-equal? (length entries) 3 "should have 3 entries")
    (check-equal? (transcript-entry-text (first entries)) "first" "oldest first")
    (check-equal? (transcript-entry-text (second entries)) "second" "middle second")
    (check-equal? (transcript-entry-text (third entries)) "third" "newest last")))

;; ============================================================
;; Test 2: visible-entries returns oldest-first
;; ============================================================
(test-case "visible-entries returns oldest-first order"
  (let* ([s0 (initial-ui-state)]
         [s1 (apply-event-to-state s0
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "first")))]
         [s2 (apply-event-to-state s1
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "second")))])
    (define entries (visible-entries s2 10))
    (check-equal? (length entries) 2)
    (check-equal? (transcript-entry-text (first entries)) "first" "oldest first")
    (check-equal? (transcript-entry-text (second entries)) "second" "newest last")))

;; ============================================================
;; Test 3: raw ui-state-transcript is newest-first (internal invariant)
;; ============================================================
(test-case "raw ui-state-transcript stores newest-first"
  (let* ([s0 (initial-ui-state)]
         [s1 (apply-event-to-state s0
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "first")))]
         [s2 (apply-event-to-state s1
                                   (make-test-event "assistant.message.completed"
                                                    (hash 'content "second")))])
    ;; Raw field should be newest-first: [second, first]
    (define raw (ui-state-transcript s2))
    (check-equal? (length raw) 2)
    (check-equal? (transcript-entry-text (first raw)) "second" "newest at head")
    (check-equal? (transcript-entry-text (second raw)) "first" "oldest at tail")))

;; ============================================================
;; Test 4: add-transcript-entry also uses cons (O(1))
;; ============================================================
(test-case "add-transcript-entry uses cons (newest-first raw)"
  (let* ([s0 (initial-ui-state)]
         [e1 (make-test-entry 'user "hello")]
         [s1 (add-transcript-entry s0 e1)]
         [e2 (make-test-entry 'user "world")]
         [s2 (add-transcript-entry s1 e2)])
    ;; Raw field: newest-first
    (define raw (ui-state-transcript s2))
    (check-equal? (length raw) 2)
    (check-equal? (transcript-entry-text (first raw)) "world" "newest at head")
    ;; But transcript-entries reverses to oldest-first
    (define ordered (transcript-entries s2))
    (check-equal? (transcript-entry-text (first ordered)) "hello" "oldest first")))
