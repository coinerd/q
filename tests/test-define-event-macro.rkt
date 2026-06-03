#lang racket/base

;; BOUNDARY: macro

;; tests/test-define-event-macro.rkt — tests for define-typed-event macro
;;
;; v0.32.2 Wave 0+1: Comprehensive expansion and integration tests.

(require rackunit
         racket/base
         "../agent/event-structs/base.rkt"
         "../util/event/event-macro.rkt")

;; ============================================================
;; Test: define-typed-event with 2 fields
;; ============================================================
(define-typed-event test-turn-start-event "test.turn.started" (model provider))

(test-case "define-typed-event: struct predicate works"
  (define evt
    (make-test-turn-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4" #:provider "openai"))
  (check-true (test-turn-start-event? evt))
  (check-true (typed-event? evt)))

(test-case "define-typed-event: constructor keyword args"
  (define evt
    (make-test-turn-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4" #:provider "openai"))
  (check-equal? (typed-event-type evt) "test.turn.started")
  (check-equal? (typed-event-session-id evt) "s1")
  (check-equal? (typed-event-turn-id evt) "t1")
  (check-equal? (test-turn-start-event-model evt) "gpt-4")
  (check-equal? (test-turn-start-event-provider evt) "openai"))

(test-case "define-typed-event: timestamp defaults to current-seconds"
  (define before (current-seconds))
  (define evt
    (make-test-turn-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4" #:provider "openai"))
  (define after (current-seconds))
  (check-true (<= before (typed-event-timestamp evt)))
  (check-true (<= (typed-event-timestamp evt) after)))

(test-case "define-typed-event: explicit timestamp overrides default"
  (define evt
    (make-test-turn-start-event #:session-id "s1"
                                #:turn-id "t1"
                                #:timestamp 12345
                                #:model "gpt-4"
                                #:provider "openai"))
  (check-equal? (typed-event-timestamp evt) 12345))

(test-case "define-typed-event: type string constant"
  (check-equal? test-turn-start-event-type "test.turn.started"))

(test-case "define-typed-event: field list"
  (check-equal? test-turn-start-event-fields '(model provider)))

;; ============================================================
;; Test: define-typed-event with 0 fields
;; ============================================================
(define-typed-event test-empty-event "test.empty" ())

(test-case "define-typed-event: zero fields"
  (define evt (make-test-empty-event #:session-id "s1" #:turn-id "t1"))
  (check-true (test-empty-event? evt))
  (check-equal? (typed-event-type evt) "test.empty")
  (check-equal? test-empty-event-fields '()))

;; ============================================================
;; Test: define-typed-event with 1 field
;; ============================================================
(define-typed-event test-single-event "test.single" (value))

(test-case "define-typed-event: single field"
  (define evt (make-test-single-event #:session-id "s1" #:turn-id "t1" #:value 42))
  (check-true (test-single-event? evt))
  (check-equal? (test-single-event-value evt) 42)
  (check-equal? test-single-event-fields '(value)))

;; ============================================================
;; Test: define-typed-event with 5 fields
;; ============================================================
(define-typed-event test-multi-event "test.multi" (a b c d e))

(test-case "define-typed-event: five fields"
  (define evt (make-test-multi-event #:session-id "s1" #:turn-id "t1" #:a 1 #:b 2 #:c 3 #:d 4 #:e 5))
  (check-true (test-multi-event? evt))
  (check-equal? (test-multi-event-a evt) 1)
  (check-equal? (test-multi-event-e evt) 5)
  (check-equal? test-multi-event-fields '(a b c d e)))

;; ============================================================
;; Test: struct->vector roundtrip
;; ============================================================
(test-case "define-typed-event: struct->vector serialization"
  (define evt
    (make-test-turn-start-event #:session-id "s1"
                                #:turn-id "t1"
                                #:timestamp 100
                                #:model "gpt-4"
                                #:provider "openai"))
  (define vec (struct->vector evt))
  (check-equal? (vector-length vec) 7)
  (check-equal? (vector-ref vec 1) "test.turn.started")
  (check-equal? (vector-ref vec 2) 100)
  (check-equal? (vector-ref vec 3) "s1")
  (check-equal? (vector-ref vec 4) "t1")
  (check-equal? (vector-ref vec 5) "gpt-4")
  (check-equal? (vector-ref vec 6) "openai"))

;; ============================================================
;; Test: optional field with default
;; ============================================================
(define-typed-event test-optional-event "test.optional" (required-field) #:optional ([opt-field #f]))

(test-case "define-typed-event: optional field defaults"
  (define evt (make-test-optional-event #:session-id "s1" #:turn-id "t1" #:required-field "hello"))
  (check-true (test-optional-event? evt))
  (check-equal? (test-optional-event-required-field evt) "hello")
  (check-equal? (test-optional-event-opt-field evt) #f)
  (check-equal? test-optional-event-fields '(required-field opt-field)))

(test-case "define-typed-event: optional field with explicit value"
  (define evt
    (make-test-optional-event #:session-id "s1"
                              #:turn-id "t1"
                              #:required-field "hello"
                              #:opt-field "world"))
  (check-equal? (test-optional-event-opt-field evt) "world"))

(test-case "define-typed-event: optional fields included in field list"
  (check-equal? test-optional-event-fields '(required-field opt-field)))

;; ============================================================
;; Test: multiple optional fields
;; ============================================================
(define-typed-event test-multi-opt-event "test.multi-opt" (x) #:optional ([y 0] [z 'none]))

(test-case "define-typed-event: multiple optional fields"
  (define evt (make-test-multi-opt-event #:session-id "s1" #:turn-id "t1" #:x 5))
  (check-equal? (test-multi-opt-event-x evt) 5)
  (check-equal? (test-multi-opt-event-y evt) 0)
  (check-equal? (test-multi-opt-event-z evt) 'none)
  (check-equal? test-multi-opt-event-fields '(x y z)))

;; ============================================================
;; Integration: verify real event-struct modules
;; ============================================================
(require "../agent/event-structs/turn-events.rkt"
         "../agent/event-structs/iteration-events.rkt"
         "../agent/event-structs/hook-events.rkt")

(test-case "integration: turn-start-event uses macro"
  (define evt
    (make-turn-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4" #:provider "openai"))
  (check-true (turn-start-event? evt))
  (check-equal? turn-start-event-type "turn.started")
  (check-equal? turn-start-event-fields '(model provider)))

(test-case "integration: injection-event with optional message"
  (define evt
    (make-injection-event #:session-id "s1"
                          #:turn-id "t1"
                          #:source "test"
                          #:content-type "text"
                          #:content-length 42))
  (check-true (injection-event? evt))
  (check-equal? (injection-event-message evt) #f)
  (check-equal? injection-event-fields '(source content-type content-length message)))

(test-case "integration: turn-cancelled-event with optional iteration"
  (define evt (make-turn-cancelled-event #:session-id "s1" #:turn-id "t1" #:reason "timeout"))
  (check-true (turn-cancelled-event? evt))
  (check-equal? (turn-cancelled-event-iteration evt) #f)
  (check-equal? turn-cancelled-event-fields '(reason iteration)))

;; -- T-05: Macro registry wiring break-glass test --
(require (only-in "../util/event/event-macro.rkt" lookup-event-fields register-event-fields!))

(test-case "T-05: register-event-fields! is required for field lookup"
  ;; Verify that a known event type has its fields registered
  (define fields (lookup-event-fields 'turn-start-event))
  (check-not-false fields "turn-start-event should have registered fields")
  (check-equal? fields '(model provider)))

(test-case "T-05: unregistered event type returns #f"
  (define fields (lookup-event-fields 'nonexistent-event-type-xyz))
  (check-false fields))

(test-case "T-05: registration is idempotent"
  ;; Re-registering should not error
  (register-event-fields! 'test-idempotent-event '(field-a field-b))
  (check-equal? (lookup-event-fields 'test-idempotent-event) '(field-a field-b))
  ;; Register again with same values
  (register-event-fields! 'test-idempotent-event '(field-a field-b))
  (check-equal? (lookup-event-fields 'test-idempotent-event) '(field-a field-b)))
