#lang racket/base

;; tests/test-define-event-macro.rkt — tests for define-typed-event macro
;;
;; v0.32.2 Wave 0: Comprehensive expansion tests.

(require rackunit
         racket/base
         "../agent/event-structs/base.rkt"
         "../util/event-macro.rkt")

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
;; Test: deprecated define-event still works
;; ============================================================
(define-event test-legacy-event (x y))

(test-case "define-event (deprecated): still works"
  (define evt (test-legacy-event 1 2))
  (check-true (test-legacy-event? evt))
  (check-equal? (test-legacy-event-x evt) 1)
  (check-equal? (test-legacy-event-y evt) 2))
