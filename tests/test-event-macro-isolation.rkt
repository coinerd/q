#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-event-macro-isolation.rkt — A3: Event macro registry isolation
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../util/event/event-macro.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "Event Macro Isolation (A3)"

    ;; Test 1: with-fresh-event-registries resets ALL 4 registries
    (test-case "with-fresh-event-registries resets all registries"
      ;; Populate registries
      (hash-set! (current-event-field-registry) 'test-event '(field1 field2))
      (hash-set! (current-event-serializer-registry) "test.event" (lambda (e) 'serialized))
      (hash-set! (current-event-deserializer-registry) "test.event" (lambda (d) 'deserialized))
      (hash-set! (current-event-schema-registry) "test.event" 99)

      ;; Verify populated
      (check-not-false (hash-ref (current-event-field-registry) 'test-event #f))
      (check-not-false (hash-ref (current-event-serializer-registry) "test.event" #f))
      (check-not-false (hash-ref (current-event-deserializer-registry) "test.event" #f))
      (check-not-false (hash-ref (current-event-schema-registry) "test.event" #f))

      ;; Verify fresh scope is clean
      (with-fresh-event-registries
       (check-false (hash-ref (current-event-field-registry) 'test-event #f))
       (check-false (hash-ref (current-event-serializer-registry) "test.event" #f))
       (check-false (hash-ref (current-event-deserializer-registry) "test.event" #f))
       (check-false (hash-ref (current-event-schema-registry) "test.event" #f))))

    ;; Test 2: Registries are independent
    (test-case "registries don't leak across isolation boundaries"
      (with-fresh-event-registries
       (hash-set! (current-event-field-registry) 'isolated '(x))
       (check-equal? (hash-ref (current-event-field-registry) 'isolated #f) '(x)))
      ;; After scope, original registry restored
      (check-false (hash-ref (current-event-field-registry) 'isolated #f)))))

(run-tests suite)
