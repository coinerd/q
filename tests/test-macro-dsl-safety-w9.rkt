#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: macro

;; tests/test-macro-dsl-safety-w9.rkt — Macro/DSL expansion safety tests (v0.99.36 W9)
;;
;; Expansion safety tests for the 3 most complex macros in the codebase:
;;   1. define-typed-event (util/event/event-macro.rkt)
;;   2. define-fsm-machine (util/fsm/fsm.rkt)
;;   3. define-tool (tools/define-tool.rkt)
;;
;; Focus: hygiene, evaluation count, edge cases, generated binding correctness.

(require rackunit
         rackunit/text-ui
         "../util/event/event-macro.rkt"
         "../util/fsm/fsm.rkt"
         "../tools/define-tool.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Module-level definitions for define-typed-event tests
;; (define-typed-event generates provide forms, so must be at module level)
;; ============================================================

;; Event with #:json-keys explicit mapping
(define-typed-event w9-jsonkey-evt "w9.test.jsonkeys" (my-field) #:json-keys (my-field customKey))

;; Event with #:defaults
(define-typed-event w9-defaults-evt "w9.test.defaults" (req-field) #:defaults (req-field "fallback"))

;; Event with #:schema-version
(define-typed-event w9-schema-evt "w9.test.schemaver" (data) #:schema-version 3)

;; Event with #:no-serialize
(define-typed-event w9-noser-evt "w9.test.noser" (field) #:no-serialize)

;; Event with zero optional fields
(define-typed-event w9-reqonly-evt "w9.test.reqonly" (a b c))

;; Event with field name ending in ?
(define-typed-event w9-pred-evt "w9.test.pred" (is-active?))

;; ============================================================
;; 1. define-typed-event expansion safety tests
;; ============================================================

(define typed-event-suite
  (test-suite "define-typed-event expansion safety"

    (test-case "#:json-keys maps field to custom JSON key"
      (define ev (make-w9-jsonkey-evt #:session-id "s1" #:turn-id "t1" #:my-field "val"))
      (define ser (lookup-event-serializer "w9.test.jsonkeys"))
      (check-pred procedure? ser "serializer should be registered")
      (when ser
        (define h (ser ev))
        (check-true (hash-has-key? h 'customKey) "custom JSON key should be present")
        (check-false (hash-has-key? h 'myField) "default camelCase key should NOT be present")))

    (test-case "#:defaults provides fallback values"
      ;; #:defaults affects deserialization, not constructor
      (define ev-with (make-w9-defaults-evt #:session-id "s1" #:turn-id "t1" #:req-field "explicit"))
      (check-equal? (w9-defaults-evt-req-field ev-with) "explicit"))

    (test-case "#:schema-version registers correct version"
      (check-equal? (lookup-event-schema-version "w9.test.schemaver") 3))

    (test-case "#:no-serialize prevents serializer registration"
      (check-false (lookup-event-serializer "w9.test.noser") "serializer should NOT be registered")
      (check-false (lookup-event-deserializer "w9.test.noser")
                   "deserializer should NOT be registered")
      ;; Struct and constructor still work
      (check-pred w9-noser-evt? (make-w9-noser-evt #:session-id "s1" #:turn-id "t1" #:field "x")))

    (test-case "zero optional fields works"
      (define ev (make-w9-reqonly-evt #:session-id "s1" #:turn-id "t1" #:a 1 #:b 2 #:c 3))
      (check-pred w9-reqonly-evt? ev)
      (check-equal? (w9-reqonly-evt-a ev) 1)
      (check-equal? (w9-reqonly-evt-b ev) 2)
      (check-equal? (w9-reqonly-evt-c ev) 3))

    (test-case "field with ? suffix gets correct JSON key"
      (define ser (lookup-event-serializer "w9.test.pred"))
      (check-pred procedure? ser "serializer should be registered")
      (when ser
        (define ev (make-w9-pred-evt #:session-id "s1" #:turn-id "t1" #:is-active? #t))
        (define h (ser ev))
        ;; field->json-key strips ? and converts: is-active? -> isActive
        (check-true (hash-has-key? h 'isActive)
                    "predicate field should have ? stripped in JSON key")))))

;; ============================================================
;; 2. define-fsm-machine expansion safety tests
;; ============================================================

(define-fsm-machine w9-empty-trans #:states (idle) #:events (noop) #:transitions)

(define-fsm-machine w9-single
                    #:states (only)
                    #:events (ping pong)
                    #:transitions [(only -> only) ping])

(define-fsm-machine w9-inv
                    #:states (a b c)
                    #:events (start finish)
                    #:transitions [(a -> b) start]
                    [(b -> c) finish])

(define-fsm-machine w9-alpha #:states (x y) #:events (go) #:transitions [(x -> y) go])

(define-fsm-machine w9-beta #:states (x y) #:events (go) #:transitions [(x -> y) go])

(define fsm-suite
  (test-suite "define-fsm-machine expansion safety"

    (test-case "empty transitions list works"
      (check-true (fsm-state? w9-empty-trans-idle))
      (check-false (w9-empty-trans-valid-transition? w9-empty-trans-idle (fsm-event 'noop))
                   "no transitions means no valid transitions"))

    (test-case "single state machine with self-transition"
      (check-true (w9-single-valid-transition? w9-single-only (fsm-event 'ping)))
      (check-false (w9-single-valid-transition? w9-single-only (fsm-event 'pong))))

    (test-case "next-state returns #f for invalid transition"
      (define result (w9-inv-next-state w9-inv-a (fsm-event 'finish)))
      (check-false result "a + finish is not a valid transition"))

    (test-case "next-state returns correct state singleton"
      (define result (w9-inv-next-state w9-inv-a (fsm-event 'start)))
      (check-not-false result "a + start should produce a state")
      (when result
        (check-equal? (fsm-state-name result) 'b)))

    (test-case "two machines with same state names don't collide"
      (check-equal? (fsm-state-name w9-alpha-x) 'x)
      (check-equal? (fsm-state-name w9-beta-x) 'x)
      (check-true (w9-alpha-valid-transition? w9-alpha-x (fsm-event 'go)))
      (check-true (w9-beta-valid-transition? w9-beta-x (fsm-event 'go))))))

;; ============================================================
;; 3. define-tool expansion safety tests
;; ============================================================

(define-tool w9-tool-opts
             #:description "Tool with optional properties"
             #:required ("path")
             #:properties [(path "string" "File path")]
             #:optional [(verbose "boolean" "Verbose output" #f)
                         (count "integer" "Iteration count" 10)]
             (lambda (args exec-ctx) (make-success-result "ok")))

(define-tool w9-tool-minimal
             #:description "Minimal tool"
             #:required ()
             #:properties []
             (lambda (args exec-ctx) (make-success-result "minimal")))

(define-tool w9-tool-v1
             #:description "Version 1"
             #:required ()
             #:properties []
             (lambda (args exec-ctx) (make-success-result "v1")))

(define-tool w9-tool-v2
             #:description "Version 2"
             #:required ()
             #:properties []
             (lambda (args exec-ctx) (make-success-result "v2")))

(define-tool w9-tool-multi-req
             #:description "Multi-required tool"
             #:required ("input" "mode" "level")
             #:properties [(input "string" "Input data")
                           (mode "string" "Processing mode")
                           (level "integer" "Verbosity level")]
             (lambda (args exec-ctx) (make-success-result "multi")))

(define tool-suite
  (test-suite "define-tool expansion safety"

    (test-case "optional properties appear in schema with defaults"
      (define schema (tool-schema w9-tool-opts))
      (define props (hash-ref schema 'properties))
      (check-true (hash-has-key? props 'verbose))
      (check-true (hash-has-key? props 'count))
      (check-equal? (hash-ref (hash-ref props 'verbose) 'default) #f)
      (check-equal? (hash-ref (hash-ref props 'count) 'default) 10))

    (test-case "minimal tool with no properties"
      (check-equal? (tool-name w9-tool-minimal) "w9-tool-minimal")
      (define schema (tool-schema w9-tool-minimal))
      (check-equal? (hash-ref schema 'required) '())
      (check-equal? (hash-count (hash-ref schema 'properties)) 0))

    (test-case "similar-named tools don't collide"
      (check-equal? (tool-name w9-tool-v1) "w9-tool-v1")
      (check-equal? (tool-name w9-tool-v2) "w9-tool-v2")
      (check-equal? (tool-result-content ((tool-execute w9-tool-v1) (hasheq) #f)) "v1")
      (check-equal? (tool-result-content ((tool-execute w9-tool-v2) (hasheq) #f)) "v2"))

    (test-case "multiple required fields in schema"
      (define schema (tool-schema w9-tool-multi-req))
      (check-equal? (hash-ref schema 'required) '("input" "mode" "level")))))

;; ============================================================
;; Run all tests
;; ============================================================

(module+ main
  (run-tests typed-event-suite)
  (run-tests fsm-suite)
  (run-tests tool-suite))

(module+ test
  (run-tests typed-event-suite)
  (run-tests fsm-suite)
  (run-tests tool-suite))
