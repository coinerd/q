#lang racket

;; BOUNDARY: integration

;; tests/test-event-schema-version.rkt -- W0: Event schema versioning tests
;;
;; Tests S8-F1: schemaVersion in serialized events + forward compat

(require rackunit
         rackunit/text-ui
         "../agent/event-json.rkt"
         "../util/event-macro.rkt"
         "../agent/event-structs/base.rkt")

(define schema-suite
  (test-suite "event schema versioning"

    ;; -- typed-event->jsexpr includes schemaVersion --
    (test-case "typed-event->jsexpr includes schemaVersion=1"
      (define evt (typed-event "test.type" 12345 "sid-1" "tid-1"))
      (define js (typed-event->jsexpr evt))
      (check-true (hash-has-key? js 'schemaVersion))
      (check-equal? (hash-ref js 'schemaVersion) 1))

    ;; -- jsexpr->typed-event ignores unknown fields (forward compat) --
    (test-case "jsexpr->typed-event ignores unknown future fields"
      (define h (hasheq 'type "unknown.future"
                        'timestamp 999
                        'sessionId "sid-future"
                        'turnId "tid-future"
                        'schemaVersion 2
                        'futureField "hello"))
      (define evt (jsexpr->typed-event h))
      (check-equal? (typed-event-type evt) "unknown.future")
      (check-equal? (typed-event-session-id evt) "sid-future"))

    ;; -- jsexpr->typed-event works with schemaVersion=1 --
    (test-case "jsexpr->typed-event round-trips schemaVersion=1"
      (define h (hasheq 'type "session.started"
                        'timestamp 1000
                        'sessionId "sid-2"
                        'turnId #f
                        'schemaVersion 1))
      (define evt (jsexpr->typed-event h))
      (check-equal? (typed-event-type evt) "session.started")
      (check-equal? (typed-event-timestamp evt) 1000))

    ;; -- current-schema-version parameter exists and defaults to 1 --
    (test-case "current-schema-version defaults to 1"
      (check-equal? (current-schema-version) 1))

    ;; -- current-schema-version can be parameterized --
    (test-case "current-schema-version can be set via parameterize"
      (parameterize ([current-schema-version 2])
        (check-equal? (current-schema-version) 2)))))

(run-tests schema-suite 'verbose)
