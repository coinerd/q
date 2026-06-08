#lang racket/base

;; @speed fast
;; @suite default
;; BOUNDARY: pure

;; tests/test-schema-versioning.rkt — Per-type schema version tests (F6, S8-F1)

(require rackunit
         rackunit/text-ui
         (only-in "../util/event/event-macro.rkt"
                  define-typed-event
                  with-fresh-event-registries
                  current-event-schema-registry
                  register-event-schema-version!
                  lookup-event-schema-version
                  current-schema-version)
         (only-in "../agent/event-json.rkt" typed-event->jsexpr jsexpr->typed-event))

;; Define test events at module level (provide must be at module level)
(define-typed-event sv-test-event-v1 "sv-test-v1" (data) #:schema-version 1)
(define-typed-event sv-test-event-v3 "sv-test-v3" (payload) #:schema-version 3)
(define-typed-event sv-test-no-ver "sv-test-no-ver" (info))

(define sv-tests
  (test-suite "schema-versioning"

    (test-case "per-type schema version registered"
      (check-equal? (lookup-event-schema-version "sv-test-v1") 1)
      (check-equal? (lookup-event-schema-version "sv-test-v3") 3)
      (check-equal? (lookup-event-schema-version "sv-test-no-ver") 1))

    (test-case "unregistered type falls back to global default"
      (check-equal? (lookup-event-schema-version "unknown-type") 1))

    (test-case "global schema version parameter works"
      (parameterize ([current-schema-version 5])
        (check-equal? (lookup-event-schema-version "unknown-type") 5)))

    (test-case "schema-version round-trips through serialization"
      (define evt (make-sv-test-event-v3 #:session-id "s1" #:turn-id "t1" #:payload "hello"))
      (define json (typed-event->jsexpr evt))
      (check-equal? (hash-ref json 'schemaVersion) 3)
      (define back (jsexpr->typed-event json))
      (check-equal? (sv-test-event-v3-payload back) "hello"))

    (test-case "no-schema-version event uses global default"
      (define evt (make-sv-test-no-ver #:session-id "s1" #:turn-id "t1" #:info "x"))
      (define json (typed-event->jsexpr evt))
      (check-equal? (hash-ref json 'schemaVersion) 1))

    (test-case "deserialization tolerates future schema version"
      (define json
        (hasheq 'type
                "sv-test-v1"
                'timestamp
                1000
                'sessionId
                "s1"
                'turnId
                "t1"
                'schemaVersion
                99
                'data
                "test"))
      ;; Should not error, just log a warning
      (define back (jsexpr->typed-event json))
      (check-equal? (sv-test-event-v1-data back) "test"))

    (test-case "manual schema version registration"
      (register-event-schema-version! "manual-type" 7)
      (check-equal? (lookup-event-schema-version "manual-type") 7))))

(run-tests sv-tests)
