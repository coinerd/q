#lang racket

;; tests/test-typed-racket-pilot.rkt — RKT-01: Typed Racket pilot validation
;; v0.22.6 W5
;;
;; Validates that the Typed Racket modules compile, export correctly,
;; and interoperate with untyped consumers.

(require rackunit
         rackunit/text-ui
         "../util/version.rkt"
         "../util/event-payloads.rkt")

(define tr-pilot-tests
  (test-suite "Typed Racket Pilot (RKT-01 v0.22.6)"

    ;; -------------------------------------------------------
    ;; util/version.rkt — Typed Racket
    ;; -------------------------------------------------------
    (test-case "q-version is a string"
      (check-pred string? q-version)
      (check-true (regexp-match? #rx"^0\\.22\\." q-version)))

    ;; -------------------------------------------------------
    ;; util/event-payloads.rkt — Typed Racket
    ;; -------------------------------------------------------
    (test-case "session-start-payload constructs and accesses"
      (define p (session-start-payload "s1" (hash 'model "gpt") 'new))
      (check-equal? (session-start-payload-session-id p) "s1")
      (check-equal? (session-start-payload-reason p) 'new)
      (check-pred hash? (session-start-payload-config p)))

    (test-case "payload->hash works for all payload types"
      (check-pred hash? (payload->hash (session-start-payload "s1" #f 'new)))
      (check-pred hash? (payload->hash (session-end-payload "s1" 42.0)))
      (check-pred hash? (payload->hash (session-switch-payload "s1" 'resume)))
      (check-pred hash? (payload->hash (tool-call-event-payload "s1" "t1" "read" "c1")))
      (check-pred hash? (payload->hash (session-id-payload "s1")))
      (check-pred hash? (payload->hash (error-payload "oops" 'runtime)))
      (check-pred hash? (payload->hash (input-payload "s1" "hello")))
      (check-pred hash? (payload->hash (gsd-mode-payload 'plan 'implement))))

    (test-case "payload->hash passes through plain hashes"
      (define h (hasheq 'foo 'bar))
      (check-equal? (payload->hash h) h))

    (test-case "payload->hash wraps unknown values"
      (define h (payload->hash 42))
      (check-equal? (hash-ref h 'payload) 42))

    (test-case "payload-session-id extracts from all payload types"
      (check-equal? (payload-session-id (session-start-payload "s1" #f 'new)) "s1")
      (check-equal? (payload-session-id (session-end-payload "s2" 1.0)) "s2")
      (check-equal? (payload-session-id (session-switch-payload "s3" 'pause)) "s3")
      (check-equal? (payload-session-id (tool-call-event-payload "s4" "t" "r" "c")) "s4")
      (check-equal? (payload-session-id (session-id-payload "s5")) "s5")
      (check-equal? (payload-session-id (input-payload "s6" "hi")) "s6")
      (check-equal? (payload-session-id (hasheq 'session-id "s7")) "s7")
      (check-false (payload-session-id 'unknown)))

    (test-case "TR boundary contracts enforce string types for session-id"
      ;; session-start-payload expects String for session-id.
      ;; From untyped code, TR auto-generates contracts that enforce this.
      (check-exn exn:fail:contract? (lambda () (session-start-payload 123 #f 'new))))

    (test-case "TR boundary contracts enforce symbol types for reason"
      ;; session-start-payload expects Symbol for reason.
      (check-exn exn:fail:contract? (lambda () (session-start-payload "s1" #f "not-a-symbol"))))

    (test-case "struct transparency works"
      (check-pred session-start-payload? (session-start-payload "s" #f 'new))
      (check-pred session-end-payload? (session-end-payload "s" 1))
      (check-pred gsd-mode-payload? (gsd-mode-payload 'a 'b)))))

(run-tests tr-pilot-tests)
