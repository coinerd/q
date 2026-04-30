#lang racket

;; tests/test-typed-racket-pilot.rkt — RKT-01: Typed Racket pilot validation
;; v0.22.6 W5
;;
;; Validates that the Typed Racket modules compile, export correctly,
;; and interoperate with untyped consumers.

(require rackunit
         rackunit/text-ui
         "../util/version.rkt"
         "../util/event-payloads.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/plan-validator.rkt")

(define tr-pilot-tests
  (test-suite "Typed Racket Pilot (RKT-01 v0.23.x)"

    ;; -------------------------------------------------------
    ;; util/version.rkt — Typed Racket
    ;; -------------------------------------------------------
    (test-case "q-version is a string"
      (check-pred string? q-version)
      (check-true (regexp-match? #rx"^0\\.23\\." q-version)))

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
      (check-exn exn:fail:contract? (lambda () (session-start-payload 123 #f 'new))))

    (test-case "TR boundary contracts enforce symbol types for reason"
      (check-exn exn:fail:contract? (lambda () (session-start-payload "s1" #f "not-a-symbol"))))

    (test-case "struct transparency works"
      (check-pred session-start-payload? (session-start-payload "s" #f 'new))
      (check-pred session-end-payload? (session-end-payload "s" 1))
      (check-pred gsd-mode-payload? (gsd-mode-payload 'a 'b)))

    ;; -------------------------------------------------------
    ;; extensions/gsd/plan-types.rkt — Typed Racket (v0.22.8)
    ;; -------------------------------------------------------
    (test-case "plan-types: gsd-task construction from untyped context"
      (define t (make-gsd-task "test task" '("file1.rkt") "action" "verify" ""))
      (check-pred gsd-task? t)
      (check-equal? (gsd-task-name t) "test task")
      (check-equal? (gsd-task-files t) '("file1.rkt"))
      (check-equal? (gsd-task-status t) 'pending))

    (test-case "plan-types: gsd-wave and gsd-plan construction"
      (define w (make-gsd-wave 0 "Test Wave" "root cause" '("f.rkt") '() "verify" '("done")))
      (check-pred gsd-wave? w)
      (check-equal? (gsd-wave-index w) 0)
      (check-equal? (gsd-wave-title w) "Test Wave")
      (check-equal? (gsd-wave-status w) 'pending)
      (define p (gsd-plan (list w) #f '() '()))
      (check-pred gsd-plan? p)
      (check-equal? (length (gsd-plan-waves p)) 1))

    (test-case "plan-types: TR boundary contract enforcement"
      ;; gsd-task expects String for name field.
      ;; Passing wrong type from untyped should raise contract error.
      (check-exn exn:fail:contract? (lambda () (gsd-task 123 '() "action" "verify" "" 'pending))))

    ;; -------------------------------------------------------
    ;; extensions/gsd/plan-validator.rkt — Typed Racket (v0.22.8)
    ;; -------------------------------------------------------
    (test-case "validate-plan-strict returns validation-result"
      (define w (make-gsd-wave 0 "Wave 0" "cause" '("f.rkt") '() "verify" '("done")))
      (define p (gsd-plan (list w) #f '() '()))
      (define result (validate-plan-strict p))
      (check-pred validation-result? result)
      (check-true (validation-valid? result)))

    (test-case "format-validation-report produces non-empty string"
      (define w (make-gsd-wave 0 "Wave 0" "cause" '("f.rkt") '() "verify" '()))
      (define p (gsd-plan (list w) #f '() '()))
      (define result (validate-plan-strict p))
      (define report (format-validation-report result))
      (check-pred string? report)
      (check-true (> (string-length report) 0)))))

(run-tests tr-pilot-tests)
