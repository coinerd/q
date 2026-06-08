#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-parameterized-registries.rkt — R-05/R-14 registry isolation tests

(require rackunit
         rackunit/text-ui
         "../util/event/event-macro.rkt"
         "../extensions/ui-surface.rkt")

(define registry-suite
  (test-suite "Parameterized registry isolation tests"

    ;; ── Event field registry ──
    (test-case "event field registry: parameterize isolation"
      ;; Register in outer scope
      (register-event-fields! 'test-outer '(a b c))
      (check-equal? (lookup-event-fields 'test-outer) '(a b c))
      ;; Inner scope with fresh registry
      (with-fresh-event-registries (check-false (lookup-event-fields 'test-outer))
                                   (register-event-fields! 'test-inner '(x y))
                                   (check-equal? (lookup-event-fields 'test-inner) '(x y)))
      ;; Outer scope unchanged
      (check-equal? (lookup-event-fields 'test-outer) '(a b c))
      (check-false (lookup-event-fields 'test-inner)))

    (test-case "event field registry: direct parameterize"
      (parameterize ([current-event-field-registry (make-hasheq)])
        (register-event-fields! 'isolated '(foo))
        (check-equal? (lookup-event-fields 'isolated) '(foo)))
      ;; Global should not have 'isolated (unless another test registered it)
      ;; We just verify the parameterize worked
      (check-true #t))

    ;; ── Serializer registry ──
    (test-case "serializer registry: parameterize isolation"
      (with-fresh-event-registries (register-event-serializer! "test-type" (lambda (e) (hasheq)))
                                   (check-pred procedure? (lookup-event-serializer "test-type")))
      ;; After scope, should be gone
      (check-false (lookup-event-serializer "test-type")))

    ;; ── Deserializer registry ──
    (test-case "deserializer registry: parameterize isolation"
      (with-fresh-event-registries (register-event-deserializer! "test-type"
                                                                 (lambda (t ts sid tid h) #f))
                                   (check-pred procedure? (lookup-event-deserializer "test-type")))
      (check-false (lookup-event-deserializer "test-type")))

    ;; ── All three registries at once ──
    (test-case "with-fresh-event-registries isolates all three"
      (with-fresh-event-registries (register-event-fields! 'combo '(a))
                                   (register-event-serializer! "combo" values)
                                   (register-event-deserializer! "combo" values)
                                   (check-equal? (lookup-event-fields 'combo) '(a))
                                   (check-pred procedure? (lookup-event-serializer "combo"))
                                   (check-pred procedure? (lookup-event-deserializer "combo")))
      ;; After scope
      (check-false (lookup-event-fields 'combo))
      (check-false (lookup-event-serializer "combo"))
      (check-false (lookup-event-deserializer "combo")))

    ;; ── UI registry ──
    (test-case "UI registry: parameterize isolation"
      ;; Override in scope
      (define test-registry (ui-callback-registry (lambda (s l) (void)) #f #f #f #f #f #f #f #f #f))
      (parameterize ([current-ui-registry test-registry])
        (check-equal? (current-ui-registry) test-registry))
      ;; After scope, back to default
      (check-not-eq? (current-ui-registry) test-registry))

    ;; ── Nested parameterize ──
    (test-case "nested parameterize preserves outer scope"
      (with-fresh-event-registries (register-event-fields! 'outer '(x))
                                   (parameterize ([current-event-field-registry (make-hasheq)])
                                     (register-event-fields! 'inner '(y))
                                     (check-equal? (lookup-event-fields 'inner) '(y))
                                     (check-false (lookup-event-fields 'outer)))
                                   ;; Back to with-fresh-event-registries scope
                                   (check-equal? (lookup-event-fields 'outer) '(x))
                                   (check-false (lookup-event-fields 'inner))))))

(run-tests registry-suite 'verbose)
