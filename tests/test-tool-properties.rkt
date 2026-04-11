#lang racket

;;; tests/test-tool-properties.rkt — Property-based Tests für Tool-Verträge
;;;
;;; Handgeschriebene Property-Tests die Tool-Verträge (invariants) prüfen:
;;;   1. tool-make-roundtrip          — Accessoren spiegeln make-tool Argumente
;;;   2. tool-registry-register-lookup — Registriertes Tool ist auffindbar
;;;   3. tool-registry-list            — list-tools enthält alle registrierten Tools
;;;   4. tool-validation-valid-args    — Korrekte Args validieren zu #t
;;;   5. tool-validation-invalid-args  — Falsche Typen werfen exn:fail
;;;   6. tool-validation-missing-required — Fehlende Pflichtfelder werfen exn:fail
;;;   7. tool-result-success           — make-success-result → is-error? = #f
;;;   8. tool-result-error             — make-error-result   → is-error? = #t
;;;   9. tool-call-idempotent          — Gleiche Argumente → gleiche Struktur
;;;  10. exec-context-default          — Default-Kontext ist gültig
;;;  11. registry-unregister           — Nach unregister ist Tool weg
;;;  12. schema-validation-complex     — Nested objects validieren korrekt

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt")

;; ── Helpers ──────────────────────────────────────────────────

(define (simple-schema [required '()])
  (hasheq 'type "object"
          'properties (hasheq 'path (hasheq 'type "string")
                              'count (hasheq 'type "integer"))
          'required required))

(define (make-dummy-tool [name "test_tool"])
  (make-tool name
             "A test tool"
             (simple-schema '("path"))
             (lambda (args ctx)
               (make-success-result '()))))

;; ── Test Suite ───────────────────────────────────────────────

(define test-tool-properties
  (test-suite
   "Tool Contract Property Tests"

   ;; 1 ────────────────────────────────────────────────────────
   (test-case "tool-make-roundtrip"
     (define schema (hasheq 'type "object"
                            'properties (hasheq 'x (hasheq 'type "integer"))
                            'required '("x")))
     (define exec (lambda (args ctx) 'done))
     (define t (make-tool "roundtrip_tool" "roundtrip desc" schema exec))
     (check-equal? (tool-name t) "roundtrip_tool")
     (check-equal? (tool-description t) "roundtrip desc")
     (check-equal? (tool-schema t) schema)
     (check-equal? (tool-execute t) exec)
     (check-true (tool? t)))

   ;; 2 ────────────────────────────────────────────────────────
   (test-case "tool-registry-register-lookup"
     (define reg (make-tool-registry))
     (define t1 (make-dummy-tool "lookup_a"))
     (define t2 (make-dummy-tool "lookup_b"))
     (register-tool! reg t1)
     (register-tool! reg t2)
     (check-equal? (tool-name (lookup-tool reg "lookup_a")) "lookup_a")
     (check-equal? (tool-name (lookup-tool reg "lookup_b")) "lookup_b")
     ;; Unknown tool → #f
     (check-false (lookup-tool reg "nonexistent")))

   ;; 3 ────────────────────────────────────────────────────────
   (test-case "tool-registry-list"
     (define reg (make-tool-registry))
     (define t1 (make-dummy-tool "list_a"))
     (define t2 (make-dummy-tool "list_b"))
     (define t3 (make-dummy-tool "list_c"))
     (register-tool! reg t1)
     (register-tool! reg t2)
     (register-tool! reg t3)
     (define names (sort (map tool-name (list-tools reg)) string<?))
     (check-equal? names '("list_a" "list_b" "list_c")))

   ;; 4 ────────────────────────────────────────────────────────
   (test-case "tool-validation-valid-args"
     (define t (make-tool "v" "v"
                          (hasheq 'type "object"
                                  'properties (hasheq 'name (hasheq 'type "string")
                                                      'age  (hasheq 'type "integer"))
                                  'required '("name"))
                          void))
     ;; All correct
     (check-true (validate-tool-args t (hasheq 'name "Alice" 'age 30)))
     ;; Optional field omitted
     (check-true (validate-tool-args t (hasheq 'name "Bob")))
     ;; Extra field is fine
     (check-true (validate-tool-args t (hasheq 'name "Eve" 'extra 'ok))))

   ;; 5 ────────────────────────────────────────────────────────
   (test-case "tool-validation-invalid-args"
     (define t (make-tool "iv" "iv"
                          (hasheq 'type "object"
                                  'properties (hasheq 'path  (hasheq 'type "string")
                                                      'count (hasheq 'type "integer"))
                                  'required '("path"))
                          void))
     ;; path expects string, got integer
     (check-exn exn:fail?
                (lambda () (validate-tool-args t (hasheq 'path 42 'count 1))))
     ;; count expects integer, got string
     (check-exn exn:fail?
                (lambda () (validate-tool-args t (hasheq 'path "/x" 'count "five"))))
     ;; boolean where string expected
     (check-exn exn:fail?
                (lambda () (validate-tool-args t (hasheq 'path #t)))))

   ;; 6 ────────────────────────────────────────────────────────
   (test-case "tool-validation-missing-required"
     (define t (make-tool "mr" "mr"
                          (hasheq 'type "object"
                                  'properties (hasheq 'a (hasheq 'type "string")
                                                      'b (hasheq 'type "integer"))
                                  'required '("a" "b"))
                          void))
     ;; Both missing
     (check-exn exn:fail?
                (lambda () (validate-tool-args t (hasheq))))
     ;; One missing
     (check-exn exn:fail?
                (lambda () (validate-tool-args t (hasheq 'a "ok"))))
     ;; Both present → passes
     (check-true (validate-tool-args t (hasheq 'a "ok" 'b 1))))

   ;; 7 ────────────────────────────────────────────────────────
   (test-case "tool-result-success"
     (define content (list (hasheq 'type "text" 'text "all good")))
     (define r (make-success-result content))
     (check-true  (tool-result? r))
     (check-false (tool-result-is-error? r))
     (check-equal? (tool-result-content r) content)
     (check-equal? (tool-result-details r) (hasheq))
     ;; With explicit details
     (define r2 (make-success-result content (hasheq 'bytesRead 99)))
     (check-equal? (tool-result-details r2) (hasheq 'bytesRead 99))
     (check-false (tool-result-is-error? r2)))

   ;; 8 ────────────────────────────────────────────────────────
   (test-case "tool-result-error"
     (define r (make-error-result "something failed"))
     (check-true  (tool-result? r))
     (check-true  (tool-result-is-error? r))
     ;; Error content is a non-empty list containing the message
     (check-true (and (list? (tool-result-content r))
                      (positive? (length (tool-result-content r)))))
     (define text-entry (list-ref (tool-result-content r) 0))
     (check-equal? (hash-ref text-entry 'type) "text")
     (check-equal? (hash-ref text-entry 'text) "something failed"))

   ;; 9 ────────────────────────────────────────────────────────
   (test-case "tool-call-idempotent"
     (define args (hasheq 'path "/tmp/x" 'line 10))
     (define c1 (make-tool-call "id-42" "read" args))
     (define c2 (make-tool-call "id-42" "read" args))
     (check-equal? (tool-call-id c1)        (tool-call-id c2))
     (check-equal? (tool-call-name c1)      (tool-call-name c2))
     (check-equal? (tool-call-arguments c1) (tool-call-arguments c2))
     (check-pred tool-call? c1)
     (check-true (tool-call? c2)))

   ;; 10 ───────────────────────────────────────────────────────
   (test-case "exec-context-default"
     (define ctx (make-exec-context))
     (check-pred exec-context? ctx)
     (check-true (path-string? (exec-context-working-directory ctx)))
     (check-false (exec-context-cancellation-token ctx))
     (check-false (exec-context-event-publisher ctx))
     (check-false (exec-context-runtime-settings ctx))
     (check-equal? (exec-context-call-id ctx) "")
     (check-false (exec-context-session-metadata ctx)))

   ;; 11 ───────────────────────────────────────────────────────
   (test-case "registry-unregister"
     (define reg (make-tool-registry))
     (define t1 (make-dummy-tool "unreg_a"))
     (define t2 (make-dummy-tool "unreg_b"))
     (register-tool! reg t1)
     (register-tool! reg t2)
     (check-equal? (length (list-tools reg)) 2)
     (unregister-tool! reg "unreg_a")
     ;; t1 is gone
     (check-false (lookup-tool reg "unreg_a"))
     ;; t2 remains
     (check-equal? (tool-name (lookup-tool reg "unreg_b")) "unreg_b")
     ;; list-tools no longer contains unreg_a
     (check-equal? (length (list-tools reg)) 1)
     ;; unregister non-existent is a no-op
     (unregister-tool! reg "no_such_tool")
     (check-equal? (length (list-tools reg)) 1))

   ;; 12 ───────────────────────────────────────────────────────
   (test-case "schema-validation-complex"
     ;; Nested object: config.nested.deep must be integer,
     ;; config.nested.flag must be boolean, items must be array
     (define nested-schema
       (hasheq 'type "object"
               'properties
               (hasheq 'config (hasheq 'type "object"
                                       'properties
                                       (hasheq 'nested (hasheq 'type "object"
                                                               'properties
                                                               (hasheq 'deep (hasheq 'type "integer")
                                                                       'flag (hasheq 'type "boolean")))))
                       'items (hasheq 'type "array")
                       'name  (hasheq 'type "string"))
               'required '("name")))
     (define ct (make-tool "complex" "complex tool" nested-schema void))

     ;; Valid nested args
     (check-true
      (validate-tool-args ct
                          (hasheq 'name   "test"
                                  'config (hasheq 'nested (hasheq 'deep 42 'flag #t))
                                  'items  (list "a" "b" "c"))))

     ;; Missing top-level required 'name'
     (check-exn exn:fail?
                (lambda ()
                  (validate-tool-args ct
                                      (hasheq 'config (hasheq) 'items '()))))

     ;; Wrong type for items (should be array, got integer)
     (check-exn exn:fail?
                (lambda ()
                  (validate-tool-args ct
                                      (hasheq 'name "ok" 'items 99))))

     ;; config is not an object (should be hash)
     (check-exn exn:fail?
                (lambda ()
                  (validate-tool-args ct
                                      (hasheq 'name "ok" 'config "not-an-object"))))

     ;; Optional nested fields omitted → still valid
     (check-true
      (validate-tool-args ct (hasheq 'name "minimal"))))

   )) ;; end test-suite

;; ── Run ──────────────────────────────────────────────────────

(run-tests test-tool-properties)
