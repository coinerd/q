#lang racket

;;; tests/test-tool-registry.rkt — TDD tests for tools/tool.rkt
;;;
;;; Covers:
;;;   - tool struct & helpers
;;;   - tool-result struct & serialization
;;;   - exec_context struct
;;;   - tool registry (register, lookup, list, unregister)
;;;   - argument validation against schema
;;;   - JSON serialization for tool results
;;;   - error helpers

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../tools/tool.rkt")

(define-runtime-path here ".")
(define q-root (simplify-path (build-path here "..")))

;; ============================================================
;; Shared test fixtures
;; ============================================================

(define sample-schema
  (hasheq 'type "object"
          'properties (hasheq 'path (hasheq 'type "string")
                              'line (hasheq 'type "integer"))
          'required '("path")))

(define sample-execute
  (lambda (args ctx)
    (make-success-result (list (hasheq 'type "text" 'text "ok")))))

(define t (make-tool "read_file"
                     "Read a file"
                     sample-schema
                     sample-execute))

(define tool-reg-tests
  (test-suite
   "Tool Registry"

   ;; ============================================================
   ;; 1. Tool struct & helpers
   ;; ============================================================

   (test-case "make-tool is a constructor procedure"
     (check-true (procedure? make-tool)))

   (test-case "make-tool produces a tool with correct fields"
     (check-pred tool? t)
     (check-equal? (tool-name t) "read_file")
     (check-equal? (tool-description t) "Read a file")
     (check-equal? (tool-schema t) sample-schema)
     (check-true (procedure? (tool-execute t))))

   (test-case "make-tool rejects non-string name"
     (check-exn exn:fail?
                (lambda () (make-tool 123 "desc" (hasheq) void))))

   (test-case "make-tool rejects non-string description"
     (check-exn exn:fail?
                (lambda () (make-tool "x" 456 (hasheq) void))))

   (test-case "make-tool rejects non-hash schema"
     (check-exn exn:fail?
                (lambda () (make-tool "x" "d" "not-a-hash" void))))

   (test-case "make-tool rejects non-procedure execute"
     (check-exn exn:fail?
                (lambda () (make-tool "x" "d" (hasheq) "not-a-proc"))))

   ;; ============================================================
   ;; 2. Tool result struct
   ;; ============================================================

   (test-case "make-tool-result is a constructor procedure"
     (check-true (procedure? make-tool-result)))

   (test-case "make-tool-result produces correct fields"
     (define tr (make-tool-result
                 (list (hasheq 'type "text" 'text "hello"))
                 (hasheq 'bytesRead 42)
                 #f))
     (check-pred tool-result? tr)
     (check-equal? (tool-result-content tr)
                   (list (hasheq 'type "text" 'text "hello")))
     (check-equal? (tool-result-details tr) (hasheq 'bytesRead 42))
     (check-false (tool-result-is-error? tr)))

   ;; ============================================================
   ;; 2a. make-error-result / make-success-result helpers
   ;; ============================================================

   (test-case "make-error-result produces an error tool-result"
     (define err (make-error-result "something went wrong"))
     (check-pred tool-result? err)
     (check-pred tool-result-is-error? err)
     (check-true (and (list? (tool-result-content err))
                      (positive? (length (tool-result-content err))))))

   (test-case "make-success-result produces a non-error result with empty details"
     (define ok (make-success-result (list (hasheq 'type "text" 'text "done"))))
     (check-pred tool-result? ok)
     (check-false (tool-result-is-error? ok))
     (check-equal? (tool-result-details ok) (hasheq)))

   (test-case "make-success-result accepts custom details"
     (define ok2 (make-success-result (list (hasheq 'type "text" 'text "done"))
                                       (hasheq 'lines 10)))
     (check-equal? (tool-result-details ok2) (hasheq 'lines 10)))

   ;; ============================================================
   ;; 2b. JSON serialization for tool results
   ;; ============================================================

   (test-case "tool-result->jsexpr produces a hash with correct keys"
     (define tr-for-json (make-tool-result
                          (list (hasheq 'type "text" 'text "file contents here"))
                          (hasheq 'path "/tmp/x.txt" 'lines 5)
                          #f))
     (define tr-jsexpr (tool-result->jsexpr tr-for-json))
     (check-pred hash? tr-jsexpr)
     (check-equal? (hash-ref tr-jsexpr 'isError) #f)
     (check-true (list? (hash-ref tr-jsexpr 'content)))
     (check-true (hash? (hash-ref tr-jsexpr 'details))))

   (test-case "tool-result JSON round-trip preserves data"
     (define tr-for-json (make-tool-result
                          (list (hasheq 'type "text" 'text "file contents here"))
                          (hasheq 'path "/tmp/x.txt" 'lines 5)
                          #f))
     (define tr-jsexpr (tool-result->jsexpr tr-for-json))
     (define tr-round (jsexpr->tool-result tr-jsexpr))
     (check-pred tool-result? tr-round)
     (check-equal? (tool-result-is-error? tr-round) (tool-result-is-error? tr-for-json))
     (check-equal? (hash-ref (list-ref (tool-result-content tr-round) 0) 'text)
                   "file contents here")
     (check-equal? (hash-ref (tool-result-details tr-round) 'path) "/tmp/x.txt"))

   (test-case "error result JSON round-trip preserves isError flag"
     (define err-json (tool-result->jsexpr (make-error-result "oops")))
     (check-equal? (hash-ref err-json 'isError) #t)
     (define err-round (jsexpr->tool-result err-json))
     (check-true (tool-result-is-error? err-round)))

   ;; ============================================================
   ;; 3. Execution context
   ;; ============================================================

   (test-case "make-exec-context is a constructor procedure"
     (check-true (procedure? make-exec-context)))

   (test-case "exec-context with required fields only"
     (define ctx (make-exec-context
                  #:working-directory "/tmp/project"
                  #:call-id "call-123"))
     (check-pred exec-context? ctx)
     (check-equal? (exec-context-working-directory ctx) "/tmp/project")
     (check-equal? (exec-context-call-id ctx) "call-123")
     (check-false (exec-context-cancellation-token ctx))
     (check-false (exec-context-event-publisher ctx))
     (check-false (exec-context-runtime-settings ctx))
     (check-false (exec-context-session-metadata ctx)))

   (test-case "exec-context with all fields populated"
     (define ctx2 (make-exec-context
                   #:working-directory "/tmp/project"
                   #:cancellation-token 'cancel-token
                   #:event-publisher 'pub
                   #:runtime-settings (hasheq 'timeout 30)
                   #:call-id "call-456"
                   #:session-metadata (hasheq 'sessionId "s1")))
     (check-equal? (exec-context-working-directory ctx2) "/tmp/project")
     (check-equal? (exec-context-cancellation-token ctx2) 'cancel-token)
     (check-equal? (exec-context-event-publisher ctx2) 'pub)
     (check-equal? (exec-context-runtime-settings ctx2) (hasheq 'timeout 30))
     (check-equal? (exec-context-call-id ctx2) "call-456")
     (check-equal? (exec-context-session-metadata ctx2) (hasheq 'sessionId "s1")))

   ;; ============================================================
   ;; 4. Tool registry
   ;; ============================================================

   (test-case "make-tool-registry produces a registry"
     (define reg (make-tool-registry))
     (check-pred tool-registry? reg)
     (check-equal? (list-tools reg) '())
     (check-equal? (tool-names reg) '()))

   (test-case "registry: register, lookup, and list tools"
     (define reg (make-tool-registry))
     (register-tool! reg t)
     (check-equal? (length (list-tools reg)) 1)
     (check-equal? (tool-names reg) '("read_file"))
     (check-equal? (tool-name (lookup-tool reg "read_file")) "read_file")
     (check-false (lookup-tool reg "nonexistent")))

   (test-case "registry: duplicate registration raises error"
     (define reg (make-tool-registry))
     (register-tool! reg t)
     (check-exn exn:fail?
                (lambda () (register-tool! reg t))))

   (test-case "registry: register second tool and verify both listed"
     (define reg (make-tool-registry))
     (register-tool! reg t)
     (define t2 (make-tool "write_file"
                           "Write a file"
                           (hasheq 'type "object"
                                   'properties (hasheq 'path (hasheq 'type "string"))
                                   'required '("path"))
                           (lambda (args ctx)
                             (make-success-result '()))))
     (register-tool! reg t2)
     (check-equal? (length (list-tools reg)) 2)
     (check-equal? (sort (tool-names reg) string<?) '("read_file" "write_file")))

   (test-case "registry: unregister tool"
     (define reg (make-tool-registry))
     (register-tool! reg t)
     (define t2 (make-tool "write_file"
                           "Write a file"
                           (hasheq 'type "object"
                                   'properties (hasheq 'path (hasheq 'type "string"))
                                   'required '("path"))
                           (lambda (args ctx)
                             (make-success-result '()))))
     (register-tool! reg t2)
     (unregister-tool! reg "read_file")
     (check-false (lookup-tool reg "read_file"))
     (check-equal? (tool-names reg) '("write_file")))

   (test-case "registry: unregister non-existent is a no-op"
     (define reg (make-tool-registry))
     (unregister-tool! reg "nonexistent"))

   (test-case "registry: re-register after unregister works"
     (define reg (make-tool-registry))
     (register-tool! reg t)
     (unregister-tool! reg "read_file")
     (register-tool! reg t)
     (check-equal? (tool-name (lookup-tool reg "read_file")) "read_file"))

   ;; ============================================================
   ;; 5. Argument validation against schema
   ;; ============================================================

   (test-case "validate-tool-args accepts valid arguments"
     (define validate-schema
       (hasheq 'type "object"
               'properties (hasheq 'path (hasheq 'type "string")
                                   'count (hasheq 'type "integer"))
               'required '("path")))
     (define vt (make-tool "validated" "desc" validate-schema void))
     (check-true (validate-tool-args vt (hasheq 'path "/tmp/x" 'count 5))))

   (test-case "validate-tool-args rejects missing required field"
     (define validate-schema
       (hasheq 'type "object"
               'properties (hasheq 'path (hasheq 'type "string")
                                   'count (hasheq 'type "integer"))
               'required '("path")))
     (define vt (make-tool "validated" "desc" validate-schema void))
     (check-exn exn:fail?
                (lambda () (validate-tool-args vt (hasheq 'count 5)))))

   (test-case "validate-tool-args rejects wrong type for string field"
     (define validate-schema
       (hasheq 'type "object"
               'properties (hasheq 'path (hasheq 'type "string")
                                   'count (hasheq 'type "integer"))
               'required '("path")))
     (define vt (make-tool "validated" "desc" validate-schema void))
     (check-exn exn:fail?
                (lambda () (validate-tool-args vt (hasheq 'path 123)))))

   (test-case "validate-tool-args rejects wrong type for integer field"
     (define validate-schema
       (hasheq 'type "object"
               'properties (hasheq 'path (hasheq 'type "string")
                                   'count (hasheq 'type "integer"))
               'required '("path")))
     (define vt (make-tool "validated" "desc" validate-schema void))
     (check-exn exn:fail?
                (lambda () (validate-tool-args vt (hasheq 'path "/x" 'count "five")))))

   (test-case "validate-tool-args allows extra arguments"
     (define validate-schema
       (hasheq 'type "object"
               'properties (hasheq 'path (hasheq 'type "string")
                                   'count (hasheq 'type "integer"))
               'required '("path")))
     (define vt (make-tool "validated" "desc" validate-schema void))
     (check-true (validate-tool-args vt (hasheq 'path "/x" 'extra "ok"))))

   (test-case "validate-tool-args passes with no required fields"
     (define no-req-schema
       (hasheq 'type "object"
               'properties (hasheq 'name (hasheq 'type "string"))))
     (define nt (make-tool "noreq" "desc" no-req-schema void))
     (check-true (validate-tool-args nt (hasheq)))
     (check-true (validate-tool-args nt (hasheq 'name "test"))))

   (test-case "validate-tool-args passes with empty schema"
     (define empty-schema-tool (make-tool "empty" "desc" (hasheq) void))
     (check-true (validate-tool-args empty-schema-tool (hasheq 'anything "goes"))))

   (test-case "validate-tool-args rejects non-hash arguments"
     (define validate-schema
       (hasheq 'type "object"
               'properties (hasheq 'path (hasheq 'type "string")
                                   'count (hasheq 'type "integer"))
               'required '("path")))
     (define vt (make-tool "validated" "desc" validate-schema void))
     (check-exn exn:fail?
                (lambda () (validate-tool-args vt "not-a-hash"))))

   ;; ============================================================
   ;; 6. Calling execute through the tool
   ;; ============================================================

   (test-case "tool execute returns correct result"
     (define echo-tool
       (make-tool "echo"
                  "Echoes args back"
                  (hasheq 'type "object"
                          'properties (hasheq 'msg (hasheq 'type "string"))
                          'required '("msg"))
                  (lambda (args ctx)
                    (make-success-result
                     (list (hasheq 'type "text"
                                   'text (hash-ref args 'msg)))))))
     (define echo-result ((tool-execute echo-tool)
                          (hasheq 'msg "hello")
                          (make-exec-context
                           #:working-directory "/tmp"
                           #:call-id "echo-1")))
     (check-pred tool-result? echo-result)
     (check-false (tool-result-is-error? echo-result))
     (check-equal? (hash-ref (list-ref (tool-result-content echo-result) 0) 'text)
                   "hello"))
   ))

(run-tests tool-reg-tests)
