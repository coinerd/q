#lang racket

(require rackunit
         "../tools/tool.rkt")

;; ============================================================
;; Tool struct
;; ============================================================

(test-case "make-tool creates a valid tool"
  (define t (make-tool "test"
                       "A test tool"
                       (hasheq 'type "object")
                       (λ (args) 'ok)))
  (check-pred tool? t)
  (check-equal? (tool-name t) "test")
  (check-equal? (tool-description t) "A test tool")
  (check-equal? (tool-schema t) (hasheq 'type "object")))

(test-case "make-tool rejects bad name"
  (check-exn exn:fail?
    (λ () (make-tool 123 "desc" (hasheq) (λ (_) 'ok)))))

(test-case "make-tool rejects bad description"
  (check-exn exn:fail?
    (λ () (make-tool "name" 42 (hasheq) (λ (_) 'ok)))))

(test-case "make-tool rejects bad schema"
  (check-exn exn:fail?
    (λ () (make-tool "name" "desc" "not-a-hash" (λ (_) 'ok)))))

(test-case "make-tool rejects bad execute"
  (check-exn exn:fail?
    (λ () (make-tool "name" "desc" (hasheq) "not-a-proc"))))

(test-case "tool execute can be called"
  (define t (make-tool "add" "addition"
                       (hasheq)
                       (λ (args) (+ (hash-ref args 'a) (hash-ref args 'b)))))
  (check-equal? ((tool-execute t) (hasheq 'a 3 'b 4)) 7))

;; ============================================================
;; Tool result
;; ============================================================

(test-case "make-tool-result creates correct struct"
  (define tr (make-tool-result "hello" (hasheq) #f))
  (check-pred tool-result? tr)
  (check-equal? (tool-result-content tr) "hello")
  (check-false (tool-result-is-error? tr)))

(test-case "make-error-result has is-error #t"
  (define tr (make-error-result "something broke"))
  (check-pred tool-result-is-error? tr)
  (check-equal? (tool-result-content tr)
                (list (hasheq 'type "text" 'text "something broke"))))

(test-case "make-success-result defaults details to empty hash"
  (define tr (make-success-result "ok"))
  (check-false (tool-result-is-error? tr))
  (check-equal? (tool-result-details tr) (hasheq)))

(test-case "make-success-result with explicit details"
  (define tr (make-success-result "ok" (hasheq 'bytes 42)))
  (check-equal? (tool-result-details tr) (hasheq 'bytes 42)))

;; ============================================================
;; Tool result serialization round-trip
;; ============================================================

(test-case "tool-result->jsexpr and jsexpr->tool-result round-trip"
  (define original (make-tool-result '("data") (hasheq 'key "val") #t))
  (define json (tool-result->jsexpr original))
  (define restored (jsexpr->tool-result json))
  (check-equal? (tool-result-content restored) '("data"))
  (check-equal? (tool-result-is-error? restored) #t))

;; ============================================================
;; Execution context
;; ============================================================

(test-case "make-exec-context with defaults"
  (define ctx (make-exec-context))
  (check-pred exec-context? ctx)
  (check-equal? (exec-context-call-id ctx) ""))

(test-case "make-exec-context with all keywords"
  (define ctx (make-exec-context #:working-directory "/tmp"
                                 #:call-id "call-123"
                                 #:session-metadata (hasheq 'key "val")))
  (check-equal? (exec-context-call-id ctx) "call-123"))

;; ============================================================
;; Tool registry
;; ============================================================

(test-case "make-tool-registry is empty"
  (define reg (make-tool-registry))
  (check-pred tool-registry? reg)
  (check-equal? (tool-names reg) '())
  (check-equal? (list-tools reg) '()))

(test-case "register-tool! and lookup-tool"
  (define reg (make-tool-registry))
  (define t (make-tool "read" "Read a file" (hasheq) (λ (_) 'ok)))
  (register-tool! reg t)
  (check-equal? (lookup-tool reg "read") t)
  (check-false (lookup-tool reg "write")))

(test-case "register-tool! rejects duplicates"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "x" "first" (hasheq) (λ (_) 'ok)))
  (check-exn exn:fail?
    (λ () (register-tool! reg (make-tool "x" "second" (hasheq) (λ (_) 'ok))))))

(test-case "register-tool! rejects non-tool"
  (define reg (make-tool-registry))
  (check-exn exn:fail?
    (λ () (register-tool! reg "not-a-tool"))))

(test-case "unregister-tool! removes a tool"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "rm-me" "desc" (hasheq) (λ (_) 'ok)))
  (check-not-false (lookup-tool reg "rm-me"))
  (unregister-tool! reg "rm-me")
  (check-false (lookup-tool reg "rm-me")))

(test-case "tool-names returns all registered names"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "a" "a" (hasheq) (λ (_) 'ok)))
  (register-tool! reg (make-tool "b" "b" (hasheq) (λ (_) 'ok)))
  (check equal? (sort (tool-names reg) string<?) '("a" "b")))

;; ============================================================
;; Tool-call struct
;; ============================================================

(test-case "make-tool-call creates struct"
  (define tc (make-tool-call "id-1" "read" (hasheq 'path "/tmp")))
  (check-pred tool-call? tc)
  (check-equal? (tool-call-id tc) "id-1")
  (check-equal? (tool-call-name tc) "read"))

;; ============================================================
;; Argument validation
;; ============================================================

(test-case "validate-tool-args passes for valid args"
  (define t (make-tool "echo" "echo"
                       (hasheq 'type "object"
                               'required '(name)
                               'properties (hasheq 'name (hasheq 'type "string")))
                       (λ (_) 'ok)))
  (check-true (validate-tool-args t (hasheq 'name "hello"))))

(test-case "validate-tool-args rejects missing required"
  (define t (make-tool "echo" "echo"
                       (hasheq 'type "object"
                               'required '(name)
                               'properties (hasheq 'name (hasheq 'type "string")))
                       (λ (_) 'ok)))
  (check-exn exn:fail?
    (λ () (validate-tool-args t (hasheq)))))

(test-case "validate-tool-args rejects wrong type"
  (define t (make-tool "echo" "echo"
                       (hasheq 'type "object"
                               'properties (hasheq 'count (hasheq 'type "integer")))
                       (λ (_) 'ok)))
  (check-exn exn:fail?
    (λ () (validate-tool-args t (hasheq 'count "not-int")))))

(test-case "validate-tool-args rejects non-hash args"
  (define t (make-tool "echo" "echo" (hasheq) (λ (_) 'ok)))
  (check-exn exn:fail?
    (λ () (validate-tool-args t "not-a-hash"))))
