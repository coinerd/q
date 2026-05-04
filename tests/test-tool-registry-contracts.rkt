#lang racket/base

;; tests/test-tool-registry-contracts.rkt — Contract enforcement tests for tool registry
;;
;; W0 scaffolding for v0.29.0 milestone: Verify that tool registry operations
;; enforce type contracts (reject non-tool values, return #f for unknown, etc.).

(require rackunit
         racket/hash
         (only-in "../tools/tool.rkt"
                  make-tool
                  tool?
                  tool-name
                  tool-registry?
                  make-tool-registry
                  register-tool!
                  unregister-tool!
                  lookup-tool
                  list-tools
                  list-tools-jsexpr
                  tool->jsexpr
                  validate-tool-args
                  tool-names
                  set-active-tools!
                  tool-active?
                  list-active-tools))

;; ── Helpers ──

(define (dummy-execute args ctx)
  (hash 'result "ok"))

(define (make-dummy-tool [name "test-tool"])
  (make-tool name "A test tool" (hasheq 'type "object") dummy-execute))

;; ── register-tool! contract enforcement ──

(test-case "register-tool!-rejects-non-tool-registry"
  (define reg (make-tool-registry))
  (define t (make-dummy-tool))
  ;; Should reject when first arg is not a tool-registry
  (check-exn exn:fail:contract? (lambda () (register-tool! "not-a-registry" t))))

(test-case "register-tool!-rejects-non-tool-value"
  (define reg (make-tool-registry))
  ;; Should reject when second arg is not a tool?
  (check-exn exn:fail:contract? (lambda () (register-tool! reg "not-a-tool"))))

(test-case "register-tool!-rejects-number"
  (define reg (make-tool-registry))
  (check-exn exn:fail:contract? (lambda () (register-tool! reg 42))))

(test-case "register-tool!-accepts-valid-tool"
  (define reg (make-tool-registry))
  (define t (make-dummy-tool))
  (check-not-exn (lambda () (register-tool! reg t)))
  (check-equal? (length (list-tools reg)) 1))

;; ── lookup-tool contract enforcement ──

(test-case "lookup-tool-returns-false-for-unknown"
  (define reg (make-tool-registry))
  (check-false (lookup-tool reg "nonexistent")))

(test-case "lookup-tool-returns-tool-for-known"
  (define reg (make-tool-registry))
  (define t (make-dummy-tool "findme"))
  (register-tool! reg t)
  (define found (lookup-tool reg "findme"))
  (check-true (tool? found))
  (check-equal? (tool-name found) "findme"))

(test-case "lookup-tool-rejects-non-registry"
  (check-exn exn:fail:contract? (lambda () (lookup-tool 42 "foo"))))

;; ── unregister-tool! contract enforcement ──

(test-case "unregister-tool!-removes-tool"
  (define reg (make-tool-registry))
  (register-tool! reg (make-dummy-tool "remove-me"))
  (check-equal? (length (list-tools reg)) 1)
  (unregister-tool! reg "remove-me")
  (check-equal? (length (list-tools reg)) 0))

;; ── list-tools contract enforcement ──

(test-case "list-tools-returns-list-of-tools"
  (define reg (make-tool-registry))
  (register-tool! reg (make-dummy-tool "a"))
  (register-tool! reg (make-dummy-tool "b"))
  (define tools (list-tools reg))
  (check-true (andmap tool? tools))
  (check-equal? (length tools) 2))

(test-case "list-tools-rejects-non-registry"
  (check-exn exn:fail:contract? (lambda () (list-tools "not-a-registry"))))

;; ── tool->jsexpr ──

(test-case "tool->jsexpr-returns-valid-hash"
  (define t (make-dummy-tool))
  (define j (tool->jsexpr t))
  (check-true (hash? j))
  (check-equal? (hash-ref j 'type) "function")
  (check-equal? (hash-ref (hash-ref j 'function) 'name) "test-tool"))

;; ── validate-tool-args ──

(test-case "validate-tool-args-accepts-valid-args"
  (define t (make-dummy-tool))
  (check-true (validate-tool-args t (hasheq))))

(test-case "validate-tool-args-rejects-non-hash"
  (define t (make-dummy-tool))
  (check-exn exn:fail? (lambda () (validate-tool-args t "not-a-hash"))))

;; ── make-tool contract enforcement ──

(test-case "make-tool-rejects-bad-name"
  (check-exn exn:fail:contract? (lambda () (make-tool 123 "desc" (hasheq) dummy-execute))))

(test-case "make-tool-rejects-bad-description"
  (check-exn exn:fail:contract? (lambda () (make-tool "name" 123 (hasheq) dummy-execute))))

(test-case "make-tool-rejects-bad-schema"
  (check-exn exn:fail:contract? (lambda () (make-tool "name" "desc" "not-a-hash" dummy-execute))))

(test-case "make-tool-rejects-bad-execute"
  (check-exn exn:fail:contract? (lambda () (make-tool "name" "desc" (hasheq) "not-a-procedure"))))

;; ── Active tools ──

(test-case "set-active-tools!-filters-list"
  (define reg (make-tool-registry))
  (register-tool! reg (make-dummy-tool "active"))
  (register-tool! reg (make-dummy-tool "inactive"))
  (set-active-tools! reg '("active"))
  (define active (list-active-tools reg))
  (check-equal? (length active) 1)
  (check-equal? (tool-name (car active)) "active"))

(test-case "tool-active?-returns-correct-status"
  (define reg (make-tool-registry))
  (register-tool! reg (make-dummy-tool "on"))
  (register-tool! reg (make-dummy-tool "off"))
  (set-active-tools! reg '("on"))
  (check-true (tool-active? reg "on"))
  (check-false (tool-active? reg "off")))
