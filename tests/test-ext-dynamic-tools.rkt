#lang racket

;; tests/test-ext-dynamic-tools.rkt — FEAT-62: dynamic tool registration from extensions
;;
;; Covers:
;;   - ext-register-tool! registers a tool via extension-ctx
;;   - ext-unregister-tool! removes a dynamically registered tool
;;   - ext-list-dynamic-tools lists registered tool names
;;   - Error when tool-registry is #f in context
;;   - Integration: registered tool appears in list-tools-jsexpr

(require rackunit
         "../extensions/context.rkt"
         "../extensions/dynamic-tools.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../tools/tool.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; ext-register-tool!
;; ============================================================

(test-case "ext-register-tool! registers a tool via extension-ctx"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "s-dyn"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg))
  (ext-register-tool! ctx
                      "my_tool"
                      "A dynamic tool"
                      (hasheq 'type "object" 'properties (hasheq 'input (hasheq 'type "string")))
                      (lambda (args) (make-success-result "ok")))
  (check-not-false (lookup-tool reg "my_tool"))
  (check-equal? (tool-name (lookup-tool reg "my_tool")) "my_tool"))

(test-case "ext-register-tool! tool is executable"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "s-exec"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg))
  (ext-register-tool!
   ctx
   "adder"
   "Adds numbers"
   (hasheq 'type "object" 'properties (hasheq 'a (hasheq 'type "number") 'b (hasheq 'type "number")))
   (lambda (args)
     (make-success-result (list (hasheq 'type
                                        "text"
                                        'text
                                        (format "~a + ~a = ~a"
                                                (hash-ref args 'a 0)
                                                (hash-ref args 'b 0)
                                                (+ (hash-ref args 'a 0) (hash-ref args 'b 0))))))))
  (define t (lookup-tool reg "adder"))
  (define result ((tool-execute t) (hasheq 'a 3 'b 4)))
  (check-false (tool-result-is-error? result)))

(test-case "ext-register-tool! errors when tool-registry is #f"
  (define ctx
    (make-extension-ctx #:session-id "s-no-reg"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-exn exn:fail?
             (lambda ()
               (ext-register-tool! ctx
                                   "bad_tool"
                                   "desc"
                                   (hasheq 'type "object")
                                   (lambda (args) (make-success-result "x"))))))

(test-case "ext-register-tool! errors on duplicate name"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "s-dup"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg))
  (ext-register-tool! ctx
                      "dup"
                      "first"
                      (hasheq 'type "object")
                      (lambda (args) (make-success-result "1")))
  (check-exn exn:fail?
             (lambda ()
               (ext-register-tool! ctx
                                   "dup"
                                   "second"
                                   (hasheq 'type "object")
                                   (lambda (args) (make-success-result "2"))))))

;; ============================================================
;; ext-unregister-tool!
;; ============================================================

(test-case "ext-unregister-tool! removes a dynamically registered tool"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "s-unreg"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg))
  (ext-register-tool! ctx
                      "temp"
                      "Temporary"
                      (hasheq 'type "object")
                      (lambda (args) (make-success-result "temp")))
  (check-not-false (lookup-tool reg "temp"))
  (ext-unregister-tool! ctx "temp")
  (check-false (lookup-tool reg "temp")))

(test-case "ext-unregister-tool! errors when tool-registry is #f"
  (define ctx
    (make-extension-ctx #:session-id "s-no-reg2"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-exn exn:fail? (lambda () (ext-unregister-tool! ctx "anything"))))

;; ============================================================
;; ext-list-dynamic-tools
;; ============================================================

(test-case "ext-list-dynamic-tools returns tool names"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "s-list"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg))
  (ext-register-tool! ctx
                      "tool_a"
                      "A"
                      (hasheq 'type "object")
                      (lambda (args) (make-success-result "a")))
  (ext-register-tool! ctx
                      "tool_b"
                      "B"
                      (hasheq 'type "object")
                      (lambda (args) (make-success-result "b")))
  (define names (ext-list-dynamic-tools ctx))
  (check-equal? (length names) 2)
  (check-not-false (member "tool_a" names))
  (check-not-false (member "tool_b" names)))

(test-case "ext-list-dynamic-tools returns empty when no registry"
  (define ctx
    (make-extension-ctx #:session-id "s-empty"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-equal? (ext-list-dynamic-tools ctx) '()))

;; ============================================================
;; Integration: dynamic tool appears in list-tools-jsexpr
;; ============================================================

(test-case "dynamically registered tool appears in list-tools-jsexpr"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "s-jsexpr"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg))
  (ext-register-tool!
   ctx
   "ext_tool"
   "Extension tool"
   (hasheq 'type "object" 'properties (hasheq 'path (hasheq 'type "string")) 'required '("path"))
   (lambda (args) (make-success-result "done")))
  (define tools (list-tools-jsexpr reg))
  (check-equal? (length tools) 1)
  (define fn (hash-ref (car tools) 'function))
  (check-equal? (hash-ref fn 'name) "ext_tool")
  (check-equal? (hash-ref fn 'description) "Extension tool"))
