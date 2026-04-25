#lang racket/base

;; TEST-03: ext-register-tool! wraps 1-arg handlers to accept (args exec-ctx)

(require rackunit
         (only-in "../extensions/dynamic-tools.rkt" ext-register-tool!)
         (only-in "../extensions/context.rkt" make-extension-ctx)
         (only-in "../tools/tool.rkt" make-tool-registry lookup-tool tool-execute tool?)
         (only-in "../agent/event-bus.rkt" make-event-bus))

(define (make-test-ctx reg)
  (make-extension-ctx #:session-id "test-session"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry #f
                      #:tool-registry reg))

(test-case "1-arg handler is wrapped to accept (args exec-ctx)"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx reg))
  (ext-register-tool! ctx "test-1arg" "desc" (hasheq)
                      (lambda (args) "result-from-1arg"))
  (define t (lookup-tool reg "test-1arg"))
  (check-true (tool? t))
  ;; The wrapped handler should accept 2 args (args + exec-ctx)
  (check-equal? ((tool-execute t) (hasheq) 'fake-exec-ctx) "result-from-1arg"))

(test-case "handler receives correct args map"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx reg))
  (ext-register-tool! ctx "test-args" "desc" (hasheq)
                      (lambda (args) (hash-ref args 'x)))
  (define t (lookup-tool reg "test-args"))
  (check-equal? ((tool-execute t) (hasheq 'x 42) 'fake-ctx) 42))
