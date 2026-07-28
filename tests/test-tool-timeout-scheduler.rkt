#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tool-timeout-scheduler.rkt — v0.70.7 W1

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         "../tools/permission-gate.rkt")

;; v0.99.66: default exec-context is now strict (deny-by-default).  These
;; tests exercise scheduler mechanics on ad-hoc tool names not present in
;; the classification table, so they must opt into a permissive context.
(define permissive-ctx (make-exec-context #:permission-config (make-permissive-permission-config)))

(define-test-suite test-tool-timeout-scheduler
                   (test-case "execute-single injects per-tool timeout into args"
                     (define args-received #f)
                     (define t
                       (make-tool "timed"
                                  "desc"
                                  (hasheq 'type "object" 'properties (hasheq))
                                  (lambda (args ctx)
                                    (set! args-received args)
                                    (make-success-result "ok"))
                                  #:timeout-seconds 42))
                     (define registry (make-tool-registry))
                     (register-tool! registry t)
                     (define tc (tool-call "id-1" "timed" (hasheq 'command "echo hi")))
                     (define result (run-tool-batch (list tc) registry #:exec-context permissive-ctx))
                     (check-equal? (hash-ref args-received 'timeout) 42))
                   (test-case "user-provided timeout is not overwritten by tool default"
                     (define args-received #f)
                     (define t
                       (make-tool "timed"
                                  "desc"
                                  (hasheq 'type "object" 'properties (hasheq))
                                  (lambda (args ctx)
                                    (set! args-received args)
                                    (make-success-result "ok"))
                                  #:timeout-seconds 30))
                     (define registry (make-tool-registry))
                     (register-tool! registry t)
                     (define tc (tool-call "id-1" "timed" (hasheq 'command "echo hi" 'timeout 99)))
                     (define result (run-tool-batch (list tc) registry #:exec-context permissive-ctx))
                     (check-equal? (hash-ref args-received 'timeout) 99))
                   (test-case "no timeout injection when tool timeout is #f"
                     (define args-received #f)
                     (define t
                       (make-tool "plain"
                                  "desc"
                                  (hasheq 'type "object" 'properties (hasheq))
                                  (lambda (args ctx)
                                    (set! args-received args)
                                    (make-success-result "ok"))))
                     (define registry (make-tool-registry))
                     (register-tool! registry t)
                     (define tc (tool-call "id-1" "plain" (hasheq 'command "echo hi")))
                     (define result (run-tool-batch (list tc) registry #:exec-context permissive-ctx))
                     (check-false (hash-has-key? args-received 'timeout))))

(run-tests test-tool-timeout-scheduler)
