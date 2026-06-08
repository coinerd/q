#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tool-timeout-scheduler.rkt — v0.70.7 W1

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt"
         "../tools/scheduler.rkt")

(define-test-suite test-tool-timeout-scheduler

  (test-case "execute-single injects per-tool timeout into args"
    (define args-received #f)
    (define t (make-tool "timed" "desc" (hasheq) (lambda (args ctx)
                                                    (set! args-received args)
                                                    (make-success-result "ok"))
                         #:timeout-seconds 42))
    (define registry (make-tool-registry))
    (register-tool! registry t)
    (define tc (tool-call "id-1" "timed" (hasheq 'command "echo hi")))
    (define result (run-tool-batch (list tc) registry))
    (check-equal? (hash-ref args-received 'timeout) 42))

  (test-case "user-provided timeout is not overwritten by tool default"
    (define args-received #f)
    (define t (make-tool "timed" "desc" (hasheq) (lambda (args ctx)
                                                    (set! args-received args)
                                                    (make-success-result "ok"))
                         #:timeout-seconds 30))
    (define registry (make-tool-registry))
    (register-tool! registry t)
    (define tc (tool-call "id-1" "timed" (hasheq 'command "echo hi" 'timeout 99)))
    (define result (run-tool-batch (list tc) registry))
    (check-equal? (hash-ref args-received 'timeout) 99))

  (test-case "no timeout injection when tool timeout is #f"
    (define args-received #f)
    (define t (make-tool "plain" "desc" (hasheq) (lambda (args ctx)
                                                    (set! args-received args)
                                                    (make-success-result "ok"))))
    (define registry (make-tool-registry))
    (register-tool! registry t)
    (define tc (tool-call "id-1" "plain" (hasheq 'command "echo hi")))
    (define result (run-tool-batch (list tc) registry))
    (check-false (hash-has-key? args-received 'timeout))))

(run-tests test-tool-timeout-scheduler)
