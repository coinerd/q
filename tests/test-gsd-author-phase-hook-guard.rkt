#lang racket
;; @covers extensions/hooks.rkt tools/scheduler-execution.rkt
;; @speed fast
;; @suite extensions
;; @boundary integration

(require rackunit rackunit/text-ui
         "helpers/fixtures.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../extensions/gsd-planning.rkt"
         "../extensions/gsd-planning/execution-policy.rkt"
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         "../tools/permission-gate.rkt")

;; Fault the registered production handler, not a synthetic block result. The
;; handler sees the trusted author phase before its result is lost to a fault.
(define (exercise fault name)
  (with-temp-dir
   (lambda (repo)
     (define sentinel (build-path repo "forbidden"))
     (define calls (box 0))
     (define ext-reg (make-extension-registry))
     (define real-handler
       (hash-ref (extension-hooks gsd-planning-extension) 'tool-call-pre))
     (define faulty-handler
       (lambda (payload)
         (set-box! calls (add1 (unbox calls)))
         (define result (real-handler payload))
         (case fault
           [(timeout) (sync never-evt)]
           [(error) (error 'injected "authorization handler failure")]
           [else result])))
     (register-extension!
      ext-reg
      (struct-copy extension gsd-planning-extension
                   [hooks (hash-set (extension-hooks gsd-planning-extension)
                                    'tool-call-pre faulty-handler)]))
     (define tools (make-tool-registry))
     (register-tool!
      tools
      (make-tool name "Forbidden mutation sentinel"
                 (hasheq 'type "object" 'properties (hasheq))
                 (lambda (_args _ctx)
                   (display-to-file "executed" sentinel)
                   (make-success-result "executed"))))
     (define dispatcher
       (lambda (point payload)
         (when (and (eq? fault 'dispatcher) (eq? point 'tool-call-pre))
           (error 'injected "dispatcher failure"))
         (dispatch-hooks point payload ext-reg)))
     (define result
       (parameterize ([current-directory repo]
                      [current-hook-timeout-ms 30]
                      [current-gsd-author-phase
                       (hasheq 'phase 'test-author 'repo repo
                               'files '("impl.rkt" "tests/a.rkt")
                               'tests '("tests/a.rkt"))])
         (run-tool-batch
          (list (make-tool-call "forbidden-call" name (hasheq))) tools
          #:parallel? #f
          #:hook-dispatcher (and (not (eq? fault 'no-hook)) dispatcher)
          #:exec-context
          (make-exec-context #:permission-config (make-permissive-permission-config)))))
     (define results (scheduler-result-results result))
     (check-equal? (length results) 1)
     (check-equal? (tool-result-is-error? (car results)) (not (eq? fault 'no-hook)))
     (check-equal? (file-exists? sentinel) (eq? fault 'no-hook))
     (unless (memq fault '(dispatcher no-hook))
       (check-equal? (unbox calls) 1)))))

(define tests
  (test-suite
   "Actual author-phase hook authorization failures"
   (test-case "registered phase hook blocks forbidden mutation normally"
     (exercise 'normal "bash") (exercise 'normal "custom-write"))
   (test-case "actual hook timeout cannot authorize shell or custom mutation"
     (exercise 'timeout "bash") (exercise 'timeout "custom-write"))
   (test-case "actual hook error cannot authorize shell or custom mutation"
     (exercise 'error "bash") (exercise 'error "custom-write"))
   (test-case "dispatcher failure cannot authorize shell or custom mutation"
     (exercise 'dispatcher "bash") (exercise 'dispatcher "custom-write"))
   (test-case "ordinary no-hook operation remains unchanged"
     (exercise 'no-hook "custom-write"))))
(module+ main (exit (if (zero? (run-tests tests)) 0 1)))
(module+ test (check-equal? (run-tests tests) 0))
