#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         rackunit/text-ui
         "../tools/permission-gate.rkt"
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         "../tools/tool-classification.rkt"
         (only-in "../util/tool/tool-types.rkt" make-tool-call))

(define suite
  (test-suite "spawn approval has one owning gate"

    (test-case "spawn tools are classified as tool-owned approval"
      (for ([name '("spawn-subagent" "spawn-subagents")])
        (check-eq? (classify-tool-by-name name) 'tool-owned-approval)
        (check-true (tool-name-tool-owned-approval? name))
        (check-true (tool-name-needs-approval? name))))

    (test-case "generic scheduler gate skips spawn tools"
      (define generic-callback-count (box 0))
      (define executed-count (box 0))
      (define registry (make-tool-registry))
      (register-tool! registry
                      (make-tool "spawn-subagent"
                                 "owned approval probe"
                                 (hasheq 'type "object" 'properties (hasheq))
                                 (lambda (_args _ctx)
                                   (set-box! executed-count (add1 (unbox executed-count)))
                                   (make-success-result "internally governed"))
                                 #:dangerous? #t))
      (define cfg
        (make-default-permission-config #:callback (lambda (_name _args)
                                                     (set-box! generic-callback-count
                                                               (add1 (unbox generic-callback-count)))
                                                     #f)))
      (define result
        (run-tool-batch (list (make-tool-call "spawn-gate-1" "spawn-subagent" (hasheq)))
                        registry
                        #:parallel? #f
                        #:exec-context (make-exec-context #:permission-config cfg)))
      (check-false (tool-result-is-error? (car (scheduler-result-results result))))
      (check-equal? (unbox generic-callback-count) 0)
      (check-equal? (unbox executed-count) 1))))

(exit (run-tests suite))
