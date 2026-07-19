#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         rackunit/text-ui
         racket/string
         "../tools/builtins/spawn-execution-plan.rkt")

(define suite
  (test-suite "immutable spawn execution plans"
    (test-case "commitment binds every execution-effective field and list order"
      (define job
        (hasheq 'task-digest
                "task-a"
                'role-digest
                "role-a"
                'model
                "approved-model"
                'max-turns
                7
                'effective-capabilities
                '(read-only shell-exec)
                'effective-tools
                '("read" "bash")
                'tool-call-id
                "call-a"
                'child-id
                "child-a"
                'session-id
                "session-a"
                'batch-order
                0))
      (define snapshot
        (hasheq 'kind
                "batch"
                'provider-binding
                (hasheq 'name "anthropic" 'identity 41)
                'model
                "approved-model"
                'cwd
                "/tmp/project"
                'safe-mode
                #t
                'parent-session-id
                "parent-session"
                'parent-call-id
                "parent-call"
                'max-parallel
                2
                'aggregate
                #t
                'jobs
                (list job (hash-set job 'batch-order 1))))
      (define plan
        (make-spawn-execution-plan 'batch
                                   snapshot
                                   (hasheq 'task-preview "two jobs" 'capabilities '(shell-exec))))
      (check-true (immutable? (spawn-execution-plan-snapshot plan)))
      (check-true (spawn-execution-plan-matches? plan snapshot))
      (define top-level-mutations
        (list (cons 'provider-binding (hasheq 'name "other" 'identity 42))
              (cons 'model "other-model")
              (cons 'cwd "/other")
              (cons 'safe-mode #f)
              (cons 'parent-session-id "other-session")
              (cons 'parent-call-id "other-call")
              (cons 'max-parallel 1)
              (cons 'aggregate #f)))
      (for ([mutation (in-list top-level-mutations)])
        (check-false
         (spawn-execution-plan-matches? plan (hash-set snapshot (car mutation) (cdr mutation)))))
      (for ([mutation (in-list (list (cons 'task-digest "task-b")
                                     (cons 'role-digest "role-b")
                                     (cons 'model "job-model-b")
                                     (cons 'max-turns 8)
                                     (cons 'effective-capabilities '(read-only))
                                     (cons 'effective-tools '("read"))
                                     (cons 'tool-call-id "call-b")
                                     (cons 'child-id "child-b")
                                     (cons 'session-id "session-b")
                                     (cons 'batch-order 9)))])
        (check-false (spawn-execution-plan-matches?
                      plan
                      (hash-set snapshot
                                'jobs
                                (cons (hash-set job (car mutation) (cdr mutation))
                                      (cdr (hash-ref snapshot 'jobs)))))))
      (check-false (spawn-execution-plan-matches?
                    plan
                    (hash-set snapshot 'jobs (reverse (hash-ref snapshot 'jobs))))))
    (test-case "single and batch use the same commitment type"
      (define single
        (make-spawn-execution-plan 'single (hasheq 'jobs '(one)) (hasheq 'task-preview "x")))
      (define batch
        (make-spawn-execution-plan 'batch (hasheq 'jobs '(one two)) (hasheq 'task-preview "y")))
      (check-equal? (spawn-execution-plan-kind single) 'single)
      (check-equal? (spawn-execution-plan-kind batch) 'batch)
      (check-regexp-match #px"^[0-9a-f]{64}$" (spawn-execution-plan-digest single))
      (check-regexp-match #px"^[0-9a-f]{64}$" (spawn-execution-plan-presentation-digest batch)))
    (test-case "approval preview redacts secrets and controls"
      (define preview
        (redacted-approval-preview
         (string-append "Bearer abcdefghijklmnopqrstuvwxyz123456\napi_key=top-secret"
                        (string #\u001b)
                        "[31m run")
         200))
      (check-false (string-contains? preview "abcdefghijklmnopqrstuvwxyz123456"))
      (check-false (string-contains? preview "top-secret"))
      (check-false (string-contains? preview (string #\u001b))))))

(exit (run-tests suite))
