#lang racket/base

;; tests/test-capability-aware-spawn.rkt
;; v0.99.21 W2 (§4.2): Tests for capability-aware subagent spawning.

;; @speed fast
(require rackunit
         rackunit/text-ui
         racket/list
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt")

(define suite
  (test-suite "Capability-Aware Subagent Spawn (v0.99.21 §4.2)"

    (test-case "child-safe-tools-filtered returns all tools when #f"
      (define all (child-safe-tools))
      (define filtered (child-safe-tools-filtered #f))
      (check-equal? (length filtered) (length all)))

    (test-case "child-safe-tools-filtered gives explicit empty authority no tools"
      (check-equal? (child-safe-tools-filtered '()) '()))

    (test-case "read-only filter returns only read-only tools"
      (define filtered (child-safe-tools-filtered '(read-only)))
      (define names (map tool-name filtered))
      ;; Should include read-only tools: read, grep, find, ls
      (check-not-false (member "read" names) "read tool should be included")
      (check-not-false (member "grep" names) "grep tool should be included")
      (check-not-false (member "find" names) "find tool should be included")
      (check-not-false (member "ls" names) "ls tool should be included")
      ;; Should NOT include write, edit, bash
      (check-false (member "write" names) "write tool should NOT be included")
      (check-false (member "edit" names) "edit tool should NOT be included")
      (check-false (member "bash" names) "bash tool should NOT be included"))

    (test-case "file-write filter returns file-write tools"
      (define filtered (child-safe-tools-filtered '(file-write)))
      (define names (map tool-name filtered))
      ;; Should include write, edit
      (check-not-false (member "write" names) "write tool should be included")
      (check-not-false (member "edit" names) "edit tool should be included")
      ;; Should NOT include bash
      (check-false (member "bash" names) "bash tool should NOT be included"))

    (test-case "multiple capabilities include all matching tools"
      (define filtered (child-safe-tools-filtered '(read-only shell-exec)))
      (define names (map tool-name filtered))
      ;; read-only: read, grep, find, ls
      (check-not-false (member "read" names) "read tool should be included")
      (check-not-false (member "ls" names) "ls tool should be included")
      ;; shell-exec: bash
      (check-not-false (member "bash" names) "bash tool should be included")
      ;; file-write should NOT be included
      (check-false (member "write" names) "write tool should NOT be included")
      (check-false (member "edit" names) "edit tool should NOT be included"))

    (test-case "subagent-config has capabilities field"
      (define cfg (parse-subagent-config (hasheq 'task "test")))
      (check-true (subagent-config? cfg))
      (check-false (subagent-config-capabilities cfg)))

    (test-case "parse-subagent-config reads capabilities arg"
      (define cfg (parse-subagent-config (hasheq 'task "test" 'capabilities '("read-only"))))
      (check-equal? (subagent-config-capabilities cfg) '(read-only)))

    (test-case "parse-subagent-config handles multiple capabilities"
      (define cfg
        (parse-subagent-config (hasheq 'task "test" 'capabilities '("read-only" "file-write"))))
      (check-equal? (subagent-config-capabilities cfg) '(read-only file-write)))

    (test-case "parse-subagent-config rejects any explicit invalid capability"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (parse-subagent-config
                    (hasheq 'task "test" 'capabilities '("read-only" "bogus-cap"))))))

    (test-case "parse-subagent-config validates every effectful field before execution"
      (for ([bad-args (in-list (list (hasheq 'task "ok" 'role 42)
                                     (hasheq 'task "ok" 'model 42)
                                     (hasheq 'task "ok" 'tools '("read" 42))
                                     (hasheq 'task "ok" 'tools '("not-a-child-tool"))
                                     (hasheq 'task "ok" 'max-turns 0)
                                     (hasheq 'task "")))])
        (check-exn exn:fail:contract? (lambda () (parse-subagent-config bad-args)))))

    (test-case "invalid single spawn fails before approval and rate effects"
      (define events '())
      (define timestamps (box '()))
      (define ctx
        (make-exec-context #:event-publisher (lambda (type payload)
                                               (set! events (cons (cons type payload) events)))))
      (parameterize ([current-spawn-timestamps timestamps])
        (define result
          (tool-spawn-subagent (hasheq 'task "valid" 'role 42 'capabilities '(shell-exec)) ctx))
        (check-true (tool-result-is-error? result))
        (check-equal? events '())
        (check-equal? (unbox timestamps) '())))

    (test-case "parse-subagent-config preserves explicit empty authority"
      (define cfg (parse-subagent-config (hasheq 'task "test" 'capabilities '())))
      (check-equal? (subagent-config-capabilities cfg) '()))

    ;; v0.99.22 A-2: Batch capabilities support
    (test-case "parse-job-capabilities returns #f when no capabilities key"
      (define caps (parse-job-capabilities (hasheq 'task "hello")))
      (check-false caps))

    (test-case "parse-job-capabilities parses valid capabilities"
      (define caps
        (parse-job-capabilities (hasheq 'task "hello" 'capabilities '("read-only" "file-write"))))
      (check-equal? caps '(read-only file-write)))

    (test-case "parse-job-capabilities rejects invalid and preserves explicit empty"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (parse-job-capabilities
                    (hasheq 'task "hello" 'capabilities '("read-only" "bogus")))))
      (check-exn exn:fail:contract?
                 (lambda () (parse-job-capabilities (hasheq 'task "hello" 'capabilities '("any")))))
      (define caps3 (parse-job-capabilities (hasheq 'task "hello" 'capabilities '())))
      (check-equal? caps3 '()))))

(run-tests suite)
