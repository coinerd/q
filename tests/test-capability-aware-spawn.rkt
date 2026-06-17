#lang racket/base

;; tests/test-capability-aware-spawn.rkt
;; v0.99.21 W2 (§4.2): Tests for capability-aware subagent spawning.

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

    (test-case "child-safe-tools-filtered returns all tools when empty list"
      (define all (child-safe-tools))
      (define filtered (child-safe-tools-filtered '()))
      (check-equal? (length filtered) (length all)))

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

    (test-case "parse-subagent-config filters invalid capabilities"
      (define cfg
        (parse-subagent-config (hasheq 'task "test" 'capabilities '("read-only" "bogus-cap"))))
      ;; Only valid capability is kept
      (check-equal? (subagent-config-capabilities cfg) '(read-only)))

    (test-case "parse-subagent-config handles empty capabilities list"
      (define cfg (parse-subagent-config (hasheq 'task "test" 'capabilities '())))
      ;; Empty list after filtering becomes #f (treat as "all tools")
      (check-false (subagent-config-capabilities cfg)))

    ;; v0.99.22 A-2: Batch capabilities support
    (test-case "parse-job-capabilities returns #f when no capabilities key"
      (define caps (parse-job-capabilities (hasheq 'task "hello")))
      (check-false caps))

    (test-case "parse-job-capabilities parses valid capabilities"
      (define caps
        (parse-job-capabilities (hasheq 'task "hello" 'capabilities '("read-only" "file-write"))))
      (check-equal? caps '(read-only file-write)))

    (test-case "parse-job-capabilities filters invalid and returns #f for empty"
      ;; Mix of valid and invalid
      (define caps1
        (parse-job-capabilities (hasheq 'task "hello" 'capabilities '("read-only" "bogus"))))
      (check-equal? caps1 '(read-only))
      ;; All invalid → #f
      (define caps2 (parse-job-capabilities (hasheq 'task "hello" 'capabilities '("bogus"))))
      (check-false caps2)
      ;; Empty list → #f
      (define caps3 (parse-job-capabilities (hasheq 'task "hello" 'capabilities '())))
      (check-false caps3))))

(run-tests suite)
