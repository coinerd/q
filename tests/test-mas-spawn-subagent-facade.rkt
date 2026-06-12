#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-spawn-subagent-facade.rkt — Tests for spawn-subagent capability facade
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../tools/builtins/spawn-subagent.rkt"
                  child-safe-tools
                  capability-filtered-child-tools
                  subagent-config
                  subagent-config?
                  subagent-config-task
                  subagent-config-role
                  subagent-config-max-turns
                  subagent-config-tools
                  subagent-config-model
                  parse-subagent-config)
         (only-in "../tools/tool.rkt" tool? tool-name tool-required-capability)
         (only-in "../agent/capability.rkt" current-session-capabilities))

(define suite
  (test-suite "MAS spawn-subagent Facade"

    ;; ── child-safe-tools: basic structure ──

    (test-case "child-safe-tools: returns list of tools"
      (define tools (child-safe-tools))
      (check-true (list? tools))
      (check-true (positive? (length tools)))
      (for ([t (in-list tools)])
        (check-true (tool? t) (format "~a should be a tool" t))))

    (test-case "child-safe-tools: includes core read-only tools"
      (define tools (child-safe-tools))
      (define names (map tool-name tools))
      (for ([name (in-list '("read" "grep" "find" "ls"))])
        (check-not-false (member name names) (format "~a should be in child-safe-tools" name))))

    (test-case "child-safe-tools: includes write and shell tools"
      (define tools (child-safe-tools))
      (define names (map tool-name tools))
      (for ([name (in-list '("write" "edit" "bash"))])
        (check-not-false (member name names) (format "~a should be in child-safe-tools" name))))

    ;; ── child-safe-tools: capability annotations ──

    (test-case "child-safe-tools: read has 'read-only capability"
      (define tools (child-safe-tools))
      (define read-tool (findf (lambda (t) (equal? (tool-name t) "read")) tools))
      (check-not-false read-tool)
      (check-equal? (tool-required-capability read-tool) 'read-only))

    (test-case "child-safe-tools: write has 'file-write capability"
      (define tools (child-safe-tools))
      (define write-tool (findf (lambda (t) (equal? (tool-name t) "write")) tools))
      (check-not-false write-tool)
      (check-equal? (tool-required-capability write-tool) 'file-write))

    (test-case "child-safe-tools: bash has 'shell-exec capability"
      (define tools (child-safe-tools))
      (define bash-tool (findf (lambda (t) (equal? (tool-name t) "bash")) tools))
      (check-not-false bash-tool)
      (check-equal? (tool-required-capability bash-tool) 'shell-exec))

    (test-case "child-safe-tools: all tools have valid capability annotations"
      (define tools (child-safe-tools))
      (for ([t (in-list tools)])
        (define cap (tool-required-capability t))
        (check-true (symbol? cap)
                    (format "tool ~a capability should be symbol, got ~a" (tool-name t) cap))
        (check-not-false (memq cap '(read-only file-write shell-exec))
                         (format "tool ~a has unexpected capability: ~a" (tool-name t) cap))))

    ;; ── capability-filtered-child-tools ──

    (test-case "capability-filtered: '(any) returns all tools"
      (parameterize ([current-session-capabilities '(any)])
        (define filtered (capability-filtered-child-tools))
        (define all (child-safe-tools))
        (check-equal? (length filtered) (length all))))

    (test-case "capability-filtered: '(read-only) returns only read tools"
      (parameterize ([current-session-capabilities '(read-only)])
        (define filtered (capability-filtered-child-tools))
        (define names (map tool-name filtered))
        (for ([name (in-list '("read" "grep" "find" "ls"))])
          (check-not-false (member name names) (format "~a should be in filtered tools" name)))
        (for ([name (in-list '("write" "edit" "bash"))])
          (check-false (member name names) (format "~a should NOT be in filtered tools" name)))))

    (test-case "capability-filtered: '(read-only file-write) includes read + write/edit"
      (parameterize ([current-session-capabilities '(read-only file-write)])
        (define filtered (capability-filtered-child-tools))
        (define names (map tool-name filtered))
        (check-not-false (member "read" names))
        (check-not-false (member "write" names))
        (check-not-false (member "edit" names))
        ;; shell-exec excluded
        (check-false (member "bash" names))))

    (test-case "capability-filtered: '(read-only shell-exec) includes bash"
      (parameterize ([current-session-capabilities '(read-only shell-exec)])
        (define filtered (capability-filtered-child-tools))
        (define names (map tool-name filtered))
        (check-not-false (member "bash" names))
        ;; file-write excluded
        (check-false (member "write" names))
        (check-false (member "edit" names))))

    (test-case "capability-filtered: empty caps returns no tools"
      (parameterize ([current-session-capabilities '()])
        (define filtered (capability-filtered-child-tools))
        (check-equal? filtered '())))

    ;; ── parse-subagent-config ──

    (test-case "parse-subagent-config: extracts task"
      (define cfg (parse-subagent-config (hasheq 'task "do something")))
      (check-equal? (subagent-config-task cfg) "do something"))

    (test-case "parse-subagent-config: default max-turns is 5"
      (define cfg (parse-subagent-config (hasheq 'task "test")))
      (check-equal? (subagent-config-max-turns cfg) 5))

    (test-case "parse-subagent-config: custom max-turns"
      (define cfg (parse-subagent-config (hasheq 'task "test" 'max-turns 10)))
      (check-equal? (subagent-config-max-turns cfg) 10))))

(run-tests suite)
