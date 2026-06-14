#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-tool-annotations.rkt — Validate tool capability annotations
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../tools/registry-table.rkt"
                  tool-specs
                  tool-spec-name
                  tool-spec-required-capability)
         (only-in "../tools/tool.rkt"
                  make-tool-registry
                  list-tools
                  tool-name
                  tool-required-capability
                  tool?)
         (only-in "../tools/registry-table.rkt" register-tools-from-specs!)
         (only-in "../agent/capability.rkt" valid-capability?))

(define suite
  (test-suite "MAS Tool Annotations"

    ;; ── All specs have valid capability ──

    (test-case "all tool-specs have valid capabilities"
      (for ([spec (in-list tool-specs)])
        (define cap (tool-spec-required-capability spec))
        (check-true (valid-capability? cap)
                    (format "spec ~a has invalid capability: ~a" (tool-spec-name spec) cap))))

    ;; ── Read-only tools ──

    (test-case "read tools require 'read-only"
      (define names '("read" "grep" "find" "ls" "date"))
      (for ([name (in-list names)])
        (define spec (findf (lambda (s) (equal? (tool-spec-name s) name)) tool-specs))
        (check-not-false spec (format "spec for ~a should exist" name))
        (when spec
          (check-equal? (tool-spec-required-capability spec)
                        'read-only
                        (format "~a should require read-only" name)))))

    (test-case "session/state tools require 'read-only"
      (define names '("session_recall" "skill-route"))
      (for ([name (in-list names)])
        (define spec (findf (lambda (s) (equal? (tool-spec-name s) name)) tool-specs))
        (check-not-false spec (format "spec for ~a should exist" name))
        (when spec
          (check-equal? (tool-spec-required-capability spec)
                        'read-only
                        (format "~a should require read-only" name)))))

    ;; ── Plan-write tools ──

    (test-case "set-task-state requires 'plan-write"
      (define spec (findf (lambda (s) (equal? (tool-spec-name s) "set-task-state")) tool-specs))
      (check-not-false spec "spec for set-task-state should exist")
      (when spec
        (check-equal? (tool-spec-required-capability spec) 'plan-write)))

    ;; ── File-write tools ──

    (test-case "file tools require 'file-write"
      (define names '("write" "edit" "delete-lines"))
      (for ([name (in-list names)])
        (define spec (findf (lambda (s) (equal? (tool-spec-name s) name)) tool-specs))
        (check-not-false spec (format "spec for ~a should exist" name))
        (when spec
          (check-equal? (tool-spec-required-capability spec)
                        'file-write
                        (format "~a should require file-write" name)))))

    ;; ── Shell-exec tools ──

    (test-case "bash requires 'shell-exec"
      (define spec (findf (lambda (s) (equal? (tool-spec-name s) "bash")) tool-specs))
      (check-not-false spec "spec for bash should exist")
      (when spec
        (check-equal? (tool-spec-required-capability spec) 'shell-exec)))

    ;; ── Network tools ──

    (test-case "firecrawl requires 'network"
      (define spec (findf (lambda (s) (equal? (tool-spec-name s) "firecrawl")) tool-specs))
      (check-not-false spec "spec for firecrawl should exist")
      (when spec
        (check-equal? (tool-spec-required-capability spec) 'network)))

    ;; ── Browser tools ──

    (test-case "browser tools require 'browser"
      (define names
        '("browser_open" "browser_observe"
                         "browser_click"
                         "browser_type"
                         "browser_press"
                         "browser_extract"
                         "browser_screenshot"
                         "browser_scroll"
                         "browser_close"
                         "browser_check_local_app"))
      (for ([name (in-list names)])
        (define spec (findf (lambda (s) (equal? (tool-spec-name s) name)) tool-specs))
        (check-not-false spec (format "spec for ~a should exist" name))
        (when spec
          (check-equal? (tool-spec-required-capability spec)
                        'browser
                        (format "~a should require browser" name)))))

    ;; ── Memory tools ──

    (test-case "memory tools require 'memory-write"
      (define names
        '("store-memory" "search-memory"
                         "delete-memory"
                         "list-memory"
                         "clear-memory"
                         "update-memory"
                         "cleanup-expired-memory"
                         "consolidate-memory"
                         "record_conclusion"
                         "save-conclusion"))
      (for ([name (in-list names)])
        (define spec (findf (lambda (s) (equal? (tool-spec-name s) name)) tool-specs))
        (check-not-false spec (format "spec for ~a should exist" name))
        (when spec
          (check-equal? (tool-spec-required-capability spec)
                        'memory-write
                        (format "~a should require memory-write" name)))))

    ;; ── Subagent tools ──

    (test-case "subagent tools require 'subagent"
      (define names '("spawn-subagent" "spawn-subagents"))
      (for ([name (in-list names)])
        (define spec (findf (lambda (s) (equal? (tool-spec-name s) name)) tool-specs))
        (check-not-false spec (format "spec for ~a should exist" name))
        (when spec
          (check-equal? (tool-spec-required-capability spec)
                        'subagent
                        (format "~a should require subagent" name)))))

    ;; ── No spec has 'any ──

    (test-case "no tool-spec has 'any default"
      (for ([spec (in-list tool-specs)])
        (check-not-equal? (tool-spec-required-capability spec)
                          'any
                          (format "~a should not have 'any" (tool-spec-name spec)))))

    ;; ── Registration propagates capabilities ──

    (test-case "registered tools inherit spec capabilities"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define tools (list-tools reg))
      (for ([t (in-list tools)])
        (define cap (tool-required-capability t))
        (check-true (valid-capability? cap)
                    (format "tool ~a should have valid capability, got ~a" (tool-name t) cap))))))

(run-tests suite)
