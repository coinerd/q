#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w4-mas.rkt — MAS subsystem real-world audit
;;
;; Audit of the MAS (Multi-Agent System) subsystem covering:
;;   1. Capability taxonomy (VALID-CAPABILITIES, ROLE-CAPABILITIES)
;;   2. MAS message envelope (struct, serialization round-trip, validation)
;;   3. Permission gate (strict/permissive, auto-approved, unknown tools)
;;   4. Spawn subagent helpers (capability normalization, HITL, text extraction)
;;   5. MAS workflow skill parsing (frontmatter → pipeline, templates)
;;   6. Agent registry (registration, activation, version pinning, resolution)
;;   7. Session capability parameter
;;
;; FINDING: MAS subsystem is well-structured with clear separation between
;; pure capability logic, serialization envelopes, and stateful registry.
;;
;; All tests use synthetic data — no real API keys or filesystem needed.

(require rackunit
         racket/set
         racket/list
         racket/string
         ;; Capability system
         "../util/capability.rkt"
         ;; MAS envelope
         "../util/message/mas-envelope.rkt"
         ;; Permission gate
         "../tools/permission-gate.rkt"
         ;; Spawn subagent helpers (pure functions)
         "../tools/builtins/spawn-subagent-helpers.rkt"
         ;; MAS workflow skill
         "../skills/mas-workflow.rkt"
         ;; Agent registry types
         "../agent/registry-types.rkt"
         ;; Agent registry (stateful — reset between tests)
         "../agent/registry.rkt")

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(define (with-fresh-registry thunk)
  (reset-registry!)
  (thunk)
  (reset-registry!))

;; Simple factory for registry tests
(define (make-dummy-factory)
  (lambda () 'dummy-agent))

;; ---------------------------------------------------------------------------
;; 1. Capability Taxonomy
;; ---------------------------------------------------------------------------

(test-case "audit-cap-valid-set"
  (check-equal? (length VALID-CAPABILITIES) 10)
  (check-not-false (member 'read-only VALID-CAPABILITIES))
  (check-not-false (member 'plan-write VALID-CAPABILITIES))
  (check-not-false (member 'shell-exec VALID-CAPABILITIES))
  (check-not-false (member 'file-write VALID-CAPABILITIES))
  (check-not-false (member 'git-write VALID-CAPABILITIES))
  (check-not-false (member 'network VALID-CAPABILITIES))
  (check-not-false (member 'memory-write VALID-CAPABILITIES))
  (check-not-false (member 'browser VALID-CAPABILITIES))
  (check-not-false (member 'subagent VALID-CAPABILITIES))
  (check-not-false (member 'any VALID-CAPABILITIES)))

(test-case "audit-cap-valid-capability"
  (check-true (valid-capability? 'read-only))
  (check-true (valid-capability? 'any))
  (check-true (valid-capability? 'shell-exec))
  (check-false (valid-capability? 'unknown))
  (check-false (valid-capability? "read-only"))
  (check-false (valid-capability? 42))
  (check-false (valid-capability? #f)))

(test-case "audit-cap-all-capabilities-excludes-any"
  (define all (all-capabilities))
  (check-false (member 'any all) "'any should NOT be in all-capabilities")
  (check-equal? (length all) 9 "Should have 9 real capabilities"))

(test-case "audit-cap-roles-defined"
  (check-true (hash-has-key? ROLE-CAPABILITIES 'supervisor))
  (check-true (hash-has-key? ROLE-CAPABILITIES 'planner))
  (check-true (hash-has-key? ROLE-CAPABILITIES 'verifier))
  (check-true (hash-has-key? ROLE-CAPABILITIES 'tool-gateway))
  (check-true (hash-has-key? ROLE-CAPABILITIES 'executor)))

(test-case "audit-cap-supervisor-capabilities"
  (define caps (hash-ref ROLE-CAPABILITIES 'supervisor))
  (check-not-false (member 'read-only caps))
  (check-not-false (member 'plan-write caps))
  (check-not-false (member 'memory-write caps))
  (check-not-false (member 'subagent caps))
  ;; Supervisor does NOT have shell-exec or file-write directly
  (check-false (member 'shell-exec caps))
  (check-false (member 'file-write caps)))

(test-case "audit-cap-verifier-read-only"
  (define caps (hash-ref ROLE-CAPABILITIES 'verifier))
  (check-equal? caps '(read-only) "Verifier should be read-only only"))

(test-case "audit-cap-executor"
  (define caps (hash-ref ROLE-CAPABILITIES 'executor))
  (check-not-false (member 'shell-exec caps))
  (check-not-false (member 'file-write caps))
  (check-false (member 'plan-write caps) "Executor should not write plans"))

(test-case "audit-cap-tool-gateway"
  (define caps (hash-ref ROLE-CAPABILITIES 'tool-gateway))
  (check-not-false (member 'shell-exec caps))
  (check-not-false (member 'file-write caps))
  (check-not-false (member 'git-write caps))
  (check-not-false (member 'network caps))
  (check-not-false (member 'browser caps)))

(test-case "audit-cap-role-has-capability"
  (check-true (role-has-capability? 'supervisor 'subagent))
  (check-true (role-has-capability? 'supervisor 'any) "Any role can use 'any capability")
  (check-true (role-has-capability? 'verifier 'read-only))
  (check-false (role-has-capability? 'verifier 'shell-exec))
  (check-false (role-has-capability? 'verifier 'file-write))
  (check-false (role-has-capability? 'planner 'shell-exec))
  (check-false (role-has-capability? 'unknown-role 'read-only)))

(test-case "audit-cap-session-parameter"
  (check-equal? (current-session-capabilities) '(any) "Default should be '(any)")
  (parameterize ([current-session-capabilities '(read-only)])
    (check-equal? (current-session-capabilities) '(read-only)))
  (check-equal? (current-session-capabilities) '(any) "Should restore after parameterize"))

;; ---------------------------------------------------------------------------
;; 2. MAS Envelope
;; ---------------------------------------------------------------------------

(test-case "audit-env-create-minimal"
  (define env (make-mas-envelope 'supervisor 'executor 'any "do something"))
  (check-true (mas-envelope? env))
  (check-equal? (mas-envelope-source-agent env) 'supervisor)
  (check-equal? (mas-envelope-target-agent env) 'executor)
  (check-equal? (mas-envelope-payload env) "do something")
  (check-equal? (mas-envelope-risk-level env) 'low "Default risk should be low")
  (check-equal? (mas-envelope-schema-version env) 1)
  (check-true (string? (mas-envelope-message-id env)))
  (check-true (string? (mas-envelope-trace-id env)))
  (check-true (integer? (mas-envelope-deadline env)) "Deadline should be auto-generated"))

(test-case "audit-env-create-full"
  (define env
    (make-mas-envelope 'supervisor
                       'planner
                       'plan-write
                       (hasheq 'action "create-plan")
                       #:message-id "msg-001"
                       #:trace-id "trace-001"
                       #:deadline 9999999999999
                       #:risk-level 'high))
  (check-equal? (mas-envelope-message-id env) "msg-001")
  (check-equal? (mas-envelope-trace-id env) "trace-001")
  (check-equal? (mas-envelope-capability env) 'plan-write)
  (check-equal? (mas-envelope-deadline env) 9999999999999)
  (check-equal? (mas-envelope-risk-level env) 'high))

(test-case "audit-env-invalid-capability"
  (check-exn exn:fail:contract?
             (lambda () (make-mas-envelope 'supervisor 'executor 'bogus "payload"))
             "Invalid capability should raise contract error"))

(test-case "audit-env-invalid-source-agent"
  (check-exn exn:fail:contract?
             (lambda () (make-mas-envelope 'unknown-role 'executor 'any "payload"))
             "Invalid source agent should raise"))

(test-case "audit-env-invalid-target-agent"
  (check-exn exn:fail:contract?
             (lambda () (make-mas-envelope 'supervisor 'unknown-role 'any "payload"))
             "Invalid target agent should raise"))

(test-case "audit-env-invalid-risk-level"
  (check-exn exn:fail:contract?
             (lambda () (make-mas-envelope 'supervisor 'executor 'any "payload" #:risk-level 'bogus))
             "Invalid risk level should raise"))

(test-case "audit-env-serialization-roundtrip"
  (define env
    (make-mas-envelope 'supervisor
                       'planner
                       'plan-write
                       "test payload"
                       #:message-id "msg-roundtrip"
                       #:trace-id "trace-rt"
                       #:deadline 1234567890
                       #:risk-level 'critical))
  (define h (envelope->hash env))
  (check-true (hash? h))
  (check-equal? (hash-ref h 'message-id) "msg-roundtrip")
  (check-equal? (hash-ref h 'source-agent) "supervisor")
  (check-equal? (hash-ref h 'target-agent) "planner")
  (check-equal? (hash-ref h 'capability) "plan-write")
  (check-equal? (hash-ref h 'risk-level) "critical")
  (check-equal? (hash-ref h 'deadline) 1234567890)
  ;; Round-trip back
  (define env2 (hash->envelope h))
  (check-true (mas-envelope? env2))
  (check-equal? (mas-envelope-payload env2) "test payload"))

(test-case "audit-env-hash-to-envelope-invalid"
  (check-false (hash->envelope "not-a-hash"))
  (check-false (hash->envelope 42))
  (check-false (hash->envelope #f)))

(test-case "audit-env-auto-generated-ids-unique"
  (define env1 (make-mas-envelope 'supervisor 'executor 'any "p1"))
  (define env2 (make-mas-envelope 'supervisor 'executor 'any "p2"))
  (check-false (string=? (mas-envelope-message-id env1) (mas-envelope-message-id env2))
               "Auto-generated message IDs should be unique")
  (check-false (string=? (mas-envelope-trace-id env1) (mas-envelope-trace-id env2))
               "Auto-generated trace IDs should be unique"))

(test-case "audit-env-risk-levels-all-valid"
  (for ([risk '(low medium high critical)])
    (define env (make-mas-envelope 'supervisor 'executor 'any "p" #:risk-level risk))
    (check-equal? (mas-envelope-risk-level env) risk)))

(test-case "audit-env-deadline-auto-future"
  (define before (current-inexact-milliseconds))
  (define env (make-mas-envelope 'supervisor 'executor 'any "p"))
  (define after (current-inexact-milliseconds))
  (define deadline (mas-envelope-deadline env))
  (check-true (> deadline (inexact->exact (round before))) "Auto-deadline should be in the future")
  ;; Default deadline is 5 minutes = 300000ms
  (check-true (>= deadline (+ (inexact->exact (round before)) 299000))
              "Deadline should be at least ~5 min from now"))

;; ---------------------------------------------------------------------------
;; 3. Permission Gate
;; ---------------------------------------------------------------------------

(test-case "audit-pg-default-config"
  (define cfg (make-default-permission-config))
  (check-true (permission-config? cfg))
  (check-equal? (permission-config-policy-mode cfg) 'strict))

(test-case "audit-pg-auto-approved-tools"
  (define cfg (make-default-permission-config))
  (check-false (tool-needs-approval? cfg "read"))
  (check-false (tool-needs-approval? cfg "grep"))
  (check-false (tool-needs-approval? cfg "ls"))
  (check-false (tool-needs-approval? cfg "find"))
  (check-false (tool-needs-approval? cfg "session_recall")))

(test-case "audit-pg-needs-approval-tools"
  (define cfg (make-default-permission-config))
  (check-true (tool-needs-approval? cfg "edit"))
  (check-true (tool-needs-approval? cfg "write"))
  (check-true (tool-needs-approval? cfg "bash"))
  (check-true (tool-needs-approval? cfg "spawn-subagent"))
  (check-true (tool-needs-approval? cfg "delete")))

(test-case "audit-pg-strict-mode-unknown-requires-approval"
  (define cfg (make-default-permission-config #:policy-mode 'strict))
  (check-true (tool-needs-approval? cfg "unknown-tool")
              "Strict mode: unknown tools require approval"))

(test-case "audit-pg-permissive-mode-unknown-auto-approved"
  (define cfg (make-default-permission-config #:policy-mode 'permissive))
  (check-false (tool-needs-approval? cfg "unknown-tool")
               "Permissive mode: unknown tools auto-approved"))

(test-case "audit-pg-memory-tools-auto-approved"
  (define cfg (make-default-permission-config))
  (check-false (tool-needs-approval? cfg "list-memory"))
  (check-false (tool-needs-approval? cfg "search-memory"))
  (check-false (tool-needs-approval? cfg "store-memory")))

(test-case "audit-pg-destructive-memory-needs-approval"
  (define cfg (make-default-permission-config))
  (check-true (tool-needs-approval? cfg "delete-memory"))
  (check-true (tool-needs-approval? cfg "clear-memory")))

(test-case "audit-pg-custom-approval-callback"
  (define approved-box (box #f))
  (define cfg
    (make-default-permission-config #:callback (lambda (tool args)
                                                 (set-box! approved-box (cons tool args))
                                                 #t)))
  (define result (request-approval cfg "edit" (hasheq 'file "test.rkt")))
  (check-true result)
  (check-equal? (car (unbox approved-box)) "edit"))

;; ---------------------------------------------------------------------------
;; 4. Spawn Subagent Helpers (Pure Functions)
;; ---------------------------------------------------------------------------

(test-case "audit-sh-normalize-none"
  (check-false (normalize-capabilities #f))
  (check-false (normalize-capabilities '())))

(test-case "audit-sh-normalize-single-string"
  (define result (normalize-capabilities "read-only"))
  (check-equal? result '(read-only)))

(test-case "audit-sh-normalize-single-invalid"
  (check-false (normalize-capabilities "bogus-cap")))

(test-case "audit-sh-normalize-list"
  (define result (normalize-capabilities '("read-only" "file-write")))
  (check-equal? result '(read-only file-write)))

(test-case "audit-sh-normalize-list-with-invalid"
  (define result (normalize-capabilities '("read-only" "bogus" "shell-exec")))
  (check-equal? result '(read-only shell-exec) "Invalid entries should be filtered"))

(test-case "audit-sh-normalize-symbols"
  (define result (normalize-capabilities '(read-only shell-exec)))
  (check-equal? result '(read-only shell-exec)))

(test-case "audit-sh-normalize-empty-after-filter"
  (check-false (normalize-capabilities '("bogus1" "bogus2")) "All-invalid list should return #f"))

(test-case "audit-sh-hitl-approval-required"
  ;; FINDING: memq returns tail list (truthy), not #t. Use check-not-false.
  (check-not-false (requires-hitl-approval? '(shell-exec)))
  (check-not-false (requires-hitl-approval? '(git-write)))
  (check-not-false (requires-hitl-approval? '(read-only shell-exec))))

(test-case "audit-sh-hitl-approval-not-required"
  (check-false (requires-hitl-approval? '(read-only)))
  (check-false (requires-hitl-approval? '(file-write)))
  (check-false (requires-hitl-approval? '()))
  (check-false (requires-hitl-approval? #f)))

(test-case "audit-sh-summary-max-chars"
  (check-equal? SUBAGENT-SUMMARY-MAX-CHARS 4000))

(test-case "audit-sh-extract-text-summary-short"
  (define result (extract-text-summary (list (hasheq 'text "Hello world"))))
  (check-equal? result "Hello world"))

(test-case "audit-sh-extract-text-summary-truncation"
  (define long-text (make-string 5000 #\A))
  (define result (extract-text-summary (list (hasheq 'text long-text))))
  (check-true (> (string-length result) 0) "Should have content")
  (check-true (string-suffix? result "...") "Truncated text should end with ellipsis")
  (check-true (<= (string-length result) SUBAGENT-SUMMARY-MAX-CHARS) "Should not exceed max chars"))

(test-case "audit-sh-extract-text-summary-empty"
  (define result (extract-text-summary '()))
  (check-equal? result ""))

(test-case "audit-sh-extract-text-summary-custom-max"
  (define result (extract-text-summary (list (hasheq 'text "ABCDEFGHIJ")) 5))
  (check-equal? result "AB..."))

;; ---------------------------------------------------------------------------
;; 5. MAS Workflow Skill Parsing
;; ---------------------------------------------------------------------------

(test-case "audit-wf-parse-simple"
  (define-values (wf err)
    (parse-mas-workflow
     "test-wf"
     "A test workflow"
     (hasheq 'agents
             (list (hasheq 'role "analyst" 'task "Read {{file}}" 'capabilities (list "read-only"))))))
  (check-false err)
  (check-true (mas-workflow? wf))
  (check-equal? (mas-workflow-name wf) "test-wf")
  (check-equal? (length (mas-workflow-steps wf)) 1)
  (check-equal? (workflow-step-role (car (mas-workflow-steps wf))) "analyst"))

(test-case "audit-wf-parse-multi-step"
  (define-values (wf err)
    (parse-mas-workflow "multi"
                        "Multi-step pipeline"
                        (hasheq 'agents
                                (list (hasheq 'role "analyst" 'task "Summarize {{file}}")
                                      (hasheq 'role "reviewer" 'task "Review: {{result}}")))))
  (check-false err)
  (check-true (mas-workflow? wf))
  (check-equal? (length (mas-workflow-steps wf)) 2))

(test-case "audit-wf-parse-missing-agents"
  (define-values (wf err) (parse-mas-workflow "bad" "desc" (hasheq)))
  (check-false wf)
  (check-true (string? err)))

(test-case "audit-wf-parse-empty-agents"
  (define-values (wf err) (parse-mas-workflow "bad" "desc" (hasheq 'agents '())))
  (check-false wf)
  (check-true (string? err)))

(test-case "audit-wf-parse-agents-not-list"
  (define-values (wf err) (parse-mas-workflow "bad" "desc" (hasheq 'agents "not-a-list")))
  (check-false wf)
  (check-true (string? err)))

(test-case "audit-wf-parse-missing-task"
  (define-values (wf err)
    (parse-mas-workflow "bad" "desc" (hasheq 'agents (list (hasheq 'role "analyst")))))
  (check-false wf)
  (check-true (string? err)))

(test-case "audit-wf-template-variables"
  (define-values (wf err)
    (parse-mas-workflow "vars"
                        "desc"
                        (hasheq 'agents
                                (list (hasheq 'role "a" 'task "Read {{file}} and {{result}}")
                                      (hasheq 'role "b" 'task "Check {{file}}")))))
  (check-false err)
  (define vars (mas-workflow-variables wf))
  (check-not-false (member "file" vars))
  (check-not-false (member "result" vars)))

(test-case "audit-wf-result-chaining"
  (define-values (wf err)
    (parse-mas-workflow "chain"
                        "desc"
                        (hasheq 'agents
                                (list (hasheq 'role "a" 'task "Do something")
                                      (hasheq 'role "b" 'task "Continue: {{result}}")))))
  (check-false err)
  (check-true (workflow-has-result-chaining? wf)))

(test-case "audit-wf-no-result-chaining"
  (define-values (wf err)
    (parse-mas-workflow "nochain"
                        "desc"
                        (hasheq 'agents (list (hasheq 'role "a" 'task "Do {{file}}")))))
  (check-false err)
  (check-false (workflow-has-result-chaining? wf)))

(test-case "audit-wf-parallel-flag"
  (define-values (wf err)
    (parse-mas-workflow "par"
                        "desc"
                        (hasheq 'agents
                                (list (hasheq 'role "a" 'task "T1" 'parallel "true")
                                      (hasheq 'role "b" 'task "T2" 'parallel "yes")))))
  (check-false err)
  (define steps (mas-workflow-steps wf))
  (check-true (workflow-step-parallel? (car steps)))
  (check-true (workflow-step-parallel? (cadr steps))))

(test-case "audit-wf-capabilities-parsed"
  (define-values (wf err)
    (parse-mas-workflow
     "caps"
     "desc"
     (hasheq 'agents
             (list (hasheq 'role "a" 'task "T" 'capabilities (list "read-only" "file-write"))))))
  (check-false err)
  (define step (car (mas-workflow-steps wf)))
  (check-equal? (workflow-step-capabilities step) '(read-only file-write)))

;; ---------------------------------------------------------------------------
;; 6. Agent Registry
;; ---------------------------------------------------------------------------

(test-case "audit-reg-register-and-resolve"
  (with-fresh-registry (lambda ()
                         (register-agent! 'test-role "1.0.0" (make-dummy-factory))
                         (define desc (resolve-agent 'test-role))
                         (check-true (agent-descriptor? desc))
                         (check-equal? (agent-descriptor-role-name desc) 'test-role)
                         (check-equal? (agent-descriptor-version desc) "1.0.0")
                         (check-true (agent-descriptor-active? desc)
                                     "First version should be active"))))

(test-case "audit-reg-multiple-versions"
  (with-fresh-registry (lambda ()
                         (register-agent! 'multi-role "1.0.0" (make-dummy-factory))
                         (register-agent! 'multi-role "2.0.0" (make-dummy-factory))
                         (define versions (agent-versions 'multi-role))
                         (check-equal? (length versions) 2)
                         (check-not-false (member "1.0.0" versions))
                         (check-not-false (member "2.0.0" versions))
                         ;; First registered should be active
                         (define active (resolve-agent 'multi-role))
                         (check-equal? (agent-descriptor-version active) "1.0.0"))))

(test-case "audit-reg-activate-version"
  (with-fresh-registry (lambda ()
                         (register-agent! 'act-role "1.0.0" (make-dummy-factory))
                         (register-agent! 'act-role "2.0.0" (make-dummy-factory))
                         (activate-agent-version! 'act-role "2.0.0")
                         (define active (resolve-agent 'act-role))
                         (check-equal? (agent-descriptor-version active) "2.0.0")
                         ;; Old version should be inactive
                         (define old (resolve-agent-version 'act-role "1.0.0"))
                         (check-false (agent-descriptor-active? old)))))

(test-case "audit-reg-idempotent-registration"
  (with-fresh-registry
   (lambda ()
     (register-agent! 'idem-role "1.0.0" (make-dummy-factory))
     (register-agent! 'idem-role "1.0.0" (make-dummy-factory))
     (define versions (agent-versions 'idem-role))
     (check-equal? (length versions) 1 "Duplicate registration should be idempotent"))))

(test-case "audit-reg-resolve-unknown"
  (with-fresh-registry
   (lambda () (check-false (resolve-agent 'unknown-role) "Unregistered role should resolve to #f"))))

(test-case "audit-reg-resolve-specific-version"
  (with-fresh-registry (lambda ()
                         (register-agent! 'ver-role "1.0.0" (make-dummy-factory))
                         (register-agent! 'ver-role "2.0.0" (make-dummy-factory))
                         (define desc (resolve-agent-version 'ver-role "2.0.0"))
                         (check-true (agent-descriptor? desc))
                         (check-equal? (agent-descriptor-version desc) "2.0.0")))

  (test-case "audit-reg-make-instance"
    (with-fresh-registry (lambda ()
                           (register-agent! 'inst-role "1.0.0" (make-dummy-factory))
                           (define agent (make-agent-instance 'inst-role))
                           (check-equal? agent 'dummy-agent)))))

(test-case "audit-reg-version-pinning"
  (with-fresh-registry (lambda ()
                         (register-agent! 'pin-role "1.0.0" (make-dummy-factory))
                         (register-agent! 'pin-role "2.0.0" (make-dummy-factory))
                         (activate-agent-version! 'pin-role "2.0.0")
                         (define pins (pin-current-versions))
                         (check-true (hash-has-key? pins 'pin-role))
                         (define pin (hash-ref pins 'pin-role))
                         (check-true (version-pin? pin))
                         (check-equal? (version-pin-pinned-version pin) "2.0.0"))))

(test-case "audit-reg-registered-roles"
  (with-fresh-registry (lambda ()
                         (register-agent! 'role-a "1.0.0" (make-dummy-factory))
                         (register-agent! 'role-b "1.0.0" (make-dummy-factory))
                         (define roles (registered-roles))
                         (check-equal? (length roles) 2)
                         (check-not-false (member 'role-a roles))
                         (check-not-false (member 'role-b roles)))))

(test-case "audit-reg-make-with-pin"
  (with-fresh-registry (lambda ()
                         (register-agent! 'wp-role "1.0.0" (make-dummy-factory))
                         (register-agent! 'wp-role "2.0.0" (make-dummy-factory))
                         ;; Active is 1.0.0
                         (define agent-no-pin (make-agent-with-pin 'wp-role #f))
                         (check-equal? agent-no-pin 'dummy-agent)
                         ;; Pin to 2.0.0
                         (define pin (version-pin 'wp-role "2.0.0" 0))
                         (define agent-pinned (make-agent-with-pin 'wp-role pin))
                         (check-equal? agent-pinned 'dummy-agent))))

(test-case "audit-reg-activate-unknown-version-error"
  (with-fresh-registry (lambda ()
                         (register-agent! 'err-role "1.0.0" (make-dummy-factory))
                         (check-exn exn:fail?
                                    (lambda () (activate-agent-version! 'err-role "99.0.0"))))))

;; ---------------------------------------------------------------------------
;; 7. Registry Types
;; ---------------------------------------------------------------------------

(test-case "audit-rt-agent-descriptor"
  (define d (agent-descriptor 'test "1.0" void #f #f #t))
  (check-true (agent-descriptor? d))
  (check-equal? (agent-descriptor-role-name d) 'test)
  (check-equal? (agent-descriptor-version d) "1.0")
  (check-true (agent-descriptor-active? d)))

(test-case "audit-rt-version-pin"
  (define pin (version-pin 'planner "2.0" 12345))
  (check-true (version-pin? pin))
  (check-equal? (version-pin-role-name pin) 'planner)
  (check-equal? (version-pin-pinned-version pin) "2.0")
  (check-equal? (version-pin-pinned-at pin) 12345))

(test-case "audit-rt-registry-entry"
  (define d (agent-descriptor 'test "1.0" void #f #f #t))
  (define entry (registry-entry 'test (list d)))
  (check-true (registry-entry? entry))
  (check-equal? (registry-entry-role-name entry) 'test)
  (check-equal? (length (registry-entry-descriptors entry)) 1))
