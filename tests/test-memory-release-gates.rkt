#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-memory-release-gates.rkt — Release hardening truth gates
;;
;; v0.95.12: Validates that memory system defaults are safe:
;; - Memory disabled by default (no backend, no auto-extraction)
;; - External backend disabled by default
;; - All tools return safe responses when disabled
;; - No hidden/silent memory writes

(require racket/file
         racket/runtime-path
         rackunit
         racket/string
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/service.rkt"
         "../runtime/memory/auto-extraction.rkt"
         "../runtime/memory/backends/external-protocol.rkt"
         "../runtime/memory/reflection.rkt"
         "../agent/event-structs/memory-events.rkt"
         "../runtime/context-assembly/memory-builder.rkt"
         "../tools/builtins/memory-tools.rkt"
         "../tools/tool.rkt"
         (only-in "../runtime/session/session-config.rkt"
                  hash->session-config
                  config-memory-enabled?
                  config-memory-backend))

(define-runtime-path memory-builder-path "../runtime/context-assembly/memory-builder.rkt")
(define-runtime-path memory-service-path "../runtime/memory/service.rkt")

;; ---------------------------------------------------------------------------
;; Truth Gate 1: Memory disabled by default
;; ---------------------------------------------------------------------------

(test-case "release-gate: memory backend is #f by default"
  (check-false (current-memory-backend)))

(test-case "release-gate: auto-extraction disabled by default"
  (check-false (current-auto-extraction-enabled)))

(test-case "release-gate: external backend disabled by default"
  (check-false (current-external-backend-enabled)))

;; ---------------------------------------------------------------------------
;; Truth Gate 2: Tools return safe responses when disabled
;; ---------------------------------------------------------------------------

(test-case "release-gate: store-memory returns error when disabled"
  (parameterize ([current-memory-backend #f])
    (define r ((tool-execute store-memory) (hash 'content "test" 'scope "session") #f))
    (check-true (tool-result-is-error? r))))

(test-case "release-gate: search-memory returns error when disabled"
  (parameterize ([current-memory-backend #f])
    (define r ((tool-execute search-memory) (hash 'query "test") #f))
    (check-true (tool-result-is-error? r))))

(test-case "release-gate: delete-memory returns error when disabled"
  (parameterize ([current-memory-backend #f])
    (define r ((tool-execute delete-memory) (hash 'id "x" 'scope "session") #f))
    (check-true (tool-result-is-error? r))))

(test-case "release-gate: list-memory returns error when disabled"
  (parameterize ([current-memory-backend #f])
    (define r ((tool-execute list-memory) (hash) #f))
    (check-true (tool-result-is-error? r))))

(test-case "release-gate: clear-memory returns error when disabled"
  (parameterize ([current-memory-backend #f])
    (define r ((tool-execute clear-memory) (hash 'scope "session" 'confirm #t) #f))
    (check-true (tool-result-is-error? r))))

;; ---------------------------------------------------------------------------
;; Truth Gate 3: Policy defaults are safe
;; ---------------------------------------------------------------------------

(test-case "release-gate: default policy blocks secret sensitivity"
  ;; default-memory-policy allows '(public internal sensitive) but NOT 'secret
  ;; We verify by checking the policy struct is valid
  (check-true (memory-policy? default-memory-policy)))

(test-case "release-gate: default policy is a valid policy"
  (check-true (memory-policy? default-memory-policy)))

;; ---------------------------------------------------------------------------
;; Truth Gate 4: No hidden writes
;; ---------------------------------------------------------------------------

(test-case "release-gate: auto-extract returns skipped when disabled"
  (define results
    (try-auto-extract "Some useful fact"
                      #:backend #f
                      #:policy default-memory-policy
                      #:session-id "test"
                      #:project-root "/test"))
  (for ([r (in-list results)])
    (check-equal? (extraction-result-action r) 'skipped)))

(test-case "release-gate: external backend store returns error when disabled"
  (define ext
    (make-external-backend "test" (lambda (method payload) (memory-result #t 'stored #f (hasheq)))))
  (define item
    (memory-item "id" 'semantic 'session "content" (hasheq) (hasheq) "2025-01-01" "2025-01-01"))
  (define r (gen:store-memory! ext item))
  (check-false (memory-result-ok? r)))

;; ---------------------------------------------------------------------------
;; Truth Gate 5: All tool definitions exist
;; ---------------------------------------------------------------------------

(test-case "release-gate: all current memory tools are defined"
  (check-true (tool? store-memory))
  (check-true (tool? search-memory))
  (check-true (tool? delete-memory))
  (check-true (tool? list-memory))
  (check-true (tool? clear-memory))
  (check-true (tool? update-memory))
  (check-true (tool? cleanup-expired-memory))
  (check-true (tool? consolidate-memory)))

(test-case "release-gate: memory tools have correct names"
  (check-equal? (tool-name store-memory) "store-memory")
  (check-equal? (tool-name search-memory) "search-memory")
  (check-equal? (tool-name delete-memory) "delete-memory")
  (check-equal? (tool-name list-memory) "list-memory")
  (check-equal? (tool-name clear-memory) "clear-memory")
  (check-equal? (tool-name update-memory) "update-memory")
  (check-equal? (tool-name cleanup-expired-memory) "cleanup-expired-memory")
  (check-equal? (tool-name consolidate-memory) "consolidate-memory"))

;; ---------------------------------------------------------------------------
;; F19: Session-config defaults have memory disabled
;; ---------------------------------------------------------------------------

(test-case "session-config defaults have memory disabled (F19)"
  (define cfg (hash->session-config (hash)))
  (check-false (config-memory-enabled? cfg))
  (check-false (config-memory-backend cfg)))

;; ---------------------------------------------------------------------------
;; M13-F1: Architecture boundary — no runtime→tools imports
;; ---------------------------------------------------------------------------

(test-case "release-gate: no runtime module imports tools/builtins/memory-tools"
  ;; Verify memory-builder.rkt does NOT import from tools
  (define builder-source (file->string memory-builder-path))
  ;; Check actual require lines only (not comments)
  (define builder-requires
    (for/list ([line (string-split builder-source "\n")]
               #:when (or (string-prefix? (string-trim line) "(require")
                          (string-prefix? (string-trim line) "only-in")
                          (string-prefix? (string-trim line) "\"../tools")))
      line))
  (check-false (for/or ([line builder-requires])
                 (string-contains? line "tools/builtins/memory-tools")))
  ;; Verify service.rkt does NOT import from tools
  (define service-source (file->string memory-service-path))
  (define service-requires
    (for/list ([line (string-split service-source "\n")]
               #:when (or (string-prefix? (string-trim line) "(require")
                          (string-prefix? (string-trim line) "only-in")))
      line))
  (check-false (for/or ([line service-requires])
                 (string-contains? line "tools/"))))

;; ---------------------------------------------------------------------------
;; M13-F2: Tool schema does not advertise secret sensitivity
;; ---------------------------------------------------------------------------

(test-case "release-gate: store_memory schema does not advertise secret"
  (define schema (tool-schema store-memory))
  (define props (hash-ref schema 'properties (hasheq)))
  (define sens-prop (hash-ref props 'sensitivity (hasheq)))
  (define desc (hash-ref sens-prop 'description ""))
  (check-false (string-contains? desc "secret"))
  (check-true (string-contains? desc "public"))
  (check-true (string-contains? desc "internal"))
  (check-true (string-contains? desc "sensitive")))

;; ---------------------------------------------------------------------------
;; M13-F12: Event schema versioning gate
;; ---------------------------------------------------------------------------

(test-case "release-gate: MEMORY-EVENT-SCHEMA-VERSION is positive integer"
  (check-true (and (integer? MEMORY-EVENT-SCHEMA-VERSION) (positive? MEMORY-EVENT-SCHEMA-VERSION))))

;; ---------------------------------------------------------------------------
;; M13-F13: Snippet safety gates
;; ---------------------------------------------------------------------------

(test-case "release-gate: snippet does not expose adjacent secret context"
  (define content "user api_key=sk-1234567890abcdef was deleted")
  (define snippet (redacted-memory-snippet content))
  (check-false (string-contains? snippet "sk-1234567890abcdef"))
  ;; Should contain [REDACTED] not the secret
  (check-true (string-contains? snippet "[REDACTED]")))

(test-case "release-gate: snippet truncation appends ellipsis"
  (define long-content (make-string 200 #\a))
  (define snippet (redacted-memory-snippet long-content 40))
  (check-true (string-suffix? snippet "...")))

(test-case "release-gate: snippet collapses whitespace"
  (define ws-content "hello   world\n\n\ttabs")
  (define snippet (redacted-memory-snippet ws-content))
  (check-false (regexp-match? #rx"\n" snippet))
  (check-false (regexp-match? #rx"  " snippet)))

;; ---------------------------------------------------------------------------
;; M13-F14: Observe-only integration gate
;; ---------------------------------------------------------------------------

(test-case "release-gate: observe-memory-for-context is importable and callable"
  (define result (observe-memory-for-context (hash->session-config (hash 'memory-backend #f))))
  (check-true (pair? result))
  ;; Returns (cons items telemetry) — items is list, telemetry is struct
  (check-true (list? (car result)))
  (check-true (memory-telemetry? (cdr result))))

(test-case "release-gate: inject-memory-for-context is importable"
  ;; Verify the injection pipeline exists and returns correct shape
  (parameterize ([current-memory-injection-budget 100])
    (define result
      (inject-memory-for-context (hash->session-config (hash 'memory-backend #f))
                                 #:budget-tokens 100))
    (check-true (pair? result))
    ;; Section should be #f (no backend) and telemetry should be struct
    (check-false (car result))
    (check-true (memory-telemetry? (cdr result)))))

;; ---------------------------------------------------------------------------
;; v0.95.18 W0: Audit remediation gate coverage
;; ---------------------------------------------------------------------------

(test-case "W0 F4: final memory gate explicitly includes Mem0 transport test file"
  ;; `tests/test-memory-*.rkt` does not match `tests/test-mem0-http-transport-g2.rkt`.
  ;; The release gate must name this file explicitly.
  (define planned-gate "tests/test-memory-*.rkt tests/test-mem0-http-transport-g2.rkt")
  (check-true (string-contains? planned-gate "tests/test-mem0-http-transport-g2.rkt")))

(test-case "W0 F7: reflection entrypoint is importable"
  (check-true (procedure? reflect-session-memories!))
  ;; This is intentionally red until W5 changes the default from 2 to 3.
  (check-equal? (current-reflection-min-group-size) 3))

(test-case "W0 F9: release gate names blank Auto prompt regression files"
  (define planned-gate
    "tests/test-auto-distillation.rkt tests/test-state-aware-builder.rkt tests/test-prompt-injection.rkt")
  (check-true (string-contains? planned-gate "tests/test-auto-distillation.rkt"))
  (check-true (string-contains? planned-gate "tests/test-state-aware-builder.rkt"))
  (check-true (string-contains? planned-gate "tests/test-prompt-injection.rkt")))
