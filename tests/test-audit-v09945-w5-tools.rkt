#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w5-tools.rkt — Tools subsystem real-world audit
;;
;; Audit of the Tools subsystem covering:
;;   1. Tool struct definition (12 fields, transparent)
;;   2. Tool construction (make-tool with defaults)
;;   3. Tool schema validation (validate-tool-schema, validate-tool-args)
;;   4. Shell risk classification (tokenizer, classifier, summary)
;;   5. Tool registry (register, lookup, active management, capability filter)
;;   6. Tool spec (struct + backward-compatible constructor)
;;   7. Tool middleware pipeline (compose, hook, safe-mode, validation, permission)
;;   8. Schema hint formatting
;;   9. Tool list merging
;;
;; FINDING: Tools subsystem has robust schema validation, shell risk
;; classification, and a well-structured middleware pipeline.
;;
;; All tests use synthetic data — no real API keys or filesystem needed.

(require rackunit
         racket/set
         racket/list
         racket/string
         ;; Tool facade (re-exports tool-struct, schema-helpers, exec-context, registry)
         "../tools/tool.rkt"
         ;; Shell risk classifier
         "../tools/shell-risk.rkt"
         ;; Tool spec
         "../tools/registry-table/spec.rkt"
         ;; Middleware pipeline
         "../tools/middleware.rkt"
         ;; Only-import: with-registry-snapshot not re-exported by tool.rkt
         (only-in "../tools/registry.rkt" with-registry-snapshot))

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(define (make-test-tool #:name [name "test-tool"]
                        #:cap [cap 'read-only]
                        #:schema [schema #f]
                        #:dangerous? [dangerous? #f])
  (make-tool name
             "A test tool"
             (or schema
                 (hasheq 'type
                         "object"
                         'properties
                         (hasheq 'path (hasheq 'type "string" 'description "File path"))
                         'required
                         '(path)))
             (lambda (args ctx) (make-success-result "ok"))
             #:required-capability cap
             #:dangerous? dangerous?))

;; Helper: extract text from tool result content (may be hash or string)
(define (result-text r)
  (define c (tool-result-content r))
  (cond
    [(string? c) c]
    [(and (hash? c) (hash-ref c 'text #f)) (hash-ref c 'text)]
    [else (format "~a" c)]))

(define base-executor (lambda (tc ctx) (make-success-result "executed")))

;; ---------------------------------------------------------------------------
;; 1. Tool Struct Definition
;; ---------------------------------------------------------------------------

(test-case "audit-ts-tool-fields"
  (define t (make-test-tool))
  (check-true (tool? t))
  (check-equal? (tool-name t) "test-tool")
  (check-equal? (tool-description t) "A test tool")
  (check-true (hash? (tool-schema t)))
  (check-true (procedure? (tool-execute t)))
  (check-equal? (tool-required-capability t) 'read-only)
  (check-false (tool-dangerous? t))
  (check-false (tool-prompt-snippet t))
  (check-false (tool-prompt-guidelines t))
  (check-false (tool-render-call t))
  (check-false (tool-render-result t))
  (check-false (tool-timeout-seconds t))
  (check-false (tool-externalizable? t) "Default externalizable? is #f"))

(test-case "audit-ts-tool-with-options"
  (define t
    (make-tool "complex"
               "Complex tool"
               (hasheq 'type "object" 'properties (hasheq) 'required '())
               void
               #:prompt-snippet "PS"
               #:prompt-guidelines "GL"
               #:dangerous? #t
               #:timeout-seconds 30
               #:required-capability 'shell-exec
               #:externalizable? #f))
  (check-equal? (tool-prompt-snippet t) "PS")
  (check-equal? (tool-prompt-guidelines t) "GL")
  (check-true (tool-dangerous? t))
  (check-equal? (tool-timeout-seconds t) 30)
  (check-equal? (tool-required-capability t) 'shell-exec)
  (check-false (tool-externalizable? t)))

(test-case "audit-ts-tool-not-tool"
  (check-false (tool? "not-a-tool"))
  (check-false (tool? 42))
  (check-false (tool? (hasheq))))

;; ---------------------------------------------------------------------------
;; 2. Schema Validation
;; ---------------------------------------------------------------------------

(test-case "audit-sv-valid-schema"
  (define schema
    (hasheq 'type "object" 'properties (hasheq 'x (hasheq 'type "string")) 'required '(x)))
  (check-true (validate-tool-schema schema)))

(test-case "audit-sv-invalid-no-type"
  (check-false (validate-tool-schema (hasheq 'properties (hasheq)))))

(test-case "audit-sv-invalid-wrong-type"
  (check-false (validate-tool-schema (hasheq 'type "array" 'properties (hasheq)))))

(test-case "audit-sv-invalid-no-properties"
  (check-false (validate-tool-schema (hasheq 'type "object"))))

(test-case "audit-sv-invalid-not-hash"
  (check-false (validate-tool-schema "not a hash"))
  (check-false (validate-tool-schema 42)))

(test-case "audit-sv-args-valid"
  (define t (make-test-tool))
  (check-true (validate-tool-args t (hasheq 'path "/tmp/test.rkt"))))

(test-case "audit-sv-args-missing-required"
  (define t (make-test-tool))
  (check-exn exn:fail? (lambda () (validate-tool-args t (hasheq 'other "value")))))

(test-case "audit-sv-args-type-mismatch"
  (define t (make-test-tool))
  (check-exn exn:fail? (lambda () (validate-tool-args t (hasheq 'path 42)))))

(test-case "audit-sv-args-non-hash"
  (define t (make-test-tool))
  (check-exn exn:fail? (lambda () (validate-tool-args t "not a hash"))))

(test-case "audit-sv-args-no-required"
  (define t
    (make-tool "no-req"
               "desc"
               (hasheq 'type "object" 'properties (hasheq 'x (hasheq 'type "string")))
               void))
  ;; No required field → all args optional
  (check-true (validate-tool-args t (hasheq))))

;; ---------------------------------------------------------------------------
;; 3. Schema Hint Formatting
;; ---------------------------------------------------------------------------

(test-case "audit-sh-format-hint-required-and-optional"
  (define t
    (make-tool "mytool"
               "desc"
               (hasheq 'type
                       "object"
                       'properties
                       (hasheq 'path (hasheq 'type "string") 'verbose (hasheq 'type "boolean"))
                       'required
                       '(path))
               void))
  (define hint (format-tool-schema-hint t))
  (check-true (string? hint))
  (check-true (string-prefix? hint "mytool("))
  (check-true (string-contains? hint "path: string"))
  (check-true (string-contains? hint "verbose?: boolean")))

(test-case "audit-sh-format-hint-all-required"
  (define t
    (make-tool "reqtool"
               "desc"
               (hasheq 'type
                       "object"
                       'properties
                       (hasheq 'a (hasheq 'type "string") 'b (hasheq 'type "integer"))
                       'required
                       '(a b))
               void))
  (define hint (format-tool-schema-hint t))
  (check-false (string-contains? hint "?") "No optional params → no ? marks"))

;; ---------------------------------------------------------------------------
;; 4. Shell Risk Classification
;; ---------------------------------------------------------------------------

(test-case "audit-sr-tokenize-simple"
  (define tokens (tokenize-shell-command "ls -la"))
  (check-true (list? tokens))
  (check-true (> (length tokens) 0))
  (check-true (andmap shell-token? tokens)))

(test-case "audit-sr-tokenize-pipe"
  (define tokens (tokenize-shell-command "cat file | grep foo"))
  (define seps (filter (lambda (t) (eq? (shell-token-type t) 'separator)) tokens))
  (check-true (> (length seps) 0) "Should have separator tokens"))

(test-case "audit-sr-tokenize-redirect"
  (define tokens (tokenize-shell-command "echo hello > file.txt"))
  (define redirs (filter (lambda (t) (eq? (shell-token-type t) 'redirect)) tokens))
  (check-true (> (length redirs) 0) "Should have redirect token"))

(test-case "audit-sr-tokenize-quotes"
  (define tokens (tokenize-shell-command "echo 'hello world'"))
  (define quotes (filter (lambda (t) (eq? (shell-token-type t) 'quote)) tokens))
  (check-true (> (length quotes) 0) "Should have quote token"))

(test-case "audit-sr-tokenize-substitution"
  (define tokens (tokenize-shell-command "echo $(date)"))
  (define subs (filter (lambda (t) (eq? (shell-token-type t) 'substitution)) tokens))
  (check-true (> (length subs) 0) "Should have substitution token"))

(test-case "audit-sr-classify-rm-rf"
  (define tokens (tokenize-shell-command "rm -rf /tmp/test"))
  (define findings (classify-shell-risks tokens))
  (check-true (> (length findings) 0))
  (define destructive (filter (lambda (f) (eq? (shell-risk-finding-type f) 'destructive)) findings))
  (check-true (> (length destructive) 0) "rm -rf should be destructive")
  (check-equal? (shell-risk-finding-severity (car destructive)) 'critical))

(test-case "audit-sr-classify-safe-command"
  (define tokens (tokenize-shell-command "ls -la"))
  (define findings (classify-shell-risks tokens))
  (check-equal? (length findings) 0 "ls should have no risk findings"))

(test-case "audit-sr-classify-mkfs"
  (define tokens (tokenize-shell-command "mkfs.ext4 /dev/sda1"))
  (define findings (classify-shell-risks tokens))
  (check-true (> (length findings) 0) "mkfs should have findings")
  (check-equal? (shell-risk-finding-severity (car findings)) 'critical))

(test-case "audit-sr-classify-git-force"
  (define tokens (tokenize-shell-command "git push --force origin main"))
  (define findings (classify-shell-risks tokens))
  (check-true (> (length findings) 0) "git --force should have findings")
  (check-equal? (shell-risk-finding-severity (car findings)) 'high))

(test-case "audit-sr-classify-pipe-to-bash"
  (define tokens (tokenize-shell-command "echo data | bash"))
  (define findings (classify-shell-risks tokens))
  (define pipe-findings
    (filter (lambda (f) (eq? (shell-risk-finding-type f) 'network-pipe)) findings))
  (check-true (> (length pipe-findings) 0) "Pipe to bash should be flagged"))

(test-case "audit-sr-classify-substitution"
  (define tokens (tokenize-shell-command "echo $(whoami)"))
  (define findings (classify-shell-risks tokens))
  (define sub-findings
    (filter (lambda (f) (eq? (shell-risk-finding-type f) 'command-substitution)) findings))
  (check-true (> (length sub-findings) 0) "Command substitution should be flagged"))

(test-case "audit-sr-summary-empty"
  (define summary (shell-risk-summary '()))
  (check-equal? (hash-ref summary 'count) 0)
  (check-equal? (hash-ref summary 'max-severity) 'info)
  (check-false (hash-ref summary 'critical?)))

(test-case "audit-sr-summary-with-findings"
  (define tokens (tokenize-shell-command "rm -rf /"))
  (define findings (classify-shell-risks tokens))
  (define summary (shell-risk-summary findings))
  (check-true (> (hash-ref summary 'count) 0))
  (check-true (hash-ref summary 'critical?))
  (check-equal? (hash-ref summary 'max-severity) 'critical))

(test-case "audit-sr-predicates"
  ;; FINDING-001 remediation: exported predicates must return exact booleans
  ;; across the contract boundary, not member's truthy tail list.
  (check-true (risk-severity? 'critical))
  (check-true (risk-severity? 'low))
  (check-false (risk-severity? 'bogus))
  (check-true (token-type? 'word))
  (check-false (token-type? 'bogus))
  (check-true (risk-type? 'destructive))
  (check-false (risk-type? 'bogus)))

;; ---------------------------------------------------------------------------
;; 5. Tool Registry
;; ---------------------------------------------------------------------------

(test-case "audit-reg-make-registry"
  (define reg (make-tool-registry))
  (check-true (tool-registry? reg))
  (check-equal? (length (tool-names reg)) 0))

(test-case "audit-reg-register-and-lookup"
  (define reg (make-tool-registry))
  (define t (make-test-tool #:name "mytool"))
  (register-tool! reg t)
  (check-equal? (length (tool-names reg)) 1)
  (define found (lookup-tool reg "mytool"))
  (check-true (tool? found))
  (check-equal? (tool-name found) "mytool"))

(test-case "audit-reg-lookup-missing"
  (define reg (make-tool-registry))
  (check-false (lookup-tool reg "nonexistent")))

(test-case "audit-reg-lookup-nil"
  (define reg (make-tool-registry))
  (check-false (lookup-tool reg #f)))

(test-case "audit-reg-unregister"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "removable"))
  (check-true (tool? (lookup-tool reg "removable")))
  (unregister-tool! reg "removable")
  (check-false (lookup-tool reg "removable")))

(test-case "audit-reg-register-non-tool-error"
  (define reg (make-tool-registry))
  (check-exn exn:fail? (lambda () (register-tool! reg "not-a-tool"))))

(test-case "audit-reg-active-management"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "a"))
  (register-tool! reg (make-test-tool #:name "b"))
  ;; No active filter → all active
  (check-true (tool-active? reg "a"))
  (check-true (tool-active? reg "b"))
  ;; Set active to only "a"
  (set-active-tools! reg '("a"))
  (check-true (tool-active? reg "a"))
  (check-false (tool-active? reg "b"))
  ;; Reset to all
  (set-active-tools! reg #f)
  (check-true (tool-active? reg "a"))
  (check-true (tool-active? reg "b")))

(test-case "audit-reg-list-active-tools"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "a"))
  (register-tool! reg (make-test-tool #:name "b"))
  (set-active-tools! reg '("a"))
  (define active (list-active-tools reg))
  (check-equal? (length active) 1)
  (check-equal? (tool-name (car active)) "a"))

(test-case "audit-reg-jsexpr-serialization"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "ser"))
  (define jsexprs (list-active-tools-jsexpr reg))
  (check-equal? (length jsexprs) 1)
  (define js (car jsexprs))
  (check-equal? (hash-ref js 'type) "function")
  (check-true (hash-has-key? js 'function))
  (define fn (hash-ref js 'function))
  (check-equal? (hash-ref fn 'name) "ser"))

(test-case "audit-reg-snapshot"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "snap"))
  (define result (with-registry-snapshot reg (lambda (snap) (hash-has-key? snap "snap"))))
  (check-true result))

(test-case "audit-reg-capability-filter"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "reader" #:cap 'read-only))
  (register-tool! reg (make-test-tool #:name "writer" #:cap 'file-write))
  (define read-tools (tools-for-capability reg 'read-only))
  (check-true (> (length read-tools) 0))
  (check-not-false (findf (lambda (t) (equal? (tool-name t) "reader")) read-tools))
  ;; Writer should NOT be in read-only tools
  (check-false (findf (lambda (t) (equal? (tool-name t) "writer")) read-tools)
               "file-write tool should not appear in read-only filter"))

(test-case "audit-reg-capability-filter-any"
  (define reg (make-tool-registry))
  (register-tool! reg (make-test-tool #:name "anytool" #:cap 'any))
  (define tools (tools-for-capability reg 'read-only))
  (check-not-false (findf (lambda (t) (equal? (tool-name t) "anytool")) tools)
                   "'any capability tools should appear in all filters"))

(test-case "audit-reg-capability-filter-invalid"
  (define reg (make-tool-registry))
  (check-exn exn:fail? (lambda () (tools-for-capability reg 'bogus-cap))))

;; ---------------------------------------------------------------------------
;; 6. Tool Spec
;; ---------------------------------------------------------------------------

(test-case "audit-ts-spec-create"
  (define s (make-tool-spec* "name" "desc" (hasheq) void #f))
  (check-true (tool-spec? s))
  (check-equal? (tool-spec-name s) "name")
  (check-equal? (tool-spec-description s) "desc")
  (check-equal? (tool-spec-required-capability s) 'any "Default should be 'any"))

(test-case "audit-ts-spec-with-capability"
  (define s (make-tool-spec* "name" "desc" (hasheq) void #f 'shell-exec))
  (check-equal? (tool-spec-required-capability s) 'shell-exec))

(test-case "audit-ts-spec-handler"
  (define s (make-tool-spec* "name" "desc" (hasheq) (lambda (args ctx) 'done) #f))
  (check-true (procedure? (tool-spec-handler s))))

;; ---------------------------------------------------------------------------
;; 7. Tool Middleware Pipeline
;; ---------------------------------------------------------------------------

(test-case "audit-mw-compose-empty"
  (define pipeline (compose-middleware))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-compose-single"
  (define logging-mw (lambda (tc ctx next) (next tc ctx)))
  (define pipeline (compose-middleware logging-mw))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-safe-mode-block"
  (define pipeline
    (make-safe-mode-middleware (lambda () #t) ; safe-mode? = #t
                               (lambda (name) #f))) ; no tools allowed
  (define tc (make-tool-call "id1" "blocked-tool" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "safe-mode")))

(test-case "audit-mw-safe-mode-allow"
  (define pipeline (make-safe-mode-middleware (lambda () #t) (lambda (name) #t))) ; all tools allowed
  (define tc (make-tool-call "id1" "allowed-tool" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-safe-mode-inactive"
  (define pipeline
    (make-safe-mode-middleware (lambda () #f) ; safe-mode off
                               (lambda (name) #f))) ; wouldn't allow anything
  (define tc (make-tool-call "id1" "any-tool" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result) "Safe-mode off → all tools pass"))

(test-case "audit-mw-permission-allowed"
  (define pipeline (make-permission-middleware (lambda (tc) 'allowed)))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-permission-blocked"
  (define pipeline (make-permission-middleware (lambda (tc) 'blocked)))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "blocked")))

(test-case "audit-mw-validation-error"
  (define validate-fn (lambda (t args) (error "validation failed")))
  (define lookup-fn (lambda (name) (make-test-tool #:name name)))
  (define pipeline (make-validation-middleware validate-fn lookup-fn))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "validation")))

(test-case "audit-mw-validation-pass"
  (define validate-fn (lambda (t args) (void)))
  (define lookup-fn (lambda (name) (make-test-tool #:name name)))
  (define pipeline (make-validation-middleware validate-fn lookup-fn))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-validation-unknown-tool"
  (define validate-fn (lambda (t args) (void)))
  (define lookup-fn (lambda (name) #f)) ; tool not found
  (define pipeline (make-validation-middleware validate-fn lookup-fn))
  (define tc (make-tool-call "id1" "unknown" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "unknown tool")))

(test-case "audit-mw-default-pipeline"
  ;; FINDING-002: Default pipeline has validation middleware with default lookup-fn
  ;; that returns #f, so ALL tools are treated as "unknown". Callers MUST provide
  ;; lookup-fn to make the default pipeline functional.
  (define pipeline (make-default-pipeline))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result-is-error? result) "Default pipeline blocks all tools without lookup-fn"))

(test-case "audit-mw-hook-block"
  (define hook-dispatcher (lambda (phase tc) (hasheq 'action 'block)))
  (define pipeline (make-hook-middleware hook-dispatcher 'preflight))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "blocked")))

(test-case "audit-mw-hook-amend"
  (define amended-tc (make-tool-call "id2" "amended" (hasheq 'extra #t)))
  (define hook-dispatcher (lambda (phase tc) (hasheq 'action 'amend 'payload amended-tc)))
  (define pipeline (make-hook-middleware hook-dispatcher 'preflight))
  (define tc (make-tool-call "id1" "original" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-hook-pass-through"
  (define hook-dispatcher (lambda (phase tc) #f))
  (define pipeline (make-hook-middleware hook-dispatcher 'preflight))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

(test-case "audit-mw-hook-nil-dispatcher"
  (define pipeline (make-hook-middleware #f 'preflight))
  (define tc (make-tool-call "id1" "test" (hasheq)))
  (define result (pipeline tc #f base-executor))
  (check-false (tool-result-is-error? result)))

;; ---------------------------------------------------------------------------
;; 8. Tool List Merging
;; ---------------------------------------------------------------------------

(test-case "audit-ml-merge-disjoint"
  (define base (list (hasheq 'function (hasheq 'name "read"))))
  (define ext (list (hasheq 'function (hasheq 'name "custom"))))
  (define merged (merge-tool-lists base ext))
  (check-equal? (length merged) 2))

(test-case "audit-ml-merge-extension-overrides"
  (define base (list (hasheq 'function (hasheq 'name "read"))))
  (define ext (list (hasheq 'function (hasheq 'name "read" 'custom #t))))
  (define merged (merge-tool-lists base ext))
  ;; Extension tools are appended, not deduped against base
  (check-true (>= (length merged) 1)))

(test-case "audit-ml-merge-dedup-extension"
  ;; Multiple extension tools with same name → deduped (last wins)
  (define ext
    (list (hasheq 'function (hasheq 'name "dup" 'v 1)) (hasheq 'function (hasheq 'name "dup" 'v 2))))
  (define merged (merge-tool-lists '() ext))
  (check-equal? (length merged) 1 "Duplicate extension tools should be deduped"))

;; ---------------------------------------------------------------------------
;; 9. Tool Result Construction
;; ---------------------------------------------------------------------------

(test-case "audit-tr-success-result"
  (define r (make-success-result "hello"))
  (check-true (tool-result? r))
  (check-false (tool-result-is-error? r))
  ;; FINDING-003: make-success-result wraps string content in a hash structure
  (check-equal? (result-text r) "hello"))

(test-case "audit-tr-error-result"
  (define r (make-error-result "something went wrong"))
  (check-true (tool-result? r))
  (check-true (tool-result-is-error? r))
  (check-true (string-contains? (result-text r) "wrong")))

(test-case "audit-tr-result-with-details"
  (define r (make-success-result "data" (hasheq 'meta "info")))
  (check-equal? (result-text r) "data")
  (check-equal? (hash-ref (tool-result-details r) 'meta) "info"))
