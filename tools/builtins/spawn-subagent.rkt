#lang racket/base

;; Subagent spawning orchestration with isolated context and scoped tools.
;; #1193, #1203/#1204: child execution and parent-provider injection.

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../../tools/tool.rkt"
         (only-in "../../util/capability.rkt" valid-capability? current-session-capabilities)
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt" safe-mode? allowed-tool?)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! make-event-bus)
         (only-in "../../runtime/settings.rkt" q-settings? setting-ref)
         (only-in "../../runtime/auto-retry.rkt" with-auto-retry)
         "../model-bridge.rkt"
         (only-in "provider-hash-bridge.rkt" messages->provider-hashes)
         "../../util/ids.rkt"
         (only-in "../../util/content/content-parts.rkt"
                  make-text-part
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-call-part
                  make-tool-result-part
                  tool-result-part-tool-call-id
                  tool-result-part-content
                  tool-result-part-is-error?
                  tool-result-part?
                  text-part?
                  text-part-text
                  content-part?)
         (only-in "../../util/message/message.rkt"
                  make-message
                  message-role
                  message-content
                  message-id)
         (only-in "../../util/tool/tool-types.rkt" make-tool-call tool-call-id tool-result-content)
         (only-in "../../util/tool/tool-types.rkt" tool-result-is-error?)
         (only-in "../../util/json/json-helpers.rkt" ensure-hash-args)
         (only-in "../../util/error/error-sanitizer.rkt" sanitize-error-message)
         "../../tools/scheduler.rkt"
         ;; Individual builtins for child-safe tool registration (no circular dep)
         (only-in "../builtins/read.rkt" tool-read)
         (only-in "../builtins/write.rkt" tool-write)
         (only-in "../builtins/edit.rkt" tool-edit)
         (only-in "../builtins/bash.rkt" tool-bash)
         (only-in "../builtins/grep.rkt" tool-grep)
         (only-in "../builtins/find.rkt" tool-find)
         (only-in "../builtins/ls.rkt" tool-ls)
         (only-in "../builtins/skill-router.rkt" tool-skill-route)
         ;; v0.99.21 §4.3: Blackboard context injection for subagents
         (only-in "../../runtime/context-assembly/blackboard-context.rkt"
                  build-blackboard-context-snippet)
         ;; W2 v0.99.35: Pure helpers extracted for testability
         "spawn-subagent-helpers.rkt"
         (prefix-in approval: "spawn-approval.rkt")
         (prefix-in rate: "spawn-rate-limit.rkt")
         (only-in "../../util/cancellation.rkt" cancellation-token-cancelled?)
         (only-in "../../util/message/provider-transport.rkt"
                  provider-tool-call-type?
                  provider-tool-stop-reason?
                  provider-completion-stop-reason?))

(provide (contract-out [resolve-role-prompt (-> (or/c string? #f) string?)]
                       [parse-subagent-config (-> any/c subagent-config?)])
         ;; Tool entries (direct — used by scheduler)
         tool-spawn-subagent
         tool-spawn-subagents
         ;; Child-safe tool list (for testing and capability facade)
         child-safe-tools
         ;; Struct (direct for match compatibility)
         subagent-config
         subagent-config?
         subagent-config-task
         subagent-config-role
         subagent-config-max-turns
         subagent-config-tools
         subagent-config-model
         subagent-config-capabilities
         ;; v0.99.21 §4.2: Capability-aware tool filtering
         child-safe-tools-filtered
         ;; v0.99.21 §4.3: Blackboard context injection for subagents
         build-subagent-blackboard-context
         ;; v0.99.22 A-2: Batch capabilities parsing
         parse-job-capabilities
         ;; v0.99.23 §5.3: HITL approval for dangerous spawns
         requires-hitl-approval?
         request-spawn-approval
         request-batch-spawn-approval
         current-spawn-approval-result
         run-subagent-with-config
         ;; v0.99.23 §5.1: Session-wide agent pool limit
         current-agent-pool-limit
         ;; W1: deterministic rate-boundary testing/introspection
         current-spawn-timestamps
         ;; W2 v0.99.35: Re-export pure helpers for backward compat
         normalize-capabilities
         extract-assistant-text
         extract-text-summary
         ;; v0.99.50 W1 (TMUX-04): Typed terminal outcomes and safe metadata
         classify-terminal-status
         make-safe-result-metadata
         result-has-content?)

;; Subagent configuration struct — replaces ad-hoc hash extraction
;; v0.99.21 §4.2: Added capabilities field (6th, default #f for backward compat)
(struct subagent-config (task role max-turns tools model capabilities) #:transparent)

;; v0.99.23 §5.1: Session-wide agent pool limit parameter.
;; Default 3 (matching the previous hardcoded max). CLI --agent-pool overrides.
(define current-agent-pool-limit (make-parameter 3))

;; Re-exported compatibility handles owned by focused boundaries.
(define current-spawn-timestamps rate:current-spawn-timestamps)
(define spawn-rate-capacity? rate:spawn-rate-capacity?)
(define reserve-spawn-rate! rate:reserve-spawn-rate!)
(define current-spawn-approval-result approval:current-spawn-approval-result)

;; W2 v0.99.35: requires-hitl-approval? moved to spawn-subagent-helpers.rkt
;; (pure function — no I/O, no mutation)

;; Focused approval transport owns correlation, preview safety, and headless policy.
(define request-spawn-approval approval:request-spawn-approval)
(define request-batch-spawn-approval approval:request-batch-spawn-approval)

(define (make-denied-spawn-result message)
  (make-tool-result (list (hasheq 'type "text" 'text message))
                    (hasheq 'terminal-status "denied" 'child-created? #f)
                    #t))

;; Default role prompt when no role is specified
(define default-role-prompt
  "You are a focused assistant executing a specific delegated task. Complete the task efficiently and return the result.")

;; Resolve a role value: if it matches a skill name, load that skill as the prompt.
;; Otherwise use it as a literal prompt string.
(define (resolve-role-prompt role-value)
  (if (string? role-value)
      (let* ([result (tool-skill-route (hasheq 'action "context" 'name role-value))])
        (if (and (tool-result? result) (not (tool-result-is-error? result)))
            ;; Skill found — use its content as the role prompt
            (let ([content (hash-ref (car (tool-result-content result)) 'text "")])
              (if (non-empty-string? content) content default-role-prompt))
            ;; Not a skill name — use as literal prompt
            (if (non-empty-string? role-value) role-value default-role-prompt)))
      default-role-prompt))

(define (validate-tool-allowlist tools who)
  (unless (or (not tools) (and (list? tools) (andmap string? tools)))
    (raise-argument-error who "#f or a list of tool-name strings" tools))
  (when tools
    (define available (map tool-name (child-safe-tools)))
    (for ([name (in-list tools)])
      (unless (member name available)
        (raise-arguments-error who "unknown or unsafe child tool" "tool" name))))
  tools)

;; The spawn-subagent tool implementation
(define (tool-spawn-subagent args [exec-ctx #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "spawn-subagent failed: ~a"
                                                          (exn-message e))))])
    ;; Validate the complete request before consuming rate budget or performing
    ;; approval/provider/session effects.
    (define cfg (parse-subagent-config args))
    (run-subagent-with-config cfg exec-ctx)))

;; Parse subagent-config from tool call args hash.
;; v0.99.21 §4.2: Added capabilities parsing.
;; W2 v0.99.35: Uses normalize-capabilities from helpers for deduplicated parsing.
(define (parse-subagent-config args)
  (unless (hash? args)
    (raise-argument-error 'parse-subagent-config "hash?" args))
  (define task (hash-ref args 'task #f))
  (unless (and (string? task) (not (string=? (string-trim task) "")))
    (raise-argument-error 'parse-subagent-config "a non-empty task string" task))
  (define role (hash-ref args 'role default-role-prompt))
  (unless (string? role)
    (raise-argument-error 'parse-subagent-config "a role string" role))
  (define max-turns (hash-ref args 'max-turns 10))
  (unless (and (exact-integer? max-turns) (positive? max-turns))
    (raise-argument-error 'parse-subagent-config "a positive exact max-turns integer" max-turns))
  (define tools (validate-tool-allowlist (hash-ref args 'tools #f) 'parse-subagent-config))
  (define model (hash-ref args 'model #f))
  (unless (or (not model) (string? model))
    (raise-argument-error 'parse-subagent-config "#f or a model string" model))
  (define caps
    (if (hash-has-key? args 'capabilities)
        (normalize-capabilities/strict (hash-ref args 'capabilities))
        #f))
  (subagent-config (immutable-string task)
                   (immutable-string role)
                   max-turns
                   (and tools (map immutable-string tools))
                   (immutable-string model)
                   caps))

;; Execute subagent using typed config struct.
;; v0.99.23 §5.3: HITL approval gate for dangerous capabilities.
(define (run-subagent-with-config cfg exec-ctx)
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "invalid subagent configuration: ~a"
                                                          (exn-message e))))])
    (define task (immutable-string (subagent-config-task cfg)))
    (unless (and task (not (string=? (string-trim task) "")))
      (error 'run-subagent-with-config "task is required"))
    (define role (subagent-config-role cfg))
    (unless (string? role)
      (error 'run-subagent-with-config "role must be a string"))
    (define max-turns (subagent-config-max-turns cfg))
    (unless (and (exact-integer? max-turns) (positive? max-turns))
      (error 'run-subagent-with-config "max-turns must be a positive exact integer"))
    (define tools (validate-tool-allowlist (subagent-config-tools cfg) 'run-subagent-with-config))
    (define model (subagent-config-model cfg))
    (unless (or (not model) (string? model))
      (error 'run-subagent-with-config "model must be #f or a string"))
    (define frozen-role (immutable-string role))
    (define frozen-tools (and tools (map immutable-string tools)))
    (define frozen-model (immutable-string model))
    (define declared-caps
      (if (eq? (subagent-config-capabilities cfg) #f)
          #f
          (normalize-capabilities/strict (subagent-config-capabilities cfg))))
    (define caps (bounded-delegated-capabilities declared-caps (current-session-capabilities)))
    (cond
      [(not (spawn-rate-capacity? 1)) (make-error-result "spawn rate limit exceeded (30/min)")]
      [(and (requires-hitl-approval? caps) (not (request-spawn-approval caps task exec-ctx)))
       (make-denied-spawn-result "subagent spawn blocked — HITL approval denied")]
      [(not (reserve-spawn-rate! 1)) (make-error-result "spawn rate limit exceeded (30/min)")]
      [else
       ;; Role/skill resolution is deliberately after approval; it may load
       ;; resources and therefore must not run for denied work.
       (define role-prompt (resolve-role-prompt frozen-role))
       (run-subagent (hasheq 'task
                             task
                             'role
                             role-prompt
                             'max-turns
                             max-turns
                             'tools
                             frozen-tools
                             'model
                             frozen-model
                             'capabilities
                             caps)
                     role-prompt
                     max-turns
                     frozen-tools
                     exec-ctx)])))

;; Resolve the provider from exec-context runtime-settings.
;; Returns the real provider if available, or falls back to a mock.
;; Extract a value from runtime-settings which may be a q-settings struct or a plain hash.
;; v0.45.21 N3: Replaced struct->vector reflection with proper setting-ref API.
(define (rt-settings-ref rt key [default #f])
  (cond
    [(hash? rt) (hash-ref rt key default)]
    [(q-settings? rt) (setting-ref rt key default)]
    [else default]))

;; #1204: Provider injection from parent session.
;; BUG: make-minimal-settings stores provider under 'default-provider,
;; not 'provider. Must check both keys to avoid silent mock fallback.
(define (resolve-provider exec-ctx)
  (define rt (and exec-ctx (exec-context-runtime-settings exec-ctx)))
  (define prov
    (or (and rt (rt-settings-ref rt 'provider #f))
        (and rt (rt-settings-ref rt 'default-provider #f))))
  (cond
    [(provider? prov) prov]
    ;; Settings loaded from config.json store provider names as strings.
    ;; Never pass those names directly to provider-send; if the parent runtime
    ;; did not inject a provider struct, fall back safely to the mock provider.
    [(string? prov) (build-mock-provider-for-subagent)]
    [else (build-mock-provider-for-subagent)]))

;; Resolve model name from exec-context or fall back to "mock-model".
;; BUG: make-minimal-settings stores model under 'default-model, not 'model.
(define (resolve-model-name exec-ctx args)
  (or (hash-ref args 'model #f)
      (let ([rt (and exec-ctx (exec-context-runtime-settings exec-ctx))])
        (or (and rt (rt-settings-ref rt 'model #f)) (and rt (rt-settings-ref rt 'default-model #f))))
      "mock-model"))

;; Child-safe tools: read-only + safe write/edit/bash for subagent children.
;; Each tool is annotated with required-capability for MAS filtering.
;; Schemas match those in registry-defaults.rkt.
(define (child-safe-tools)
  (list (make-tool "read"
                   "Read file contents"
                   (hasheq 'type
                           "object"
                           'required
                           '("path")
                           'properties
                           (hasheq 'path
                                   (hasheq 'type "string" 'description "Path to file")
                                   'offset
                                   (hasheq 'type "integer" 'description "Line offset")
                                   'limit
                                   (hasheq 'type "integer" 'description "Max lines")))
                   tool-read
                   #:required-capability 'read-only)
        (make-tool "write"
                   "Write content to a file"
                   (hasheq 'type
                           "object"
                           'required
                           '("path" "content")
                           'properties
                           (hasheq 'path
                                   (hasheq 'type "string" 'description "Path to file")
                                   'content
                                   (hasheq 'type "string" 'description "Content to write")))
                   tool-write
                   #:required-capability 'file-write)
        (make-tool "edit"
                   "Edit a file with exact text replacement"
                   (hasheq 'type
                           "object"
                           'required
                           '("path" "edits")
                           'properties
                           (hasheq 'path
                                   (hasheq 'type "string" 'description "Path to file")
                                   'edits
                                   (hasheq 'type "array" 'description "Edit operations")))
                   tool-edit
                   #:required-capability 'file-write)
        (make-tool "bash"
                   "Execute a bash command"
                   (hasheq 'type
                           "object"
                           'required
                           '("command")
                           'properties
                           (hasheq 'command
                                   (hasheq 'type "string" 'description "Bash command")
                                   'timeout
                                   (hasheq 'type "integer" 'description "Timeout in seconds")))
                   tool-bash
                   #:required-capability 'shell-exec)
        (make-tool "grep"
                   "Search file contents with regex"
                   (hasheq 'type
                           "object"
                           'required
                           '("pattern")
                           'properties
                           (hasheq 'pattern
                                   (hasheq 'type "string" 'description "Regex pattern")
                                   'path
                                   (hasheq 'type "string" 'description "File or directory path")))
                   tool-grep
                   #:required-capability 'read-only)
        (make-tool "find"
                   "Find files by name pattern"
                   (hasheq 'type
                           "object"
                           'required
                           '("pattern")
                           'properties
                           (hasheq 'pattern
                                   (hasheq 'type "string" 'description "Name pattern")
                                   'path
                                   (hasheq 'type "string" 'description "Search directory")))
                   tool-find
                   #:required-capability 'read-only)
        (make-tool "ls"
                   "List directory contents"
                   (hasheq 'type
                           "object"
                           'required
                           '("path")
                           'properties
                           (hasheq 'path (hasheq 'type "string" 'description "Directory path")))
                   tool-ls
                   #:required-capability 'read-only)))

;; M5: Validate all child-safe tools have valid capability annotations.
;; Runs at module load to catch annotation bugs early.
(for ([t (in-list (child-safe-tools))])
  (unless (valid-capability? (tool-required-capability t))
    (error 'child-safe-tools
           "tool ~a has invalid capability: ~a"
           (tool-name t)
           (tool-required-capability t))))

;; v0.99.21 §4.2: Capability-aware tool filtering for subagent children.
;; Returns child-safe tools filtered by the given capabilities list.
;; - When capabilities is #f or empty, returns ALL child-safe tools (backward compat).
;; - When capabilities is a list of symbols, returns only tools whose
;;   required-capability is 'any or is in the capabilities list.
(define (child-safe-tools-filtered capabilities)
  (define all-tools (child-safe-tools))
  (cond
    [(not capabilities) all-tools]
    [(null? capabilities) '()]
    [(memq 'any capabilities)
     (error 'child-safe-tools-filtered "delegated any wildcard is not permitted")]
    [else
     (filter (lambda (t)
               (define rc (tool-required-capability t))
               (or (eq? rc 'any) (memq rc capabilities)))
             all-tools)]))

(define (effective-child-tool-names capabilities)
  (for/list ([child-tool (in-list (child-safe-tools-filtered capabilities))]
             #:when (or (not (safe-mode?)) (allowed-tool? (tool-name child-tool))))
    (tool-name child-tool)))

;; v0.99.21 §4.3: Build blackboard context string for subagent injection.
;; Reads the parent session's blackboard via current-blackboard parameter.
;; Returns a formatted string with '## Parent Session Context' header when
;; the blackboard has content, or "" when no blackboard is available.
;; Uses the existing build-blackboard-context-snippet which handles:
;;   - #f when no blackboard or empty
;;   - Token budget guard (capped at 500 chars)
(define (build-subagent-blackboard-context)
  (define snippet (build-blackboard-context-snippet))
  (if (and snippet (> (string-length snippet) 0))
      (string-append "## Parent Session Context\n" snippet "\n")
      ""))

;; Internal: run the subagent
(define (run-subagent args role-prompt max-turns allowed-tools exec-ctx)
  (define task (hash-ref args 'task))
  (define failure-terminalizer (box #f))
  (with-handlers ([exn:fail? (lambda (error-value)
                               (define terminalize-failure (unbox failure-terminalizer))
                               (if terminalize-failure
                                   (terminalize-failure error-value)
                                   (make-error-result (sanitize-error-message
                                                       (format "subagent failed: ~a"
                                                               (exn-message error-value))))))])
    ;; #1204: Resolve real provider from parent's runtime-settings
    (define provider (resolve-provider exec-ctx))
    (define model-name (resolve-model-name exec-ctx args))

    ;; Create scoped tool registry with child-safe tools
    ;; v0.99.21 §4.2: Filter tools by capabilities when specified
    (define capabilities (hash-ref args 'capabilities #f))
    (define registry (make-tool-registry))
    ;; Capabilities set the maximum authority; an explicit tool allowlist can
    ;; only narrow that set and is enforced at registry construction.
    (define capability-tools
      (filter (lambda (child-tool) (or (not (safe-mode?)) (allowed-tool? (tool-name child-tool))))
              (child-safe-tools-filtered capabilities)))
    (define selected-tools
      (if allowed-tools
          (filter (lambda (child-tool) (member (tool-name child-tool) allowed-tools))
                  capability-tools)
          capability-tools))
    (for ([tool (in-list selected-tools)])
      (register-tool! registry tool))

    ;; Build child session. Batch execution plans allocate these identities
    ;; before approval; single-spawn callers retain generated defaults.
    (define session-id (hash-ref args 'planned-session-id generate-id))
    (define child-id (hash-ref args 'planned-child-id generate-id))
    (define tool-call-id (hash-ref args 'planned-tool-call-id generate-id))
    (define terminal-emitted? (box #f))
    (define child-parent-publisher (and exec-ctx (exec-context-event-publisher exec-ctx)))
    (define (terminalize! terminal-status raw-status result-text turns-used)
      (define base-meta (make-safe-result-metadata result-text session-id terminal-status raw-status))
      (define terminal-meta
        (hash-set* base-meta
                   'turns-used
                   turns-used
                   'child-id
                   child-id
                   'tool-call-id
                   tool-call-id
                   'parent-call-id
                   (if exec-ctx
                       (exec-context-call-id exec-ctx)
                       "")))
      (unless (unbox terminal-emitted?)
        (set-box! terminal-emitted? #t)
        (when child-parent-publisher
          (with-handlers ([exn:fail? void])
            (child-parent-publisher "subagent.terminal" terminal-meta))))
      terminal-meta)
    (set-box! failure-terminalizer
              (lambda (error-value)
                (define terminal-meta (terminalize! 'failed 'failed "" 0))
                (make-tool-result
                 (list (hasheq 'type
                               "text"
                               'text
                               (sanitize-error-message (format "subagent failed: ~a"
                                                               (exn-message error-value)))))
                 terminal-meta
                 #t)))
    (define bus (make-event-bus))
    (define settings (hasheq 'model model-name))

    ;; Create execution context for child
    (define child-ctx
      (make-exec-context
       #:working-directory (if exec-ctx
                               (exec-context-working-directory exec-ctx)
                               (current-directory))
       #:cancellation-token (and exec-ctx (exec-context-cancellation-token exec-ctx))
       #:permission-config (and exec-ctx (exec-context-permission-config exec-ctx))
       #:event-publisher (lambda (event-type payload)
                           (emit-session-event! bus session-id event-type payload))
       #:runtime-settings settings
       #:call-id tool-call-id
       #:session-metadata (hasheq 'session-id session-id 'child-id child-id 'role "subagent")))

    ;; v0.99.21 §4.3: Inject parent blackboard context into subagent system prompt
    (define bb-context (build-subagent-blackboard-context))
    (define effective-role-prompt
      (if (> (string-length bb-context) 0)
          (string-append bb-context "\n" role-prompt)
          role-prompt))

    ;; Build messages for child
    (define system-msg
      (make-message (generate-id)
                    #f
                    'system
                    'message
                    (list (make-text-part effective-role-prompt))
                    (current-seconds)
                    (hasheq)))
    (define user-msg
      (make-message (generate-id)
                    #f
                    'user
                    'message
                    (list (make-text-part task))
                    (current-seconds)
                    (hasheq)))

    ;; Run the child loop and preserve its exact terminal status and actual
    ;; provider-turn count through the tool boundary.
    (define-values (result-messages final-status turns-used)
      (run-subagent-loop provider registry (list system-msg user-msg) child-ctx max-turns))
    (define result-text (extract-assistant-text result-messages))
    (define has-content? (result-has-content? result-text))
    (define terminal-status (classify-terminal-status final-status has-content?))
    (define terminal-meta (terminalize! terminal-status final-status result-text turns-used))
    (if (terminal-status-success? terminal-status)
        (make-success-result (list (hasheq 'type "text" 'text result-text)) terminal-meta)
        (make-tool-result (list (hasheq 'type
                                        "text"
                                        'text
                                        (case terminal-status
                                          [(timed-out) "subagent timed out."]
                                          [(cancelled) "subagent cancelled."]
                                          [else "subagent failed."])))
                          terminal-meta
                          #t))))

;; ============================================================
;; Message-to-provider conversion
;; ============================================================

;; v0.99.50 W1 (TMUX-04): Duplicate provider conversion removed.
;; Child message conversion delegates through provider-hash-bridge.rkt to the
;; neutral provider-transport owner. Keeping one canonical wire representation
;; prevents parent/child and adapter request shapes from diverging.

;; Run a simple agent loop for the subagent
;; v0.19.4 GAP-1 fix: actually dispatch tool_calls instead of returning 'stopped
(define (run-subagent-loop provider registry messages ctx max-turns)
  (define token (exec-context-cancellation-token ctx))
  (define (cancelled?)
    (and token (cancellation-token-cancelled? token)))
  (let loop ([msgs messages]
             [turns-remaining max-turns]
             [all-results '()]
             [turns-used 0])
    (cond
      [(cancelled?) (values all-results 'cancelled turns-used)]
      [(<= turns-remaining 0) (values all-results 'max-turns-reached turns-used)]
      [else
       (define current-turn (add1 turns-used))
       (with-handlers ([exn:fail?
                        (lambda (_)
                          (values all-results (if (cancelled?) 'cancelled 'failed) current-turn))])
         (define provider-msgs (messages->provider-hashes msgs))
         (define req (make-model-request provider-msgs (list-active-tools-jsexpr registry) (hasheq)))
         (define resp
           (with-auto-retry (lambda ()
                              (when (cancelled?)
                                (error 'subagent-cancelled "child cancellation requested"))
                              (provider-send provider req))
                            #:on-retry (lambda (_attempt _maximum _delay _message _category)
                                         (when (cancelled?)
                                           (error 'subagent-cancelled
                                                  "child cancellation requested")))
                            #:per-type-budgets (hash 'timeout 2 'rate-limit 4 'provider-error 2)))
         (if (cancelled?)
             (values all-results 'cancelled current-turn)
             (let* ([content (model-response-content resp)]
                    [content-parts
                     (filter-map
                      (lambda (item)
                        (match (hash-ref item 'type #f)
                          ["text" (make-text-part (hash-ref item 'text ""))]
                          [(? provider-tool-call-type?)
                           (make-tool-call-part (hash-ref item 'id (generate-id))
                                                (hash-ref item 'name "")
                                                (ensure-hash-args (hash-ref item 'arguments "{}")))]
                          [_ #f]))
                      content)]
                    [assistant-msg (make-message (generate-id)
                                                 #f
                                                 'assistant
                                                 'message
                                                 content-parts
                                                 (current-seconds)
                                                 (hasheq))]
                    [new-all (append all-results (list assistant-msg))]
                    [stop-reason (model-response-stop-reason resp)])
               (cond
                 [(provider-tool-stop-reason? stop-reason)
                  (define tool-calls
                    (for/list ([part (in-list content-parts)]
                               #:when (tool-call-part? part))
                      (make-tool-call (tool-call-part-id part)
                                      (tool-call-part-name part)
                                      (tool-call-part-arguments part))))
                  (cond
                    [(null? tool-calls) (values new-all 'failed current-turn)]
                    [(cancelled?) (values new-all 'cancelled current-turn)]
                    [else
                     (define sched-result (run-tool-batch tool-calls registry #:exec-context ctx))
                     (define results (scheduler-result-results sched-result))
                     (define tool-result-msgs
                       (for/list ([tc (in-list tool-calls)]
                                  [tr (in-list results)])
                         (make-message (generate-id)
                                       (message-id assistant-msg)
                                       'tool
                                       'tool-result
                                       (list (make-tool-result-part (tool-call-id tc)
                                                                    (tool-result-content tr)
                                                                    (tool-result-is-error? tr)))
                                       (current-seconds)
                                       (hasheq 'toolCallId
                                               (tool-call-id tc)
                                               'isError
                                               (tool-result-is-error? tr)))))
                     (if (cancelled?)
                         (values (append new-all tool-result-msgs) 'cancelled current-turn)
                         (loop (append msgs (list assistant-msg) tool-result-msgs)
                               (sub1 turns-remaining)
                               (append new-all tool-result-msgs)
                               current-turn))])]
                 [(provider-completion-stop-reason? stop-reason)
                  (values new-all 'complete current-turn)]
                 [else (values new-all 'failed current-turn)]))))])))

;; ============================================================
;; spawn-subagents: Batch parallel execution
;; ============================================================

;; v0.99.22 A-2: Parse capabilities from a job hash (for batch spawn path).
;; W2 v0.99.35: Now delegates to normalize-capabilities from helpers.
;; Returns #f when no valid capabilities, or a list of symbols.
(define (parse-job-capabilities job)
  (if (hash-has-key? job 'capabilities)
      (normalize-capabilities/strict (hash-ref job 'capabilities))
      #f))

;; Immutable execution plan for a normalized batch.  The raw digest binds the
;; exact caller-owned request while snapshot-digest binds the immutable,
;; execution-effective representation shown for approval.
(struct batch-execution-plan
        (batch-id jobs snapshot snapshot-digest raw-digest dangerous-jobs max-parallel aggregate?)
  #:transparent)

(define (immutable-string value)
  (and (string? value) (string->immutable-string (string-copy value))))

;; Normalize and validate one job without resolving providers, starting
;; threads, creating sessions, or invoking children.
(define (normalize-batch-job job index exec-ctx)
  (unless (hash? job)
    (error 'tool-spawn-subagents "job ~a must be an object" index))
  (define task (hash-ref job 'task #f))
  (unless (and (string? task) (not (string=? (string-trim task) "")) (<= (string-length task) 100000))
    (error 'tool-spawn-subagents "task for job ~a must be a non-empty bounded string" index))
  (define explicit-job-id (hash-ref job 'jobId #f))
  (when (and explicit-job-id (not (valid-plan-id? explicit-job-id)))
    (error 'tool-spawn-subagents "jobId for job ~a must be a bounded terminal-safe identifier" index))
  (define role-raw (hash-ref job 'role (hash-ref job 'rolePrompt #f)))
  (when (and role-raw (or (not (string? role-raw)) (> (string-length role-raw) 10000)))
    (error 'tool-spawn-subagents "role for job ~a must be a bounded string" index))
  (define model-raw (hash-ref job 'model #f))
  (when (and model-raw (or (not (string? model-raw)) (> (string-length model-raw) 200)))
    (error 'tool-spawn-subagents "model for job ~a must be a bounded string" index))
  (define max-turns (hash-ref job 'max-turns 10))
  (unless (and (exact-integer? max-turns) (> max-turns 0))
    (error 'tool-spawn-subagents "max-turns for job ~a must be a positive integer" index))
  (define declared-caps
    (if (hash-has-key? job 'capabilities)
        (with-handlers ([exn:fail:contract? (lambda (_)
                                              (error 'tool-spawn-subagents
                                                     "invalid capability declaration for job ~a"
                                                     index))])
          (normalize-capabilities/strict (hash-ref job 'capabilities)))
        #f))
  (define caps (bounded-delegated-capabilities declared-caps (current-session-capabilities)))
  (hasheq 'job-id
          (immutable-string explicit-job-id)
          'task
          (immutable-string task)
          'role
          (immutable-string role-raw)
          'model
          (immutable-string (or model-raw (resolve-model-name exec-ctx (hasheq))))
          'max-turns
          max-turns
          'effective-capabilities
          caps
          'effective-tools
          (effective-child-tool-names caps)))

(define (build-batch-execution-plan args jobs max-parallel aggregate? exec-ctx)
  ;; Complete this pass before allocating identities or doing other effects.
  (define normalized
    (for/list ([job (in-list jobs)]
               [index (in-naturals)])
      (normalize-batch-job job index exec-ctx)))
  (define explicit-ids (filter values (map (lambda (job) (hash-ref job 'job-id)) normalized)))
  (unless (= (length explicit-ids) (length (remove-duplicates explicit-ids string=?)))
    (error 'tool-spawn-subagents "jobId values must be unique within a batch"))
  (define batch-id-raw (hash-ref args 'batchId (hash-ref args 'batch-id #f)))
  (when (and batch-id-raw (not (valid-plan-id? batch-id-raw)))
    (error 'tool-spawn-subagents "batchId must be a bounded terminal-safe identifier"))
  (define batch-id (or (immutable-string batch-id-raw) (immutable-string (generate-id))))
  (define planned-jobs
    (for/list ([job (in-list normalized)])
      (hasheq 'job-id
              (or (hash-ref job 'job-id) (generate-id))
              'task
              (hash-ref job 'task)
              'task-digest
              (sha256-digest (or (hash-ref job 'task) ""))
              'role
              (hash-ref job 'role)
              'model
              (hash-ref job 'model)
              'max-turns
              (hash-ref job 'max-turns)
              'effective-capabilities
              (hash-ref job 'effective-capabilities)
              'effective-tools
              (hash-ref job 'effective-tools)
              'tool-call-id
              (generate-id)
              'child-id
              (generate-id)
              'session-id
              (generate-id))))
  (define effective-parallel (min max-parallel (length planned-jobs) (current-agent-pool-limit)))
  (define snapshot
    (immutable-canonical-copy (hasheq 'batch-id
                                      batch-id
                                      'jobs
                                      planned-jobs
                                      'max-parallel
                                      effective-parallel
                                      'aggregate
                                      (and aggregate? #t))))
  (define snapshot-digest (sha256-digest snapshot))
  (define dangerous-jobs
    (filter (lambda (job) (requires-hitl-approval? (hash-ref job 'effective-capabilities #f)))
            (hash-ref snapshot 'jobs)))
  (batch-execution-plan batch-id
                        (hash-ref snapshot 'jobs)
                        snapshot
                        snapshot-digest
                        (sha256-digest args)
                        dangerous-jobs
                        effective-parallel
                        aggregate?))

;; Run a single already-planned job and return (cons job-id result).
(define (run-single-job job provider parent-ctx)
  (define job-id (hash-ref job 'job-id))
  (define task (hash-ref job 'task #f))
  (define role-prompt (or (hash-ref job 'role #f) default-role-prompt))
  (define model-name (or (hash-ref job 'model #f) (resolve-model-name parent-ctx (hasheq))))
  (define max-turns (hash-ref job 'max-turns))
  (define caps (hash-ref job 'effective-capabilities #f))
  (define result
    (if (not task)
        (make-error-result "task is required for each job")
        (run-subagent (hasheq 'task
                              task
                              'role
                              role-prompt
                              'model
                              model-name
                              'max-turns
                              max-turns
                              'capabilities
                              caps
                              'planned-tool-call-id
                              (hash-ref job 'tool-call-id)
                              'planned-child-id
                              (hash-ref job 'child-id)
                              'planned-session-id
                              (hash-ref job 'session-id))
                      role-prompt
                      max-turns
                      (hash-ref job 'effective-tools)
                      parent-ctx)))
  (cons job-id result))

;; Batch subagent execution with immutable planning and one all-or-nothing
;; dangerous-capability approval gate.
(define (tool-spawn-subagents args [exec-ctx #f])
  (define jobs (and (hash? args) (hash-ref args 'jobs #f)))
  (define max-parallel (and (hash? args) (hash-ref args 'maxParallel 3)))
  (define aggregate? (and (hash? args) (hash-ref args 'aggregate #t)))
  (cond
    [(not jobs) (make-error-result "jobs array is required")]
    [(not (list? jobs)) (make-error-result "jobs must be an array")]
    [(null? jobs) (make-error-result "jobs array must not be empty")]
    [(> (length jobs) 12) (make-error-result "jobs array must not exceed 12 items")]
    [(not (and (exact-integer? max-parallel) (>= max-parallel 1)))
     (make-error-result "maxParallel must be at least 1")]
    [(> max-parallel 3) (make-error-result "maxParallel must be at most 3")]
    [(not (boolean? aggregate?)) (make-error-result "aggregate must be boolean")]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-result (format "spawn-subagents failed: ~a"
                                                             (exn-message e))))])
       (define plan (build-batch-execution-plan args jobs max-parallel aggregate? exec-ctx))
       (define spawn-count (length (batch-execution-plan-jobs plan)))
       (cond
         [(not (spawn-rate-capacity? spawn-count))
          (make-error-result "spawn rate limit exceeded (30/min)")]
         [(and (pair? (batch-execution-plan-dangerous-jobs plan))
               (not (request-batch-spawn-approval (batch-execution-plan-batch-id plan)
                                                  (batch-execution-plan-snapshot plan)
                                                  (batch-execution-plan-snapshot-digest plan)
                                                  (batch-execution-plan-dangerous-jobs plan)
                                                  exec-ctx)))
          (make-denied-spawn-result "subagent batch blocked — HITL approval denied")]
         [(not (string=? (batch-execution-plan-raw-digest plan) (sha256-digest args)))
          (make-error-result "subagent batch request changed after approval")]
         [(not (reserve-spawn-rate! spawn-count))
          (make-error-result "spawn rate limit exceeded (30/min)")]
         [else
          ;; Approval precedes provider resolution. Revalidate once more directly
          ;; before worker creation to close mutation/reorder/widening races.
          (define provider (resolve-provider exec-ctx))
          (if (not (string=? (batch-execution-plan-raw-digest plan) (sha256-digest args)))
              (make-error-result "subagent batch request changed after approval")
              (finish-batch plan provider exec-ctx))]))]))

(define (finish-batch plan provider exec-ctx)
  (define results
    (run-jobs-parallel (batch-execution-plan-jobs plan)
                       provider
                       exec-ctx
                       (batch-execution-plan-max-parallel plan)))
  (define job-results
    (for/list ([entry (in-list results)]
               [job (in-list (batch-execution-plan-jobs plan))])
      (define result (cdr entry))
      (define metadata (or (tool-result-details result) (hasheq)))
      (hasheq 'jobId
              (car entry)
              'success
              (not (tool-result-is-error? result))
              'terminalStatus
              (hash-ref metadata 'terminal-status "failed")
              'turnsUsed
              (hash-ref metadata 'turns-used 0)
              'resultPresent
              (hash-ref metadata 'result-present? #f)
              'contentSize
              (hash-ref metadata 'content-size 0)
              'contentDigest
              (hash-ref metadata 'content-digest "")
              'taskDigest
              (hash-ref job 'task-digest)
              'capabilities
              (hash-ref job 'effective-capabilities)
              'toolCallId
              (hash-ref job 'tool-call-id)
              'childId
              (hash-ref job 'child-id)
              'sessionId
              (hash-ref job 'session-id))))
  ;; Raw child output belongs only to the explicit tool-result content channel.
  ;; Retained details/events stay digest-only and safe for logs/traces.
  (define visible-job-results
    (for/list ([job-result (in-list job-results)]
               [entry (in-list results)])
      (hasheq 'jobId
              (hash-ref job-result 'jobId)
              'success
              (hash-ref job-result 'success #f)
              'content
              (tool-result-content (cdr entry)))))
  (define success-count (count (lambda (result) (hash-ref result 'success #f)) job-results))
  (define fail-count (- (length job-results) success-count))
  (define summary-text
    (if (batch-execution-plan-aggregate? plan)
        (format "Batch complete: ~a/~a succeeded, ~a failed.\n\n~a"
                success-count
                (length job-results)
                fail-count
                (string-join
                 (for/list ([job-result (in-list job-results)]
                            [entry (in-list results)])
                   (format "[~a] ~a"
                           (hash-ref job-result 'jobId)
                           (if (hash-ref job-result 'success #f)
                               (extract-text-summary (tool-result-content (cdr entry)))
                               (string-upcase (hash-ref job-result 'terminalStatus "failed")))))
                 "\n---\n"))
        (format "Batch complete: ~a/~a succeeded, ~a failed."
                success-count
                (length job-results)
                fail-count)))
  (make-success-result (list (hasheq 'type "text" 'text summary-text)
                             (hasheq 'type "batch-results" 'jobs visible-job-results))
                       (hasheq 'batch-id
                               (batch-execution-plan-batch-id plan)
                               'batchId
                               (batch-execution-plan-batch-id plan)
                               'snapshot-digest
                               (batch-execution-plan-snapshot-digest plan)
                               'total-jobs
                               (length job-results)
                               'succeeded
                               success-count
                               'failed
                               fail-count
                               'jobs
                               job-results)))

;; W2 v0.99.35: extract-text-summary and SUBAGENT-SUMMARY-MAX-CHARS
;; moved to spawn-subagent-helpers.rkt (pure functions).

;; Run jobs in parallel with bounded concurrency using threads + channel
(define (run-jobs-parallel jobs provider exec-ctx max-parallel)
  (define n (length jobs))
  (cond
    ;; Single job: run directly (no thread overhead)
    [(= n 1) (list (run-single-job (car jobs) provider exec-ctx))]
    ;; Multiple jobs: use threads with bounded concurrency. Each worker owns a
    ;; distinct vector slot, so completion timing cannot reorder output.
    [else
     (define ordered-results (make-vector n #f))
     (define concurrency-sem (make-semaphore max-parallel))
     (define threads
       (for/list ([job (in-list jobs)]
                  [index (in-naturals)])
         (thread (lambda ()
                   (call-with-semaphore
                    concurrency-sem
                    (lambda ()
                      (vector-set! ordered-results index (run-single-job job provider exec-ctx))))))))
     (for-each thread-wait threads)
     (vector->list ordered-results)]))

;; Helper: create a mock provider for subagent testing (fallback when no real provider)
(define (build-mock-provider-for-subagent)
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text "Subagent task completed."))
                         (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                         "mock-model"
                         'stop))
  (make-mock-provider mock-response #:name "subagent-mock"))

;; Test submodule — exports private helpers for unit testing
(module+ test
  (require rackunit)
  (provide extract-text-summary
           SUBAGENT-SUMMARY-MAX-CHARS)
  ;; Truncation limit test
  (check-equal? SUBAGENT-SUMMARY-MAX-CHARS 4000)
  (check-true
   (>= (string-length (extract-text-summary (list (hasheq 'type "text" 'text (make-string 500 #\X)))))
       497)))
