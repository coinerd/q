#lang racket/base

;; tools/builtins/spawn-subagent.rkt — Subagent spawning tool
;;
;; Spawns an isolated child agent process to execute a delegated task.
;; The child gets a fresh context window, optional role prompt, and
;; scoped tool access. Returns the child's final text result.
;;
;; #1193: Subagent Spawning — Isolated Child Agent Processes
;; #1203/#1204: Real provider injection — uses parent's provider

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../../tools/tool.rkt"
         (only-in "../../util/capability.rkt" valid-capability?)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! make-event-bus)
         (only-in "../../runtime/settings.rkt" q-settings? setting-ref)
         "../model-bridge.rkt"
         (only-in "provider-hash-bridge.rkt"
                  message->provider-hash
                  content-part->provider-hash
                  messages->provider-hashes)
         "../../util/ids.rkt"
         (only-in "../../util/content/content-parts.rkt"
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
         (only-in "../../agent/blackboard.rkt" current-blackboard))

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
         current-spawn-approval-result
         run-subagent-with-config
         ;; v0.99.23 §5.1: Session-wide agent pool limit
         current-agent-pool-limit)

;; Subagent configuration struct — replaces ad-hoc hash extraction
;; v0.99.21 §4.2: Added capabilities field (6th, default #f for backward compat)
(struct subagent-config (task role max-turns tools model capabilities) #:transparent)

;; v0.99.23 §5.1: Session-wide agent pool limit parameter.
;; Default 3 (matching the previous hardcoded max). CLI --agent-pool overrides.
(define current-agent-pool-limit (make-parameter 3))

;; Spawn rate limiter: parameter holding list of spawn timestamps (milliseconds)
(define current-spawn-timestamps (make-parameter (box '())))
(define SPAWN-RATE-LIMIT 30) ;; max spawns per minute
(define SPAWN-RATE-WINDOW 60000) ;; 60 seconds in ms

;; Check and record spawn attempt. Returns #t if allowed, #f if rate-limited.
(define (check-spawn-rate!)
  (define now (current-inexact-milliseconds))
  (define ts-box (current-spawn-timestamps))
  (define valid-ts (filter (lambda (t) (> (- now t) SPAWN-RATE-WINDOW)) (unbox ts-box)))
  (if (>= (length valid-ts) SPAWN-RATE-LIMIT)
      #f
      (begin
        (set-box! ts-box (cons now valid-ts))
        #t)))

;; v0.99.23 §5.3: HITL approval override hook.
;; Defaults to #t (permissive — non-interactive mode).
;; Tests can parameterize this to #f to simulate denial.
(define current-spawn-approval-result (make-parameter #t))

;; v0.99.23 §5.3: Check if subagent spawn requires HITL approval.
;; Returns #t when capabilities include shell-exec or git-write.
;; Returns #f for #f, '(), or read-only capabilities.
(define (requires-hitl-approval? capabilities)
  (and (list? capabilities)
       (pair? capabilities)
       (or (memq 'shell-exec capabilities) (memq 'git-write capabilities))))

;; v0.99.23 §5.3: Request HITL approval via exec-ctx event system.
;; Returns #t when approved, #f when denied.
;; When no approval channel is available (non-interactive mode), returns #t
;; (permissive — the approval gate only blocks in interactive/TUI mode).
(define (request-spawn-approval capabilities task-desc exec-ctx)
  (define publisher (and exec-ctx (exec-context-event-publisher exec-ctx)))
  ;; Emit the approval-request event for TUI display (when available)
  (when publisher
    (publisher 'mas.spawn-approval-requested
               (hasheq 'capabilities
                       capabilities
                       'task-preview
                       (if (and (string? task-desc) (> (string-length task-desc) 200))
                           (substring task-desc 0 200)
                           (or task-desc "")))))
  ;; Consult the approval result parameter.
  ;; Default #t (permissive). TUI mode would set this based on user response.
  (current-spawn-approval-result))

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

;; The spawn-subagent tool implementation
(define (tool-spawn-subagent args [exec-ctx #f])
  (if (not (check-spawn-rate!))
      (make-error-result "spawn rate limit exceeded (30/min)")
      (let ([cfg (parse-subagent-config args)])
        (if (not (subagent-config-task cfg))
            (make-error-result "task is required")
            (run-subagent-with-config cfg exec-ctx)))))

;; Parse subagent-config from tool call args hash.
;; v0.99.21 §4.2: Added capabilities parsing.
(define (parse-subagent-config args)
  (define caps-raw (hash-ref args 'capabilities #f))
  (define caps
    (cond
      [(not caps-raw) #f]
      [(list? caps-raw)
       (filter valid-capability?
               (map (lambda (c)
                      (if (string? c)
                          (string->symbol c)
                          c))
                    caps-raw))]
      [(string? caps-raw) (list (string->symbol caps-raw))]
      [else #f]))
  (subagent-config (hash-ref args 'task #f)
                   (resolve-role-prompt (hash-ref args 'role default-role-prompt))
                   (hash-ref args 'max-turns 5)
                   (hash-ref args 'tools #f)
                   (hash-ref args 'model #f)
                   (and caps (pair? caps) caps)))

;; Execute subagent using typed config struct.
;; v0.99.23 §5.3: HITL approval gate for dangerous capabilities.
(define (run-subagent-with-config cfg exec-ctx)
  (define caps (subagent-config-capabilities cfg))
  (if (and (requires-hitl-approval? caps)
           (not (request-spawn-approval caps (subagent-config-task cfg) exec-ctx)))
      (make-error-result "subagent spawn blocked — HITL approval denied")
      (run-subagent (hasheq 'task
                            (subagent-config-task cfg)
                            'role
                            (subagent-config-role cfg)
                            'max-turns
                            (subagent-config-max-turns cfg)
                            'tools
                            (subagent-config-tools cfg)
                            'model
                            (subagent-config-model cfg)
                            'capabilities
                            caps)
                    (subagent-config-role cfg)
                    (subagent-config-max-turns cfg)
                    (subagent-config-tools cfg)
                    exec-ctx)))

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
    [(or (not capabilities) (null? capabilities)) all-tools]
    [else
     (filter (lambda (t)
               (define rc (tool-required-capability t))
               (or (eq? rc 'any) (memq rc capabilities)))
             all-tools)]))

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
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (sanitize-error-message
                                                   (format "subagent failed: ~a" (exn-message e)))))])
    ;; #1204: Resolve real provider from parent's runtime-settings
    (define provider (resolve-provider exec-ctx))
    (define model-name (resolve-model-name exec-ctx args))

    ;; Create scoped tool registry with child-safe tools
    ;; v0.99.21 §4.2: Filter tools by capabilities when specified
    (define capabilities (hash-ref args 'capabilities #f))
    (define registry (make-tool-registry))
    ;; Register tools directly to avoid circular dependency with registry-defaults
    (for ([t (child-safe-tools-filtered capabilities)])
      (register-tool! registry t))

    ;; Build child session
    (define session-id (generate-id))
    (define bus (make-event-bus))
    (define settings (hasheq 'model model-name))

    ;; Create execution context for child
    (define child-ctx
      (make-exec-context #:working-directory (if exec-ctx
                                                 (exec-context-working-directory exec-ctx)
                                                 (current-directory))
                         #:event-publisher (lambda (event-type payload)
                                             (emit-session-event! bus session-id event-type payload))
                         #:runtime-settings settings
                         #:call-id (generate-id)
                         #:session-metadata (hasheq 'session-id session-id 'role "subagent")))

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
                    (list effective-role-prompt)
                    (current-seconds)
                    (hasheq)))
    (define user-msg
      (make-message (generate-id) #f 'user 'message (list task) (current-seconds) (hasheq)))

    ;; Run the child agent loop
    (define-values (result-messages final-status)
      (run-subagent-loop provider registry (list system-msg user-msg) child-ctx max-turns))

    ;; Extract final text from result — ONLY text content, not tool-call-parts.
    ;; Bug fix: (format "~a" content) on mixed lists containing tool-call-part
    ;; structs produces #hasheq() / #(struct:...) garbage that the parent LLM
    ;; cannot parse. Extract string items + text-parts from content.
    ;; When no text is present (tool-call-only message), summarize tool names.
    (define result-text
      (string-join (for/list ([m (in-list result-messages)]
                              #:when (eq? (message-role m) 'assistant))
                     (define content (message-content m))
                     (cond
                       [(string? content) content]
                       [(list? content)
                        (define strings-only
                          (for/list ([c (in-list content)]
                                     #:when (string? c))
                            c))
                        (define text-parts
                          (for/list ([c (in-list content)]
                                     #:when (text-part? c))
                            (text-part-text c)))
                        (define tool-parts
                          (for/list ([c (in-list content)]
                                     #:when (tool-call-part? c))
                            (tool-call-part-name c)))
                        (define all-text (append strings-only text-parts))
                        (cond
                          [(pair? all-text) (string-join all-text "\n")]
                          ;; No text — summarize what tools were called
                          [(pair? tool-parts) (format "[called: ~a]" (string-join tool-parts ", "))]
                          [else ""])]
                       [else (format "~a" content)]))
                   "\n"))

    (make-success-result
     (list (hasheq 'type "text" 'text result-text))
     (hasheq 'turns-used max-turns 'status (symbol->string final-status) 'session-id session-id))))

;; ============================================================
;; Message-to-provider conversion
;; ============================================================

;; Convert internal message struct to a provider-facing JSON hash.
;; Providers expect simple {role, content} hashes, not q internal structs.
;; Handles both string content (initial messages) and content-part lists
;; (assistant messages with tool calls, tool-result messages).
(define (message->provider-hash msg)
  (define role-sym (message-role msg))
  (define role-str
    (if (symbol? role-sym)
        (symbol->string role-sym)
        (format "~a" role-sym)))
  (define content (message-content msg))
  (define content-val
    (cond
      [(string? content) content]
      [(null? content) ""]
      [(and (list? content) (andmap string? content)) (string-join content "\n")]
      [(and (list? content) (andmap content-part? content))
       (for/list ([cp (in-list content)])
         (content-part->provider-hash cp))]
      [(list? content)
       (for/list ([c (in-list content)])
         (cond
           [(string? c) c]
           [(content-part? c) (content-part->provider-hash c)]
           [(hash? c) c]
           [else (format "~a" c)]))]
      [else (format "~a" content)]))
  (hasheq 'role role-str 'content content-val))

;; Convert content-part to provider-facing hash for JSON serialization.
(define (content-part->provider-hash cp)
  (cond
    [(text-part? cp) (hasheq 'type "text" 'text (text-part-text cp))]
    [(tool-call-part? cp)
     (hasheq 'type
             "tool_call"
             'id
             (or (tool-call-part-id cp) "")
             'name
             (or (tool-call-part-name cp) "")
             'arguments
             (tool-call-part-arguments cp))]
    [(tool-result-part? cp)
     (hasheq 'type
             "tool_result"
             'tool_call_id
             (or (tool-result-part-tool-call-id cp) "")
             'content
             (tool-result-part-content cp)
             'is_error
             (tool-result-part-is-error? cp))]
    [else (hasheq 'type "text" 'text (format "~a" cp))]))

;; Convert a list of message structs to provider-facing JSON hashes.
(define (messages->provider-hashes msgs)
  (map message->provider-hash msgs))

;; Run a simple agent loop for the subagent
;; v0.19.4 GAP-1 fix: actually dispatch tool_calls instead of returning 'stopped
(define (run-subagent-loop provider registry messages ctx max-turns)
  (let loop ([msgs messages]
             [turns-remaining max-turns]
             [all-results '()])
    (cond
      [(<= turns-remaining 0) (values all-results 'max-turns-reached)]
      [else
       ;; #SERIALIZE-FIX: Convert internal message structs to provider-facing
       ;; JSON hashes before building the model request. Real providers call
       ;; jsexpr->bytes on the request body, which rejects Racket structs.
       (define provider-msgs (messages->provider-hashes msgs))
       (define req (make-model-request provider-msgs (list-active-tools-jsexpr registry) (hasheq)))
       (define resp (provider-send provider req))
       (define content (model-response-content resp))
       ;; Build assistant message from response content parts
       (define content-parts
         (for/list ([c (in-list content)])
           (match (hash-ref c 'type #f)
             ["text" (hash-ref c 'text (format "~a" c))]
             ["tool_call"
              (make-tool-call-part (hash-ref c 'id (generate-id))
                                   (hash-ref c 'name "")
                                   (ensure-hash-args (hash-ref c 'arguments "{}")))]
             [#f (hash-ref c 'text (format "~a" c))]
             [_ (format "~a" c)])))
       (define assistant-msg
         (make-message (generate-id) #f 'assistant 'message content-parts (current-seconds) (hasheq)))
       (define new-all (append all-results (list assistant-msg)))
       (cond
         ;; Dispatch tool calls and continue the loop
         [(eq? (model-response-stop-reason resp) 'tool_calls)
          (define tool-calls
            (for/list ([part (in-list content-parts)]
                       #:when (tool-call-part? part))
              (make-tool-call (tool-call-part-id part)
                              (tool-call-part-name part)
                              (tool-call-part-arguments part))))
          (if (null? tool-calls)
              (values new-all 'complete)
              (let ()
                ;; Run tool batch
                (define sched-result (run-tool-batch tool-calls registry #:exec-context ctx))
                (define results (scheduler-result-results sched-result))
                ;; Build tool-result messages
                (define tool-result-msgs
                  (for/list ([tc (in-list tool-calls)]
                             [tr (in-list results)])
                    (make-message
                     (generate-id)
                     (message-id assistant-msg)
                     'tool
                     'tool-result
                     (list (make-tool-result-part (tool-call-id tc)
                                                  (tool-result-content tr)
                                                  (tool-result-is-error? tr)))
                     (current-seconds)
                     (hasheq 'toolCallId (tool-call-id tc) 'isError (tool-result-is-error? tr)))))
                (loop (append msgs (list assistant-msg) tool-result-msgs)
                      (sub1 turns-remaining)
                      (append new-all tool-result-msgs))))]
         [else (values new-all 'complete)])])))

;; ============================================================
;; spawn-subagents: Batch parallel execution
;; ============================================================

;; v0.99.22 A-2: Parse capabilities from a job hash (for batch spawn path).
;; Mirrors the capability parsing in parse-subagent-config.
;; Returns #f when no valid capabilities, or a list of symbols.
(define (parse-job-capabilities job)
  (define caps-raw (hash-ref job 'capabilities #f))
  (cond
    [(not caps-raw) #f]
    [(list? caps-raw)
     (define filtered
       (filter valid-capability?
               (map (lambda (c)
                      (if (string? c)
                          (string->symbol c)
                          c))
                    caps-raw)))
     (if (null? filtered) #f filtered)]
    [else #f]))

;; Run a single job and return (cons job-id result) or (cons job-id error)
(define (run-single-job job provider parent-ctx)
  (define job-id (hash-ref job 'jobId #f))
  (define task (hash-ref job 'task #f))
  (define role-prompt (hash-ref job 'role (hash-ref job 'rolePrompt #f)))
  (define model-name (or (hash-ref job 'model #f) (resolve-model-name parent-ctx job)))
  (define max-turns (hash-ref job 'max-turns 5))
  ;; v0.99.22 A-2: Parse capabilities from job hash (mirrors parse-subagent-config)
  (define caps (parse-job-capabilities job))
  (define result
    (if (not task)
        (make-error-result "task is required for each job")
        (run-subagent (hasheq 'task
                              task
                              'role
                              (or role-prompt default-role-prompt)
                              'model
                              model-name
                              'max-turns
                              max-turns
                              'capabilities
                              caps)
                      (or role-prompt default-role-prompt)
                      max-turns
                      #f ;; allowed-tools
                      parent-ctx)))
  (cons job-id result))

;; Batch subagent execution with concurrency control
(define (tool-spawn-subagents args [exec-ctx #f])
  (define jobs (hash-ref args 'jobs #f))
  (define max-parallel (hash-ref args 'maxParallel 3))
  (define aggregate? (hash-ref args 'aggregate #t))

  (cond
    [(not jobs) (make-error-result "jobs array is required")]
    [(not (list? jobs)) (make-error-result "jobs must be an array")]
    [(= (length jobs) 0) (make-error-result "jobs array must not be empty")]
    [(> (length jobs) 12) (make-error-result "jobs array must not exceed 12 items")]
    [(< max-parallel 1) (make-error-result "maxParallel must be at least 1")]
    [(not (check-spawn-rate!)) (make-error-result "spawn rate limit exceeded (30/min)")]
    [else
     ;; v0.83.10: Clamp instead of reject. Semaphore-based queuing handles excess.
     ;; v0.99.23 §5.1: Respect session-wide --agent-pool limit parameter.
     (define pool-limit (current-agent-pool-limit))
     (define effective-parallel (min max-parallel (length jobs) pool-limit))
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-result (format "spawn-subagents failed: ~a"
                                                             (exn-message e))))])
       ;; Resolve provider once for all jobs
       (define provider (resolve-provider exec-ctx))

       ;; Execute jobs with bounded parallelism using futures/threads
       (define results (run-jobs-parallel jobs provider exec-ctx effective-parallel))

       ;; Aggregate results
       (define job-results
         (for/list ([r (in-list results)])
           (define job-id (car r))
           (define result (cdr r))
           (hasheq 'jobId
                   job-id
                   'success
                   (not (tool-result-is-error? result))
                   'content
                   (tool-result-content result)
                   'details
                   (tool-result-details result))))

       (define success-count (count (lambda (r) (hash-ref r 'success #f)) job-results))
       (define fail-count (- (length job-results) success-count))

       (define summary-text
         (if aggregate?
             (format "Batch complete: ~a/~a succeeded, ~a failed.\n\n~a"
                     success-count
                     (length job-results)
                     fail-count
                     (string-join (for/list ([jr (in-list job-results)])
                                    (format "[~a] ~a"
                                            (or (hash-ref jr 'jobId #f) "unnamed")
                                            (if (hash-ref jr 'success #f)
                                                (extract-text-summary (hash-ref jr 'content '()))
                                                "FAILED")))
                                  "\n---\n"))
             (format "Batch complete: ~a/~a succeeded, ~a failed."
                     success-count
                     (length job-results)
                     fail-count)))

       (make-success-result (list (hasheq 'type "text" 'text summary-text))
                            (hasheq 'total-jobs
                                    (length job-results)
                                    'succeeded
                                    success-count
                                    'failed
                                    fail-count
                                    'jobs
                                    job-results)))]))

;; Extract a text summary from tool result content.
;; Bug fix: Raised from 200→4000 chars — the old 200-char limit destroyed
;; nearly all useful content from subagent tasks.
(define SUBAGENT-SUMMARY-MAX-CHARS 4000)

(define (extract-text-summary content [max-chars SUBAGENT-SUMMARY-MAX-CHARS])
  (define full-text
    (string-join (for/list ([c (in-list (if (list? content)
                                            content
                                            '()))])
                   (cond
                     [(and (hash? c) (hash-ref c 'text #f)) (hash-ref c 'text "")]
                     [(string? c) c]
                     [else ""]))
                 "\n"))
  (if (> (string-length full-text) max-chars)
      (string-append (substring full-text 0 (- max-chars 3)) "...")
      full-text))

;; Run jobs in parallel with bounded concurrency using threads + channel
(define (run-jobs-parallel jobs provider exec-ctx max-parallel)
  (define n (length jobs))
  (cond
    ;; Single job: run directly (no thread overhead)
    [(= n 1) (list (run-single-job (car jobs) provider exec-ctx))]
    ;; Multiple jobs: use threads with bounded concurrency
    [else
     (define results-box (box '()))
     (define concurrency-sem (make-semaphore max-parallel))
     (define mutex-sem (make-semaphore 1))
     (define threads
       (for/list ([job (in-list jobs)])
         (thread (lambda ()
                   ;; Acquire concurrency slot
                   (call-with-semaphore
                    concurrency-sem
                    (lambda ()
                      (define r (run-single-job job provider exec-ctx))
                      ;; Thread-safe append to results (shared mutex)
                      (call-with-semaphore
                       mutex-sem
                       (lambda () (set-box! results-box (cons r (unbox results-box)))))))))))
     ;; Wait for all threads to complete
     (for-each thread-wait threads)
     ;; Results are in reverse order (last completed first), restore job order
     (reverse (unbox results-box))]))

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
