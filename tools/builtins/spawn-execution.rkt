#lang racket/base

;; Subagent execution — provider/model dispatch, agent loop.
;; Extracted from spawn-subagent.rkt (v0.99.50+).
;; Coordinator always passes provider/model via args; no fallback resolution.

(require racket/match
         racket/list
         racket/string
         (only-in "../../tools/tool.rkt"
                  exec-context-working-directory
                  exec-context-cancellation-token
                  exec-context-permission-config
                  exec-context-event-publisher
                  exec-context-runtime-settings
                  exec-context-call-id
                  exec-context-session-metadata
                  make-exec-context
                  make-tool-registry
                  register-tool!
                  list-active-tools-jsexpr
                  tool-name
                  tool-required-capability
                  make-tool
                  tool-result?
                  tool-result-is-error?
                  tool-result-content
                  tool-result-details
                  make-tool-result
                  make-error-result
                  make-success-result)
         (only-in "../model-bridge.rkt"
                  provider?
                  provider-send
                  make-model-request
                  make-model-response
                  make-mock-provider
                  model-response-content
                  model-response-stop-reason)
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
                  text-part?
                  text-part-text
                  content-part?)
         (only-in "../../util/message/message.rkt"
                  make-message
                  message-role
                  message-content
                  message-id)
         (only-in "../../util/tool/tool-types.rkt" make-tool-call tool-call-id tool-result-content)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/json/json-helpers.rkt" ensure-hash-args)
         (only-in "../../util/error/error-sanitizer.rkt" sanitize-error-message)
         (only-in "../../util/capability.rkt" valid-capability?)
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt" safe-mode? allowed-tool?)
         (only-in "../../runtime/auto-retry.rkt" with-auto-retry)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! make-event-bus)
         (only-in "../../util/cancellation.rkt" cancellation-token-cancelled?)
         (only-in "../../util/message/provider-transport.rkt"
                  provider-tool-call-type?
                  provider-tool-stop-reason?
                  provider-completion-stop-reason?)
         (only-in "../../runtime/settings.rkt" q-settings? setting-ref)
         (only-in "provider-hash-bridge.rkt" messages->provider-hashes)
         "../../tools/scheduler.rkt"
         "spawn-subagent-helpers.rkt"
         "spawn-execution-plan.rkt"
         (prefix-in rate: "spawn-rate-limit.rkt")
         ;; Individual builtins for child-safe tool registration (no circular dep)
         (only-in "../builtins/read.rkt" tool-read)
         (only-in "../builtins/write.rkt" tool-write)
         (only-in "../builtins/edit.rkt" tool-edit)
         (only-in "../builtins/bash.rkt" tool-bash)
         (only-in "../builtins/grep.rkt" tool-grep)
         (only-in "../builtins/find.rkt" tool-find)
         (only-in "../builtins/ls.rkt" tool-ls))

(provide run-subagent
         run-subagent-loop
         child-safe-tools
         child-safe-tools-filtered)

;; ============================================================
;; Child-safe tools
;; ============================================================

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

;; ============================================================
;; Agent loop -- provider dispatch
;; ============================================================

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
         (define request-model (rt-settings-ref (exec-context-runtime-settings ctx) 'model #f))
         (define req
           (make-model-request provider-msgs
                               (list-active-tools-jsexpr registry)
                               (if request-model
                                   (hasheq 'model request-model)
                                   (hasheq))))
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
;; Subagent runner
;; ============================================================

;; Internal: run the subagent
;; Coordinator always passes provider/model via args (planned-provider, model).
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
    ;; Provider and model are always passed via args by the coordinator.
    (define provider (hash-ref args 'planned-provider))
    (unless (provider? provider)
      (error 'run-subagent "planned-provider must be a provider"))
    (define model-name (hash-ref args 'model "mock-model"))

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
                   (hash-ref args
                             'planned-parent-call-id
                             (lambda ()
                               (if exec-ctx
                                   (exec-context-call-id exec-ctx)
                                   "")))))
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
      (make-exec-context #:working-directory (if (hash-has-key? args 'planned-working-directory)
                                                 (hash-ref args 'planned-working-directory)
                                                 (if exec-ctx
                                                     (exec-context-working-directory exec-ctx)
                                                     (current-directory)))
                         #:cancellation-token
                         (and exec-ctx (exec-context-cancellation-token exec-ctx))
                         #:permission-config (and exec-ctx (exec-context-permission-config exec-ctx))
                         #:event-publisher (lambda (event-type payload)
                                             (emit-session-event! bus session-id event-type payload))
                         #:runtime-settings settings
                         #:call-id tool-call-id
                         #:session-metadata (hasheq 'session-id
                                                    session-id
                                                    'child-id
                                                    child-id
                                                    'parent-session-id
                                                    (hash-ref args 'planned-parent-session-id "")
                                                    'role
                                                    "subagent")))

    ;; Build messages for child
    (define system-msg
      (make-message (generate-id)
                    #f
                    'system
                    'message
                    (list (make-text-part role-prompt))
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
;; Internal helpers
;; ============================================================

;; Extract a value from runtime-settings which may be a q-settings struct or a plain hash.
(define (rt-settings-ref rt key [default #f])
  (cond
    [(hash? rt) (hash-ref rt key default)]
    [(q-settings? rt) (setting-ref rt key default)]
    [else default]))
