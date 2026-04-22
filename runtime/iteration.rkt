#lang racket/base

;; runtime/iteration.rkt — agent iteration loop
;;
;; Extracted from agent-session.rkt for single-responsibility separation.
;; Handles the multi-turn tool-call iteration loop: run agent turn → check
;; for tool calls → execute tools → feed results back → repeat.
;;
;; Also provides shared helpers used by agent-session.rkt:
;;   now-seconds, emit-session-event!, maybe-dispatch-hooks
;;
;; ── LAYER EXCEPTION (ARCH-01 / #341) ──────────────────────────
;; This module resides in the runtime/ layer but imports upward into
;; the tools/ and extensions/ layers:
;;
;;   tools/tool.rkt       → make-exec-context, tool-result accessors, list-tools-jsexpr
;;   tools/scheduler.rkt  → run-tool-batch, scheduler-result-results
;;   extensions/hooks.rkt → dispatch-hooks
;;
;; These upward imports exist because the iteration loop is the single
;; coordination point that wires together tool execution and extension
;; hooks within each agent turn. Moving this wiring into tools/ or
;; extensions/ would create a circular dependency back into runtime/
;; (the loop needs session-store, compactor, and queue). A full
;; dependency-injection refactor to eliminate the upward imports is
;; deferred to a future major version; the current trade-off keeps
;; the coordination logic in one well-documented place.
;; ───────────────────────────────────────────────────────────────

(require racket/contract
         racket/list
         racket/path
         json
         (only-in racket/string string-trim string-join)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  make-tool-result-part
                  make-text-part
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-call
                  tool-call-id
                  tool-call-name
                  make-loop-result
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata
                  make-event
                  ;; FEAT-61: needed by injected-message collector
                  event-ev
                  event-payload)
         "../agent/event-bus.rkt"
         (only-in "../agent/queue.rkt"
                  dequeue-steering!
                  dequeue-followup!
                  dequeue-all-followups!
                  queue-status)
         "../agent/loop.rkt"
         ;; ARCH-01 upward import — runtime→tools: iteration loop builds exec
         ;; contexts and queries the tool registry to pass tool definitions to
         ;; the LLM and to construct tool-result messages.
         (only-in "../tools/tool.rkt"
                  make-exec-context
                  make-error-result
                  tool-result-content
                  tool-result-is-error?
                  list-tools-jsexpr
                  merge-tool-lists)
         ;; Settings struct for exec-context runtime-settings (#1240)
         (only-in "../runtime/settings.rkt" make-minimal-settings setting-ref setting-ref*)
         ;; ARCH-01 upward import — runtime→tools: iteration loop is the sole
         ;; call site for run-tool-batch, coordinating parallel tool execution
         ;; within each agent turn.
         "../tools/scheduler.rkt"
         ;; ARCH-01 upward import — runtime→extensions: iteration loop
         ;; dispatches lifecycle hooks (turn-start, tool-call, tool-result,
         ;; turn-end, etc.) so extensions can observe/amend/block each step.
         "../extensions/hooks.rkt"
         "../runtime/session-store.rkt"
         (only-in "../runtime/compactor.rkt"
                  build-tiered-context-with-hooks
                  tiered-context->message-list
                  compact-history
                  compaction-result-removed-count
                  compaction-result-kept-messages)
         "../runtime/cutpoint-rules.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         ;; R2-6: Import hook-result accessors
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         (only-in "../runtime/auto-retry.rkt"
                  with-auto-retry
                  retryable-error?
                  context-overflow-error?
                  timeout-error?)
         ;; FEAT-61: message injection support
         (only-in "../extensions/message-inject.rkt" injection-event-topic)
         ;; v0.14.1: mid-turn token budget check
         (only-in "../llm/token-budget.rkt" estimate-context-tokens))

(provide run-iteration-loop
         emit-session-event!
         maybe-dispatch-hooks
         ensure-hash-args ;; for testing
         ;; FEAT-61: message injection support
         make-injected-collector!
         drain-injected-messages!
         ;; FEAT-66: overflow recovery (for testing)
         call-with-overflow-recovery
         ;; v0.14.1: mid-turn token budget check (for testing)
         check-mid-turn-budget!)

;; ============================================================
;; Shared helpers
;; ============================================================

(define (emit-session-event! bus sid event-name payload)
  (define evt (make-event event-name (now-seconds) sid #f payload))
  (publish! bus evt)
  evt)

;; Safely dispatch hooks if extension-registry is present.
;; Returns (values amended-payload hook-result) or (values payload #f) if no registry.
(define (maybe-dispatch-hooks ext-reg hook-point payload #:ctx [ctx #f])
  (if ext-reg
      (let ([result (dispatch-hooks hook-point payload ext-reg #:ctx ctx)])
        (values (hook-result-payload result) result))
      (values payload #f)))

;; v0.14.1: Check if context exceeds mid-turn token budget.
;; Returns estimated token count. Emits event if over budget.
(define (check-mid-turn-budget! ctx bus session-id config)
  (define max-tokens (hash-ref config 'max-context-tokens 128000))
  (define budget-threshold (inexact->exact (floor (* max-tokens 0.9))))
  ;; Extract text from message? structs for token estimation
  (define texts
    (for/list ([msg (in-list ctx)])
      (define content (message-content msg))
      (cond
        [(string? content) content]
        [(list? content)
         (apply string-append
                (for/list ([part (in-list content)]
                           #:when (text-part? part))
                  (text-part-text part)))]
        [else ""])))
  (define estimated
    (for/sum ([t (in-list texts)]) (estimate-context-tokens (list (hasheq 'content t)))))
  (when (> estimated budget-threshold)
    (emit-session-event!
     bus
     session-id
     "context.mid-turn-over-budget"
     (hasheq 'estimated-tokens estimated 'budget budget-threshold 'max-tokens max-tokens)))
  estimated)

;; ============================================================
;; Iteration-loop helpers (private)
;; ============================================================

;; Extract tool-call structs from assistant messages' tool-call-parts
(define (extract-tool-calls-from-messages messages)
  (for*/list ([msg (in-list messages)]
              #:when (eq? (message-role msg) 'assistant)
              [part (in-list (message-content msg))]
              #:when (tool-call-part? part))
    (make-tool-call (tool-call-part-id part)
                    (tool-call-part-name part)
                    (ensure-hash-args (tool-call-part-arguments part)))))

;; Create tool-result messages from scheduler results
(define (make-tool-result-messages tool-calls results parent-msg-id)
  (for/list ([tc (in-list tool-calls)]
             [tr (in-list results)])
    (define msg-id (generate-id))
    (make-message msg-id
                  parent-msg-id
                  'tool
                  'tool-result
                  (list (make-tool-result-part (tool-call-id tc)
                                               (tool-result-content tr)
                                               (tool-result-is-error? tr)))
                  (now-seconds)
                  (hasheq 'toolCallId (tool-call-id tc) 'isError (tool-result-is-error? tr)))))

;; ============================================================
;; Queue helpers
;; ============================================================

;; Dequeue all steering messages from queue (returns list, empty if none)
(define (dequeue-all-steering! q)
  (let loop ([acc '()])
    (define msg (dequeue-steering! q))
    (if msg
        (loop (append acc (list msg)))
        acc)))

;; ============================================================
;; Extracted iteration-loop sub-procedures (QUAL-08)
;; ============================================================

;; Build assembled context using tiered context assembly with hooks.
;; Returns the assembled message list.
(define (build-assembled-context ctx-to-use config ext-reg bus session-id iteration)
  ;; WP-37 + R2-6: Context Assembly with Tier A/B/C separation and Hook support
  (define tier-b-count (hash-ref config 'tier-b-count 20))
  (define tier-c-count (hash-ref config 'tier-c-count 4))
  (define max-tokens (hash-ref config 'max-tokens 8192))

  ;; R2-6: Create hook dispatcher function for context assembly
  (define ctx-assembly-hook-dispatcher
    (and ext-reg
         (lambda (hook-point payload)
           (define result (dispatch-hooks hook-point payload ext-reg))
           result)))

  ;; Build tiered context with hook support
  (define-values (tc assembly-hook-result)
    (build-tiered-context-with-hooks ctx-to-use
                                     #:hook-dispatcher ctx-assembly-hook-dispatcher
                                     #:tier-b-count tier-b-count
                                     #:tier-c-count tier-c-count
                                     #:max-tokens max-tokens))

  ;; Handle block action from context-assembly hook
  (when (and assembly-hook-result (eq? (hook-result-action assembly-hook-result) 'block))
    (emit-session-event! bus session-id "context.assembly.blocked" (hasheq 'reason "extension-block"))
    (raise (exn:fail "Context assembly blocked by extension" (current-continuation-marks))))

  (define ctx-assembled (tiered-context->message-list tc))

  ;; Emit context.assembled event
  (emit-session-event! bus
                       session-id
                       "context.assembled"
                       (hasheq 'iteration
                               iteration
                               'total-messages
                               (length ctx-to-use)
                               'assembled-messages
                               (length ctx-assembled)))

  ;; Dispatch 'context hook — extensions can amend final context
  (define-values (ctx-final _ctx-hook) (maybe-dispatch-hooks ext-reg 'context ctx-assembled))

  ctx-final)

;; Run the provider turn: dispatch before-provider-request hook, then run agent turn.
;; Returns the loop-result from run-agent-turn.
(define (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token config)
  ;; Dispatch 'before-provider-request hook (informational)
  (define-values (_bpr-payload _bpr-res)
    (maybe-dispatch-hooks ext-reg
                          'before-provider-request
                          (hasheq 'session-id session-id 'turn-id turn-id)))

  ;; Get tools from registry for the LLM request
  (define base-tools (and reg (list-tools-jsexpr reg)))

  ;; #673: Merge extension-provided tools into the tool list
  (define tools
    (let ([ext-tools (and ext-reg
                          (let ()
                            (define-values (amended hook-res)
                              (maybe-dispatch-hooks ext-reg 'register-tools (hasheq)))
                            (if (and hook-res (eq? (hook-result-action hook-res) 'amend))
                                (hash-ref (hook-result-payload hook-res) 'tools '())
                                '())))])
      (if (and base-tools (pair? ext-tools))
          (merge-tool-lists base-tools ext-tools)
          base-tools)))

  ;; v0.14.4 Wave 2 FIX: Extract ONLY provider-specific settings from config.
  ;; The full config is a mutable hash with event-bus, extension-registry, etc.
  ;; Passing it to make-model-request causes hash-set contract violations
  ;; because provider.rkt's ensure-model-setting calls hash-set (immutable-only).
  (define provider-settings-raw
    (for/hash ([(k v) (in-hash config)]
               #:when (memq k '(max-tokens temperature top_p frequency_penalty presence_penalty)))
      (values k v)))
  ;; v0.15.1 Wave 1: Also resolve max-tokens from config if not in flat runtime hash.
  ;; Config may have max-tokens in: top-level, providers.<name>.max-tokens, or models.default.max-tokens.
  (define provider-settings
    (let* ([settings (hash-ref config 'settings #f)]
           [model-name (hash-ref config 'model-name #f)]
           [resolve-max-tokens
            (lambda ()
              (or
               (hash-has-key? provider-settings-raw 'max-tokens)
               (and settings (setting-ref settings 'max-tokens #f))
               (and settings
                    model-name
                    (setting-ref* settings `(providers ,(string->symbol model-name) max-tokens) #f))
               (and settings (setting-ref* settings '(providers openai-compatible max-tokens) #f))
               (and settings (setting-ref* settings '(models default max-tokens) #f))))])
      (if (and settings (not (hash-has-key? provider-settings-raw 'max-tokens)))
          (let ([mt (resolve-max-tokens)])
            (if mt
                (hash-set provider-settings-raw 'max-tokens mt)
                provider-settings-raw))
          provider-settings-raw)))

  (define ctx-for-retry (box ctx-final))

  (with-auto-retry (lambda ()
                     (run-agent-turn (unbox ctx-for-retry)
                                     prov
                                     bus
                                     #:session-id session-id
                                     #:turn-id turn-id
                                     #:tools tools
                                     #:cancellation-token token
                                     #:provider-settings provider-settings))
                   #:max-retries 2
                   #:base-delay-ms 1000
                   #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                                (publish! bus
                                          (make-event "auto-retry.start"
                                                      (current-inexact-milliseconds)
                                                      session-id
                                                      turn-id
                                                      (hasheq 'attempt
                                                              attempt
                                                              'max-retries
                                                              max-retries
                                                              'delay-ms
                                                              delay-ms
                                                              'error
                                                              error-msg
                                                              'errorType
                                                              error-type))))))

;; Handle tool-calls-pending: extract calls, run through scheduler, emit events,
;; and return (values tool-result-messages updated-ctx) for the next loop iteration.
(define (handle-tool-calls-pending new-msgs
                                   ctx-with-steering
                                   ext-reg
                                   reg
                                   bus
                                   session-id
                                   log-path
                                   token
                                   config)
  ;; Extract tool calls from assistant messages
  (define tool-calls (extract-tool-calls-from-messages new-msgs))

  ;; Find assistant message ID for tool-result parent
  (define assistant-msg-id
    (let ([asst-msgs (filter (lambda (m) (eq? (message-role m) 'assistant)) new-msgs)])
      (if (null? asst-msgs)
          #f
          (message-id (last asst-msgs)))))

  ;; Dispatch 'tool-call hook (F2: renamed from 'before-tool-execution)
  (define-values (tool-calls-to-run tool-call-blocked?)
    (let-values ([(amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-call tool-calls)])
      (if (and hook-res (eq? (hook-result-action hook-res) 'block))
          (values '() #t)
          (values amended #f))))

  ;; #1208: Dispatch 'tool.execution.start hook before tool batch
  (when (and ext-reg (not (null? tool-calls-to-run)))
    (maybe-dispatch-hooks
     ext-reg
     'tool.execution.start
     (hasheq 'tools (map tool-call-name tool-calls-to-run) 'count (length tool-calls-to-run))))

  ;; Run tool batch through scheduler (skip if blocked)
  (define sched-result
    (if tool-call-blocked?
        (scheduler-result '() '() #f)
        (let ([hook-dispatcher-fn (and ext-reg
                                       (lambda (hook-point payload)
                                         (dispatch-hooks hook-point payload ext-reg)))])
          (run-tool-batch
           tool-calls-to-run
           reg
           #:hook-dispatcher hook-dispatcher-fn
           #:exec-context
           (make-exec-context
            #:working-directory (path-only log-path)
            #:cancellation-token token
            #:event-publisher (lambda (event-type payload)
                                (emit-session-event! bus session-id event-type payload))
            #:runtime-settings (or (hash-ref config 'settings #f)
                                   (make-minimal-settings #:provider (hash-ref config 'provider #f)
                                                          #:model (hash-ref config 'model-name #f)))
            #:call-id (generate-id)
            #:session-metadata
            (hasheq 'session-id session-id 'session-index (hash-ref config 'session-index #f)))
           #:parallel? (hash-ref config 'parallel-tools #t)))))

  ;; #1208: Dispatch 'tool.execution.end hook after tool batch
  (when (and ext-reg (not tool-call-blocked?))
    (maybe-dispatch-hooks
     ext-reg
     'tool.execution.end
     (hasheq
      'tools
      (for/list ([tc (in-list tool-calls-to-run)]
                 [tr (in-list (scheduler-result-results sched-result))])
        (hasheq 'name (tool-call-name tc) 'status (if (tool-result-is-error? tr) 'error 'completed)))
      'count
      (length tool-calls-to-run))))

  ;; Emit tool.call.completed / tool.call.failed events (only for executed calls)
  (for ([tc (in-list tool-calls-to-run)]
        [tr (in-list (scheduler-result-results sched-result))])
    (if (tool-result-is-error? tr)
        (emit-session-event! bus
                             session-id
                             "tool.call.failed"
                             (hasheq 'name (tool-call-name tc) 'error (tool-result-content tr)))
        (emit-session-event! bus
                             session-id
                             "tool.call.completed"
                             (hasheq 'name (tool-call-name tc) 'result (tool-result-content tr)))))

  ;; Convert scheduler results to tool-result messages.
  ;; When blocked, synthesize error results for ALL original tool calls
  ;; so the LLM sees a proper tool-result for every tool_call (#446).
  (define tool-result-msgs
    (if tool-call-blocked?
        (make-tool-result-messages tool-calls
                                   (for/list ([tc (in-list tool-calls)])
                                     (make-error-result (format "Tool call '~a' blocked by extension"
                                                                (tool-call-name tc))))
                                   assistant-msg-id)
        (make-tool-result-messages tool-calls-to-run
                                   (scheduler-result-results sched-result)
                                   assistant-msg-id)))

  ;; Dispatch 'tool-result hook (F2: renamed from 'after-tool-execution)
  (define tool-result-msgs-amended
    (let-values ([(amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-result tool-result-msgs)])
      (if (and hook-res (eq? (hook-result-action hook-res) 'block)) tool-result-msgs amended)))

  ;; F1: Validate — filter to only message? values
  (define validated-msgs (filter message? tool-result-msgs-amended))

  ;; Append validated tool results to log
  (append-entries! log-path validated-msgs)

  ;; Return updated context for next iteration
  (append ctx-with-steering new-msgs validated-msgs))

;; ============================================================
;; FEAT-66: Context overflow recovery
;; ============================================================

;; Handle context overflow by compacting the context and retrying once.
;; Uses compact-history for proper summarization and build-retry-messages
;; to re-inject the original user prompt after compaction.
;; Returns the result of the retry, or re-raises if not an overflow error.
(define (call-with-overflow-recovery thunk ctx bus session-id)
  (with-handlers ([context-overflow-error?
                   (lambda (e)
                     (emit-session-event! bus
                                          session-id
                                          "context.overflow.detected"
                                          (hasheq 'error (exn-message e)))
                     ;; Compact the context properly
                     (define compact-result (compact-history ctx))
                     (emit-session-event! bus
                                          session-id
                                          "context.overflow.compacted"
                                          (hasheq 'original-size
                                                  (length ctx)
                                                  'removed-count
                                                  (compaction-result-removed-count compact-result)
                                                  'kept-count
                                                  (length (compaction-result-kept-messages
                                                           compact-result))))
                     (thunk))]
                  [exn:fail? (lambda (e)
                               ;; Re-raise non-overflow errors
                               (raise e))])
    (thunk)))

;; ============================================================
;; FEAT-61: Message injection drain
;; ============================================================

;; Drain all pending injected messages from the event bus.
;; Subscribes to "message.injected" events on first call (per bus+box pair).
;; Returns a list of message? structs ready for context inclusion.
(define (drain-injected-messages! bus injected-box session-id)
  (define msgs (unbox injected-box))
  (if (null? msgs)
      '()
      (begin
        (set-box! injected-box '())
        (filter message? msgs))))

;; Create an injected-message collector: subscribes to message.injected events
;; on the bus and accumulates them in a box.
(define (make-injected-collector! bus)
  (define collected (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) injection-event-topic)
                  (define payload (event-payload evt))
                  (define msg (hash-ref payload 'message #f))
                  (when msg
                    ;; Thread-safe append using box swap
                    (let loop ()
                      (define old (unbox collected))
                      (unless (box-cas! collected old (append old (list msg)))
                        (loop))))))
              #:filter (lambda (evt) (equal? (event-ev evt) injection-event-topic)))
  collected)

;; ============================================================
;; v0.15.2 (BUG-INTENT-WITHOUT-ACTION): Intent-without-action detection
;; ============================================================

;; Regex matching intent-to-act phrases followed by action verbs.
;; Case-insensitive via pregexp (?i:...) syntax.
(define INTENT-ACTION-RX
  #px"(?i:(?:I'll|I will|let me|now I'll|now let me) (?:write|create|rewrite|edit|update|modify|fix|refactor|implement|build|generate))")

;; detect-intent-pattern : (or/c string? #f) -> boolean?
;; Returns #t if the text matches intent-to-act patterns like
;; "I'll rewrite the file" or "Let me create a new script".
(define (detect-intent-pattern text)
  (and (string? text) (> (string-length text) 0) (regexp-match? INTENT-ACTION-RX text)))

;; extract-last-assistant-text : (listof message?) -> (or/c string? #f)
;; Extracts the text content from the last assistant message in the context.
(define (extract-last-assistant-text ctx)
  (for/first ([msg (in-list (reverse ctx))]
              #:when (eq? (message-role msg) 'assistant))
    (define content (message-content msg))
    (define texts
      (for/list ([part (in-list content)]
                 #:when (text-part? part))
        (text-part-text part)))
    (if (null? texts)
        #f
        (string-join texts " "))))

;; ============================================================
;; Iteration loop
;; ============================================================

(define (run-iteration-loop context
                            prov
                            bus
                            reg
                            ext-reg
                            log-path
                            session-id
                            max-iterations
                            #:cancellation-token [token #f]
                            #:config [config (hash)]
                            #:queue [steering-queue #f]
                            #:follow-up-delivery-mode [follow-up-mode 'all]
                            ;; FEAT-61: injected message collector box (created by caller)
                            #:injected-box [injected-box #f]
                            ;; #1158: shutdown check thunks (avoid circular dep on agent-session)
                            #:shutdown-check [shutdown-check #f]
                            #:force-shutdown-check [force-shutdown-check #f])
  ;; v0.14.1: Soft limit = max-iterations (warn), Hard limit = max-iterations-hard (stop)
  ;; v0.14.4 Wave 1: Default hard limit = max(max-iterations * 1.6, 80) for slow models
  (define max-iterations-hard
    (hash-ref config 'max-iterations-hard (max (inexact->exact (floor (* max-iterations 1.6))) 80)))
  ;; Dispatch 'before-agent-start hook — extensions can block or modify config
  (define agent-start-payload
    (hasheq 'session-id
            session-id
            'max-iterations
            max-iterations
            'context-message-count
            (length context)))
  (define-values (amended-start start-hook-res)
    (maybe-dispatch-hooks ext-reg 'before-agent-start agent-start-payload))
  (when (and start-hook-res (eq? (hook-result-action start-hook-res) 'block))
    (emit-session-event! bus
                         session-id
                         "agent.blocked"
                         (hasheq 'reason "extension-block" 'hook 'before-agent-start)))
  (if (and start-hook-res (eq? (hook-result-action start-hook-res) 'block))
      (make-loop-result '() 'completed (hasheq 'reason "extension-block"))
      (let loop ([ctx context]
                 [iteration 0]
                 [consecutive-tool-count 0]
                 [intent-retry-count 0])

        ;; ── Cooperative cancellation check (between iterations) ──
        ;; R2-5 + FEAT-61: Check steering queue + drain injected messages.
        (define ctx-with-steering
          (if steering-queue
              (let ([steering-msgs (dequeue-all-steering! steering-queue)])
                ;; #664: Emit queue-status event for TUI status bar
                (let ([qs (queue-status steering-queue)])
                  (when (or (> (hash-ref qs 'steering 0) 0) (> (hash-ref qs 'followup 0) 0))
                    (emit-session-event! bus session-id "queue.status-update" qs)))
                (if (null? steering-msgs)
                    ctx
                    (append ctx steering-msgs)))
              ctx))

        ;; FEAT-61: Drain any extension-injected messages
        (define ctx-with-injected
          (if injected-box
              (let ([injected (drain-injected-messages! bus injected-box session-id)])
                (if (null? injected)
                    ctx-with-steering
                    (begin
                      (emit-session-event! bus
                                           session-id
                                           "message.injected.drain"
                                           (hasheq 'count (length injected)))
                      (append ctx-with-steering injected))))
              ctx-with-steering))

        (cond
          ;; #1158: Forced shutdown (double Ctrl+C) — immediate abort
          [(and force-shutdown-check (force-shutdown-check))
           (emit-session-event! bus
                                session-id
                                "turn.cancelled"
                                (hasheq 'reason "force-shutdown" 'iteration iteration))
           (make-loop-result (append ctx-with-injected '())
                             'cancelled
                             (hasheq 'reason "force-shutdown" 'iteration iteration))]

          [(and token (cancellation-token-cancelled? token))
           (emit-session-event! bus
                                session-id
                                "turn.cancelled"
                                (hasheq 'reason "cancellation-token" 'iteration iteration))
           (make-loop-result (append ctx-with-injected '())
                             'cancelled
                             (hasheq 'reason "cancellation-token" 'iteration iteration))]

          ;; #1158: Graceful shutdown — finish after current iteration
          [(and shutdown-check (shutdown-check))
           (emit-session-event! bus
                                session-id
                                "turn.cancelled"
                                (hasheq 'reason "graceful-shutdown" 'iteration iteration))
           (make-loop-result (append ctx-with-injected '())
                             'completed
                             (hasheq 'reason "graceful-shutdown" 'iteration iteration))]

          [else
           (define turn-id (generate-id))

           ;; Dispatch 'turn-start hook — extensions can amend context or block
           (define-values (ctx-to-use turn-blocked?)
             (let-values ([(amended-ctx hook-res)
                           (maybe-dispatch-hooks ext-reg 'turn-start ctx-with-injected)])
               (if (and hook-res (eq? (hook-result-action hook-res) 'block))
                   (values ctx-with-injected #t)
                   (values amended-ctx #f))))

           (cond
             ;; ── Turn blocked by extension ──
             [turn-blocked?
              (emit-session-event! bus session-id "turn.blocked" (hasheq 'reason "extension-block"))
              (make-loop-result '() 'completed (hasheq 'reason "extension-block"))]

             [else
              ;; Build assembled context (tiered + hooks)
              (define ctx-final
                (build-assembled-context ctx-to-use config ext-reg bus session-id iteration))

              ;; Run provider turn
              (define result
                (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token config))

              (define termination (loop-result-termination-reason result))
              (define new-msgs (loop-result-messages result))

              ;; v0.15.0 Wave 1: emit iteration.decision for trace logging
              (emit-session-event! bus
                                   session-id
                                   "iteration.decision"
                                   (hasheq 'iteration
                                           (add1 iteration)
                                           'termination
                                           termination
                                           'consecutive_tools
                                           consecutive-tool-count
                                           'max_iterations
                                           max-iterations
                                           'max_iterations_hard
                                           max-iterations-hard))

              (cond
                ;; ── Completed: append and return ──
                [(eq? termination 'completed)
                 (append-entries! log-path new-msgs)
                 ;; Dispatch 'turn-end hook
                 (let-values ([(amended-result after-hook-res)
                               (maybe-dispatch-hooks ext-reg 'turn-end result)])
                   (let ([effective-result (if (and after-hook-res
                                                    (eq? (hook-result-action after-hook-res) 'amend))
                                               amended-result
                                               result)])
                     ;; #662: Drain follow-up queue — if follow-ups exist,
                     ;; inject as user messages and continue the loop.
                     ;; #763: Support 'all (drain all) and 'one-at-a-time modes.
                     (define base-result
                       (if steering-queue
                           (let ([followups (if (eq? follow-up-mode 'one-at-a-time)
                                                (let ([one (dequeue-followup! steering-queue)])
                                                  (if one
                                                      (list one)
                                                      '()))
                                                (dequeue-all-followups! steering-queue))])
                             (if (null? followups)
                                 effective-result
                                 (let ([followup-msgs (for/list ([fu (in-list followups)])
                                                        (make-message (generate-id)
                                                                      #f
                                                                      'user
                                                                      'text
                                                                      (list (make-text-part fu))
                                                                      (now-seconds)
                                                                      (hasheq 'source "followup")))])
                                   (emit-session-event! bus
                                                        session-id
                                                        "followup.injected"
                                                        (hasheq 'count
                                                                (length followups)
                                                                'mode
                                                                (symbol->string follow-up-mode)))
                                   (append-entries! log-path followup-msgs)
                                   (loop (append ctx-with-injected new-msgs followup-msgs)
                                         (add1 iteration)
                                         0
                                         intent-retry-count))))
                           effective-result))
                     ;; v0.15.2 (BUG-INTENT-WITHOUT-ACTION): If model expressed intent
                     ;; to write/edit but made no tool call, inject a steering nudge.
                     (define assistant-text (extract-last-assistant-text ctx-with-injected))
                     (define intent-detected? (detect-intent-pattern assistant-text))
                     (define had-tool-calls?
                       (not (null? (extract-tool-calls-from-messages new-msgs))))
                     (if (and intent-detected? (not had-tool-calls?) (< intent-retry-count 1))
                         (begin
                           (emit-session-event!
                            bus
                            session-id
                            "intent.nudge"
                            (hasheq 'text assistant-text 'intent-retry-count intent-retry-count))
                           (let ([nudge
                                  (make-message
                                   (generate-id)
                                   #f
                                   'user
                                   'message
                                   (list
                                    (make-text-part
                                     "[steering:intent] Your previous message said you would perform an action "
                                     "but no tool was used. Use the write or edit tool now. Do not explain — act."))
                                   (now-seconds)
                                   (hasheq 'source "intent-without-action"))])
                             (append-entries! log-path (list nudge))
                             (loop (append ctx-with-injected new-msgs (list nudge))
                                   (add1 iteration)
                                   0
                                   (add1 intent-retry-count))))
                         base-result)))]

                ;; ── Hard iteration limit reached: append and stop ──
                [(and (eq? termination 'tool-calls-pending) (>= (add1 iteration) max-iterations-hard))
                 (append-entries! log-path new-msgs)
                 (emit-session-event! bus
                                      session-id
                                      "runtime.error"
                                      (hasheq 'error
                                              "max-iterations-exceeded"
                                              'iteration
                                              iteration
                                              'maxIterations
                                              max-iterations-hard))
                 (make-loop-result new-msgs
                                   'max-iterations-exceeded
                                   (hash-set (loop-result-metadata result) 'maxIterationsReached #t))]

                ;; ── Soft iteration limit: warn but continue ──
                [(and (eq? termination 'tool-calls-pending)
                      (>= (add1 iteration) max-iterations)
                      (< (add1 iteration) max-iterations-hard))
                 (append-entries! log-path new-msgs)
                 (emit-session-event! bus
                                      session-id
                                      "iteration.soft-warning"
                                      (hasheq 'iteration
                                              (add1 iteration)
                                              'soft-limit
                                              max-iterations
                                              'hard-limit
                                              max-iterations-hard
                                              'remaining
                                              (- max-iterations-hard (add1 iteration))))
                 (define updated-ctx
                   (handle-tool-calls-pending new-msgs
                                              ctx-with-injected
                                              ext-reg
                                              reg
                                              bus
                                              session-id
                                              log-path
                                              token
                                              config))
                 ;; v0.14.1: mid-turn token budget check
                 (check-mid-turn-budget! updated-ctx bus session-id config)
                 (loop updated-ctx (add1 iteration) (add1 consecutive-tool-count) intent-retry-count)]

                ;; ── Tool calls pending: execute tools and re-run ──
                [(eq? termination 'tool-calls-pending)
                 (append-entries! log-path new-msgs)
                 (define updated-ctx
                   (handle-tool-calls-pending new-msgs
                                              ctx-with-injected
                                              ext-reg
                                              reg
                                              bus
                                              session-id
                                              log-path
                                              token
                                              config))
                 ;; v0.14.1: emit exploration progress after 4+ consecutive tool-only turns
                 (when (>= consecutive-tool-count 4)
                   (define tool-names
                     (for/list ([tc (in-list (extract-tool-calls-from-messages new-msgs))])
                       (tool-call-name tc)))
                   (emit-session-event! bus
                                        session-id
                                        "exploration.progress"
                                        (hasheq 'consecutive-tools
                                                (add1 consecutive-tool-count)
                                                'tool-names
                                                tool-names
                                                'iteration
                                                (add1 iteration))))
                 ;; v0.15.1 Wave 3: Multi-level exploration steering escalation
                 ;; with tool-type-aware counting.
                 ;; - consecutive-tool-count resets when file writes are detected
                 ;; - Level 1 (5+): gentle nudge to use write/edit tools
                 ;; - Level 2 (7+): strong nudge with explicit instruction
                 ;; - Level 3 (12+): hard cap — inject message and break the loop
                 (define has-file-write?
                   (for/or ([tc (in-list (extract-tool-calls-from-messages new-msgs))])
                     (member (tool-call-name tc) '("write" "edit" "replace" "create"))))
                 (define effective-tool-count
                   (if has-file-write?
                       0
                       (add1 consecutive-tool-count)))
                 (define exec-context updated-ctx)
                 (cond
                   ;; Level 3: Hard cap at 12 — force-stop the exploration
                   [(>= effective-tool-count 12)
                    (set!
                     exec-context
                     (append
                      exec-context
                      (list
                       (make-message
                        (generate-id)
                        #f
                        'user
                        'message
                        (list (make-text-part
                               (format
                                (string-append
                                 "[steering:hard] You've made ~a consecutive read-only tool calls. "
                                 "This is beyond the exploration budget. "
                                 "You MUST now use the write or edit tool to produce output, "
                                 "or explain what you cannot do.")
                                effective-tool-count)))
                        (now-seconds)
                        (hasheq)))))
                    (emit-session-event!
                     bus
                     session-id
                     "exploration.hard-cap"
                     (hasheq 'consecutive-tools effective-tool-count 'iteration (add1 iteration)))]
                   ;; Level 2: Strong nudge at 7+
                   [(>= effective-tool-count 7)
                    (set!
                     exec-context
                     (append
                      exec-context
                      (list
                       (make-message
                        (generate-id)
                        #f
                        'user
                        'message
                        (list (make-text-part
                               (format
                                (string-append
                                 "[steering:strong] ~a consecutive read-only tool calls detected. "
                                 "Stop exploring. You MUST produce the actual output now "
                                 "using the write or edit tool. "
                                 "Do NOT run any more read-only commands.")
                                effective-tool-count)))
                        (now-seconds)
                        (hasheq)))))]
                   ;; Level 1: Gentle nudge at 5+
                   [(>= effective-tool-count 5)
                    (set!
                     exec-context
                     (append
                      exec-context
                      (list
                       (make-message
                        (generate-id)
                        #f
                        'user
                        'message
                        (list (make-text-part
                               (format (string-append
                                        "[steering] You've made ~a consecutive read-only tool calls. "
                                        "You MUST now use the write or edit tool to produce output. "
                                        "Do not explain what you will do — just do it.")
                                       effective-tool-count)))
                        (now-seconds)
                        (hasheq)))))])
                 ;; v0.14.1: mid-turn token budget check
                 (check-mid-turn-budget! exec-context bus session-id config)
                 (loop exec-context (add1 iteration) effective-tool-count intent-retry-count)]

                ;; ── Unknown termination: append and return ──
                [else
                 (append-entries! log-path new-msgs)
                 result])])]))))
