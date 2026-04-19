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
         (only-in racket/string string-trim)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  make-tool-result-part
                  make-text-part
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
         (only-in "../runtime/settings.rkt" make-minimal-settings setting-ref)
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
         (only-in "../extensions/message-inject.rkt" injection-event-topic))

(provide run-iteration-loop
         emit-session-event!
         maybe-dispatch-hooks
         ensure-hash-args ;; for testing
         ;; FEAT-61: message injection support
         make-injected-collector!
         drain-injected-messages!
         ;; FEAT-66: overflow recovery (for testing)
         call-with-overflow-recovery)

;; ============================================================
;; Shared helpers
;; ============================================================

(define (emit-session-event! bus sid event-name payload)
  (define evt (make-event event-name (now-seconds) sid #f payload))
  (publish! bus evt)
  evt)

;; Safely dispatch hooks if extension-registry is present.
;; Returns (values amended-payload hook-result) or (values payload #f) if no registry.
(define (maybe-dispatch-hooks ext-reg hook-point payload)
  (if ext-reg
      (let ([result (dispatch-hooks hook-point payload ext-reg)])
        (values (hook-result-payload result) result))
      (values payload #f)))

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
(define (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token)
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

  (define ctx-for-retry (box ctx-final))

  (with-auto-retry
   (lambda ()
     (run-agent-turn (unbox ctx-for-retry)
                     prov
                     bus
                     #:session-id session-id
                     #:turn-id turn-id
                     #:tools tools
                     #:cancellation-token token))
   #:max-retries 2
   #:base-delay-ms 1000
   #:context-reducer
   (lambda (attempt)
     ;; On timeout retry: trim oldest non-system messages.
     ;; Keep system prompt + last N messages to reduce payload.
     (define ctx (unbox ctx-for-retry))
     (define n (length ctx))
     (define keep-count (max 4 (quotient n 2)))
     (define system-msgs (filter (lambda (m) (eq? (message-role m) 'system)) ctx))
     (define non-system-msgs (filter (lambda (m) (not (eq? (message-role m) 'system))) ctx))
     (define trimmed-non-system (takef (reverse non-system-msgs) (lambda (_) #t)))
     ;; Take last keep-count non-system messages
     (define kept-non-system
       (if (> (length trimmed-non-system) keep-count)
           (take trimmed-non-system keep-count)
           trimmed-non-system))
     (define reduced-ctx (append system-msgs (reverse kept-non-system)))
     (set-box! ctx-for-retry reduced-ctx)
     ;; Emit context-reduced event for TUI visibility
     (publish!
      bus
      (make-event
       "auto-retry.context-reduced"
       (current-inexact-milliseconds)
       session-id
       turn-id
       (hasheq 'original-messages n 'reduced-messages (length reduced-ctx) 'attempt attempt)))
     (lambda ()
       (run-agent-turn (unbox ctx-for-retry)
                       prov
                       bus
                       #:session-id session-id
                       #:turn-id turn-id
                       #:tools tools
                       #:cancellation-token token)))
   #:on-retry
   (lambda (attempt max-retries delay-ms error-msg)
     (publish!
      bus
      (make-event
       "auto-retry.start"
       (current-inexact-milliseconds)
       session-id
       turn-id
       (hasheq 'attempt attempt 'max-retries max-retries 'delay-ms delay-ms 'error error-msg))))))

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
            #:session-metadata (hasheq 'session-id session-id))
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
                 [iteration 0])

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
                (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token))

              (define termination (loop-result-termination-reason result))
              (define new-msgs (loop-result-messages result))

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
                                       (add1 iteration)))))
                         effective-result)))]

                ;; ── Max iterations reached: append and stop ──
                [(and (eq? termination 'tool-calls-pending) (>= (add1 iteration) max-iterations))
                 (append-entries! log-path new-msgs)
                 (emit-session-event! bus
                                      session-id
                                      "runtime.error"
                                      (hasheq 'error
                                              "max-iterations-exceeded"
                                              'iteration
                                              iteration
                                              'maxIterations
                                              max-iterations))
                 (make-loop-result new-msgs
                                   'max-iterations-exceeded
                                   (hash-set (loop-result-metadata result) 'maxIterationsReached #t))]

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
                 (loop updated-ctx (add1 iteration))]

                ;; ── Unknown termination: append and return ──
                [else
                 (append-entries! log-path new-msgs)
                 result])])]))))
