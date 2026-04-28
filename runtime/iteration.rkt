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
;;   tools/tool.rkt       → list-tools-jsexpr, merge-tool-lists (registry queries)
;;   extensions/hooks.rkt → dispatch-hooks
;;
;; v0.22.0 (ARCH-01): Removed dead imports (scheduler.rkt, make-exec-context,
;; make-error-result, tool-result-content, tool-result-is-error?).
;; The scheduler is called via tool-coordinator.rkt, not directly.
;; ───────────────────────────────────────────────────────────────

(require racket/contract
         racket/list
         racket/set
         racket/path
         json
         (only-in racket/string string-trim string-join string-contains?)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  make-tool-result-part
                  tool-result-part?
                  tool-result-part-is-error?
                  tool-result-part-content
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
                  tool-call-arguments
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
         ;; ARCH-01: tool registry queries for LLM tool definitions
         ;; the LLM and to construct tool-result messages.
         (only-in "../tools/tool.rkt"
                  list-tools-jsexpr
                  merge-tool-lists)
         ;; Settings struct for exec-context runtime-settings (#1240)
         (only-in "../runtime/settings.rkt" make-minimal-settings setting-ref setting-ref*)
         ;; within each agent turn.
         "../tools/scheduler.rkt"
         ;; ARCH-01 upward import — runtime→extensions: iteration loop
         ;; dispatches lifecycle hooks (turn-start, tool-call, tool-result,
         ;; turn-end, etc.) so extensions can observe/amend/block each step.
         "../extensions/hooks.rkt"
         (only-in "../extensions/context.rkt" make-extension-ctx)
         "../runtime/session-store.rkt"
         "../runtime/tool-coordinator.rkt"
         (only-in "../runtime/compactor.rkt"
                  build-tiered-context-with-hooks
                  tiered-context->message-list
                  compact-history
                  compaction-result-removed-count
                  compaction-result-kept-messages)
         ;; tool-coordinator re-exports these:
         ;;   extract-tool-calls-from-messages, make-tool-result-messages,
         ;;   handle-tool-calls-pending
         (only-in "../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending
                  extract-tool-calls-from-messages)
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
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         ;; QUAL-01 (v0.22.0): shared runtime helpers
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks))

(provide run-iteration-loop
         ;; emit-session-event! and maybe-dispatch-hooks re-exported from runtime-helpers
         emit-session-event!
         maybe-dispatch-hooks
         ensure-hash-args ;; for testing
         ;; v0.18.0: steering helpers for testing
         extract-tool-target-path
         update-seen-paths
         ;; FEAT-61: message injection support
         make-injected-collector!
         drain-injected-messages!
         ;; FEAT-66: overflow recovery (for testing)
         call-with-overflow-recovery
         ;; v0.14.1: mid-turn token budget check (for testing)
         check-mid-turn-budget!
         ;; v0.20.5 W3: pre-registration on session open
         register-session-extensions!)

;; ============================================================
;; Shared helpers (QUAL-01: re-exported from runtime-helpers.rkt)
;; ============================================================

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
;; extract-tool-calls-from-messages: moved to runtime/tool-coordinator.rkt
;; make-tool-result-messages: moved to runtime/tool-coordinator.rkt

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

  ;; Emit context.assembled event (v0.19.12 W1: added tokenCount)
  (define ctx-token-count (estimate-context-tokens ctx-assembled))
  (emit-session-event! bus
                       session-id
                       "context.assembled"
                       (hasheq 'iteration
                               iteration
                               'total-messages
                               (length ctx-to-use)
                               'assembled-messages
                               (length ctx-assembled)
                               'tokenCount
                               ctx-token-count))

  ;; Dispatch 'context hook — extensions can amend final context
  (define-values (ctx-final _ctx-hook) (maybe-dispatch-hooks ext-reg 'context ctx-assembled))

  ctx-final)

;; ============================================================
;; v0.20.5 W3: Extension Pre-Registration
;; ============================================================

;;; register-session-extensions! : tool-registry? extension-registry? event-bus?
;;;                                  string? -> (listof hash?)
;;;
;;; Dispatches the 'register-tools hook through the extension registry
;;; so that extension tools are available and extension state (event bus,
;;; pinned dir) is initialized BEFORE the first run-prompt! call.
;;;
;;; Returns the list of extension-provided tools (as jsexpr hashes),
;;; or '() if no extensions or no tools registered.
;;;
;;; Idempotent: extensions track their own registration state internally.
;;; Safe to call multiple times.
(define (register-session-extensions! tool-reg ext-reg bus session-id)
  (cond
    [(not ext-reg) '()]
    [else
     (define the-ext-ctx
       (make-extension-ctx #:session-id session-id
                           #:session-dir #f
                           #:event-bus bus
                           #:extension-registry ext-reg
                           #:tool-registry tool-reg))
     (define-values (_amended hook-res)
       (maybe-dispatch-hooks ext-reg 'register-tools (hasheq) #:ctx the-ext-ctx))
     (if (and hook-res (eq? (hook-result-action hook-res) 'amend))
         (hash-ref (hook-result-payload hook-res) 'tools '())
         '())]))

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
  ;; v0.20.5 W3: Uses shared register-session-extensions! function.
  ;; Idempotent — extensions track their own state.
  (define ext-tools (and ext-reg (register-session-extensions! reg ext-reg bus session-id)))
  (define tools
    (if (and base-tools (pair? ext-tools))
        (merge-tool-lists base-tools ext-tools)
        base-tools))

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
;; handle-tool-calls-pending: moved to runtime/tool-coordinator.rkt

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
;; v0.21.3: Intent detection removed. The prompt is self-correcting by design.
;; detect-intent-pattern, INTENT-ACTION-RX, detect-stall?, MAX-STALL-RETRIES all removed.

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
;; v0.18.0: Context-aware exploration steering helpers
;; ============================================================

;; Extract the target file path from a tool call's arguments.
;; Returns #f if no path found. Only reliable for read/write/edit tools.
(define (extract-tool-target-path tc)
  (define args (tool-call-arguments tc))
  (cond
    [(hash? args) (or (hash-ref args 'path #f) (hash-ref args 'file #f))]
    [else #f]))

;; Compute the new seen-paths set and whether to increment the counter.
;; Returns (values new-seen-paths should-increment?)
;; v0.19.10: Keep at most n elements from the front of a list.
(define (take-at-most lst n)
  (if (<= (length lst) n)
      lst
      (take lst n)))

(define (update-seen-paths tool-calls seen-paths)
  ;; FIX: Use read-tools blacklist instead of write-tools whitelist.
  ;; Extension tools (planning-write, bash, gh-*) are not in the old whitelist,
  ;; so the counter never resets when the agent uses them. With a blacklist,
  ;; any tool that isn't a read/explore tool resets the counter.
  (define read-tools '("read" "find" "grep" "ls" "planning-read"))
  (define has-non-read?
    (for/or ([tc (in-list tool-calls)])
      (not (member (tool-call-name tc) read-tools))))
  (cond
    [has-non-read? (values (set) #f)] ;; Reset on any non-read tool
    [else
     ;; Collect paths from read tools
     (define new-paths
       (for/set ([tc (in-list tool-calls)]
                 #:when (member (tool-call-name tc) read-tools))
         (define p (extract-tool-target-path tc))
         (or p 'no-path)))
     ;; Any path we haven't seen before?
     (define has-new-path?
       (for/or ([p (in-set new-paths)])
         (not (set-member? seen-paths p))))
     (values (set-union seen-paths new-paths) has-new-path?)]))

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
                 [seen-paths (set)] ;; v0.18.0: same-file dedup tracking
                 [intent-retry-count 0]
                 [consecutive-error-count 0] ;; v0.19.10: spiral breaker
                 [recent-tool-names '()] ;; v0.19.10: bash-only streak detection
                 [explore-count 0] ;; v0.19.11: cumulative explore budget
                 [implement-count 0] ;; v0.19.11: cumulative implement budget
                 [stall-retry-count 0]) ;; v0.20.4 W2: stall detection

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
                     (define followup-continued?
                       (and steering-queue
                            (let ([followups (if (eq? follow-up-mode 'one-at-a-time)
                                                 (let ([one (dequeue-followup! steering-queue)])
                                                   (if one
                                                       (list one)
                                                       '()))
                                                 (dequeue-all-followups! steering-queue))])
                              (if (null? followups)
                                  #f
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
                                          (set)
                                          intent-retry-count
                                          0
                                          '()
                                          explore-count
                                          implement-count
                                          0))))))
                     (define base-result (if followup-continued? effective-result effective-result))
                     ;; v0.21.3: Intent/stall detection removed.
                     ;; The prompt is self-correcting by design - no runtime nudging.
                     base-result))]
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
                 (loop updated-ctx
                       (add1 iteration)
                       (add1 consecutive-tool-count)
                       seen-paths
                       intent-retry-count
                       consecutive-error-count
                       recent-tool-names
                       explore-count
                       implement-count
                       stall-retry-count)]

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
                 ;; v0.21.3: Exploration steering removed. Compute counters without steering.
                 (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
                 (define-values (new-seen-paths should-increment?)
                   (update-seen-paths current-tool-calls seen-paths))
                 (define effective-tool-count
                   (if should-increment?
                       (add1 consecutive-tool-count)
                       consecutive-tool-count))
                 (define new-explore-count
                   (+ explore-count
                      (for/sum ([tc (extract-tool-calls-from-messages new-msgs)])
                               (if (member (tool-call-name tc) '("read" "grep" "find" "ls")) 1 0))))
                 (define new-implement-count
                   (+ implement-count
                      (for/sum ([tc (extract-tool-calls-from-messages new-msgs)])
                               (if (member (tool-call-name tc) '("edit" "write")) 1 0))))
                 (define new-error-count
                   (+ consecutive-error-count
                      (for/sum
                       ([tr (filter tool-result-part? (apply append (map message-content new-msgs)))])
                       (if (tool-result-part-is-error? tr) 1 0))))
                 (define new-recent-tools
                   (take-at-most (append recent-tool-names
                                         (map tool-call-name
                                              (extract-tool-calls-from-messages new-msgs)))
                                 20))
                 (loop updated-ctx
                       (add1 iteration)
                       effective-tool-count
                       new-seen-paths
                       intent-retry-count
                       new-error-count
                       new-recent-tools
                       new-explore-count
                       new-implement-count
                       stall-retry-count)]

                ;; ── Unknown termination: append and return ──
                [else
                 (append-entries! log-path new-msgs)
                 result])])]))))
