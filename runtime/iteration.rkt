#lang racket/base

;; runtime/iteration.rkt — agent iteration loop
;;
;; Extracted from agent-session.rkt for single-responsibility separation.
;; Handles the multi-turn tool-call iteration loop: run agent turn → check
;; for tool calls → execute tools → feed results back → repeat.
;;
;; Also provides shared helpers used by agent-session.rkt:
;;   now-seconds, emit-session-event!, maybe-dispatch-hooks

(require racket/contract
         racket/list
         racket/path
         json
         (only-in racket/string string-trim)
         (only-in "../agent/types.rkt"
                  message? message-id message-role message-content
                  make-message make-tool-result-part
                  tool-call-part? tool-call-part-id tool-call-part-name tool-call-part-arguments
                  make-tool-call tool-call-id tool-call-name
                  make-loop-result loop-result-termination-reason loop-result-messages loop-result-metadata
                  make-event)
         "../agent/event-bus.rkt"
         "../agent/queue.rkt"
         "../agent/loop.rkt"
         (only-in "../tools/tool.rkt"
                  make-exec-context
                  tool-result-content tool-result-is-error?
                  list-tools)
         "../tools/scheduler.rkt"
         "../extensions/hooks.rkt"
         "../runtime/session-store.rkt"
         (only-in "../runtime/compactor.rkt"
                  build-tiered-context-with-hooks tiered-context->message-list)
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt"
                  cancellation-token? cancellation-token-cancelled?)
         ;; R2-6: Import hook-result accessors
         (only-in "../extensions/hooks.rkt"
                  hook-result-action hook-result-payload hook-result?))

(provide run-iteration-loop
         now-seconds
         emit-session-event!
         maybe-dispatch-hooks)

;; ============================================================
;; Shared helpers
;; ============================================================

(define (now-seconds)
  (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000))))

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

;; Parse tool-call arguments from string to hash if needed.
;; Streaming produces arguments as JSON strings; tools expect hashes.
(define (ensure-hash-args args)
  (cond
    [(hash? args) args]
    [(string? args)
     (define cleaned (string-trim args))
     (if (or (string=? cleaned "") (string=? cleaned "{}"))
         (hash)
         (with-handlers ([exn:fail? (lambda (_) (hash))])
           (define parsed (string->jsexpr cleaned))
           (if (hash? parsed)
               parsed
               (hash))))]
    [else (hash)]))

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
    (make-message msg-id parent-msg-id 'tool 'tool-result
                  (list (make-tool-result-part
                         (tool-call-id tc)
                         (tool-result-content tr)
                         (tool-result-is-error? tr)))
                  (now-seconds)
                  (hasheq 'toolCallId (tool-call-id tc)
                          'isError (tool-result-is-error? tr)))))

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
;; Iteration loop
;; ============================================================

(define (run-iteration-loop context prov bus reg ext-reg log-path session-id max-iterations
                              #:cancellation-token [token #f]
                              #:config [config (hash)]
                              #:queue [steering-queue #f])
  ;; Dispatch 'before-agent-start hook — extensions can block or modify config
  (define agent-start-payload
    (hasheq 'session-id session-id
            'max-iterations max-iterations
            'context-message-count (length context)))
  (define-values (amended-start start-hook-res)
    (maybe-dispatch-hooks ext-reg 'before-agent-start agent-start-payload))
  (when (and start-hook-res (eq? (hook-result-action start-hook-res) 'block))
    (emit-session-event! bus session-id "agent.blocked"
                         (hasheq 'reason "extension-block" 'hook 'before-agent-start)))
  (if (and start-hook-res (eq? (hook-result-action start-hook-res) 'block))
      (make-loop-result '() 'completed (hasheq 'reason "extension-block"))
      (let loop ([ctx context]
             [iteration 0])

    ;; ── Cooperative cancellation check (between iterations) ──
    ;; R2-5: Check steering queue for injected messages.
    (define ctx-with-steering
      (if steering-queue
          (let ([steering-msgs (dequeue-all-steering! steering-queue)])
            (if (null? steering-msgs)
                ctx
                (append ctx steering-msgs)))
          ctx))

    (cond
      [(and token (cancellation-token-cancelled? token))
       (emit-session-event! bus session-id "turn.cancelled"
                            (hasheq 'reason "cancellation-token"
                                    'iteration iteration))
       (make-loop-result (append ctx-with-steering '()) 'cancelled
                         (hasheq 'reason "cancellation-token"
                                 'iteration iteration))]

      [else
       (define turn-id (generate-id))

       ;; Dispatch 'turn-start hook — extensions can amend context or block (F2: renamed from 'before-turn)
       (define-values (ctx-to-use turn-blocked?)
         (let-values ([(amended-ctx hook-res)
                       (maybe-dispatch-hooks ext-reg 'turn-start ctx-with-steering)])
           (if (and hook-res (eq? (hook-result-action hook-res) 'block))
               (values ctx-with-steering #t)
               (values amended-ctx #f))))

       (cond
         ;; ── Turn blocked by extension: emit event and return completed (F6) ──
         [turn-blocked?
          (emit-session-event! bus session-id "turn.blocked"
                               (hasheq 'reason "extension-block"))
          (make-loop-result '() 'completed (hasheq 'reason "extension-block"))]

         [else
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
            (build-tiered-context-with-hooks
             ctx-to-use
             #:hook-dispatcher ctx-assembly-hook-dispatcher
             #:tier-b-count tier-b-count
             #:tier-c-count tier-c-count
             #:max-tokens max-tokens))

          ;; Handle block action from context-assembly hook
          (when (and assembly-hook-result
                     (eq? (hook-result-action assembly-hook-result) 'block))
            (emit-session-event! bus session-id "context.assembly.blocked"
                                 (hasheq 'reason "extension-block"))
            (raise (exn:fail "Context assembly blocked by extension"
                             (current-continuation-marks))))

          (define ctx-assembled (tiered-context->message-list tc))

          ;; Emit context.assembled event
          (emit-session-event! bus session-id "context.assembled"
                               (hasheq 'iteration iteration
                                       'total-messages (length ctx-to-use)
                                       'assembled-messages (length ctx-assembled)))

          ;; Dispatch 'context hook — extensions can amend final context
          (define-values (ctx-final _ctx-hook)
            (maybe-dispatch-hooks ext-reg 'context ctx-assembled))

       ;; Dispatch 'before-provider-request hook (informational)
       (define-values (_bpr-payload _bpr-res)
         (maybe-dispatch-hooks ext-reg 'before-provider-request
                                   (hasheq 'session-id session-id 'turn-id turn-id)))

       ;; Get tools from registry for the LLM request
       (define tools (and reg (list-tools reg)))

       (define result
         (run-agent-turn ctx-final prov bus
                         #:session-id session-id
                         #:turn-id turn-id
                         #:tools tools
                         #:cancellation-token token))

       (define termination (loop-result-termination-reason result))
       (define new-msgs (loop-result-messages result))

       (cond
         ;; ── Completed: append and return ──
         [(eq? termination 'completed)
          (append-entries! log-path new-msgs)
          ;; Dispatch 'turn-end hook — capture amended result (F2: renamed, F7: capture return)
          (define-values (amended-result after-hook-res)
            (maybe-dispatch-hooks ext-reg 'turn-end result))
          (define effective-result
            (if (and after-hook-res (eq? (hook-result-action after-hook-res) 'amend))
                amended-result
                result))
          effective-result]

         ;; ── Max iterations reached: append and stop ──
         [(and (eq? termination 'tool-calls-pending)
               (>= (add1 iteration) max-iterations))
          (append-entries! log-path new-msgs)
          (emit-session-event! bus session-id "runtime.error"
                               (hasheq 'error "max-iterations-exceeded"
                                       'iteration iteration
                                       'maxIterations max-iterations))
          (make-loop-result new-msgs
                            'max-iterations-exceeded
                            (hash-set (loop-result-metadata result)
                                      'maxIterationsReached #t))]

         ;; ── Tool calls pending: execute tools and re-run ──
         [(eq? termination 'tool-calls-pending)
          (append-entries! log-path new-msgs)

          ;; Extract tool calls from assistant messages
          (define tool-calls (extract-tool-calls-from-messages new-msgs))

          ;; Dispatch 'tool-call hook (F2: renamed from 'before-tool-execution)
          (define tool-calls-to-run
            (let-values ([(amended hook-res)
                          (maybe-dispatch-hooks ext-reg 'tool-call tool-calls)])
              (if (and hook-res (eq? (hook-result-action hook-res) 'block))
                  '()  ; blocked → empty list = skip execution
                  amended)))

          ;; Find assistant message ID for tool-result parent
          (define assistant-msg-id
            (let ([asst-msgs (filter (λ (m) (eq? (message-role m) 'assistant)) new-msgs)])
              (if (null? asst-msgs) #f (message-id (last asst-msgs)))))

          ;; Run tool batch through scheduler
          ;; Pass extension registry as hook-dispatcher so scheduler's
          ;; preflight pipeline can validate and amend tool calls
          (define hook-dispatcher-fn
            (and ext-reg
                 (λ (hook-point payload)
                   (dispatch-hooks hook-point payload ext-reg))))

          (define sched-result
            (run-tool-batch tool-calls-to-run reg
                            #:hook-dispatcher hook-dispatcher-fn
                            #:exec-context (make-exec-context
                                            #:working-directory (path-only log-path)
                                            #:cancellation-token token
                                            #:event-publisher (λ (event-type payload)
                                                                (emit-session-event! bus session-id event-type payload))
                                            #:call-id (generate-id)
                                            #:session-metadata (hasheq 'session-id session-id))
                            #:parallel? (hash-ref config 'parallel-tools #f)))

          ;; Emit tool.call.completed / tool.call.failed events
          (for ([tc (in-list tool-calls-to-run)]
                [tr (in-list (scheduler-result-results sched-result))])
            (if (tool-result-is-error? tr)
                (emit-session-event! bus session-id "tool.call.failed"
                                     (hasheq 'name (tool-call-name tc)
                                             'error (tool-result-content tr)))
                (emit-session-event! bus session-id "tool.call.completed"
                                     (hasheq 'name (tool-call-name tc)
                                             'result (tool-result-content tr)))))

          ;; Convert scheduler results to tool-result messages
          (define tool-result-msgs
            (make-tool-result-messages tool-calls-to-run
                                       (scheduler-result-results sched-result)
                                       assistant-msg-id))

          ;; Dispatch 'tool-result hook (F2: renamed from 'after-tool-execution)
          (define tool-result-msgs-amended
            (let-values ([(amended hook-res)
                          (maybe-dispatch-hooks ext-reg 'tool-result tool-result-msgs)])
              (if (and hook-res (eq? (hook-result-action hook-res) 'block))
                  tool-result-msgs
                  amended)))

          ;; F1: Validate — filter to only message? values
          (define validated-msgs (filter message? tool-result-msgs-amended))

          ;; Append validated tool results to log
          (append-entries! log-path validated-msgs)

          ;; Re-run with updated context (using validated msgs)
          (define updated-ctx (append ctx-with-steering new-msgs validated-msgs))
          (loop updated-ctx (add1 iteration))]

         ;; ── Unknown termination: append and return ──
         [else
          (append-entries! log-path new-msgs)
          result])
         ])
       ])
    )
  ))
