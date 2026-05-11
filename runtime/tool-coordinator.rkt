#lang racket/base

;; runtime/tool-coordinator.rkt — tool call extraction, scheduling, and result assembly
;; STABILITY: internal
;;
;; Extracted from runtime/iteration.rkt (A-02, v0.16.1 Wave 4) for
;; single-responsibility separation. Handles the tool-call lifecycle
;; within an agent iteration:
;;   1. Extract tool calls from assistant messages
;;   2. Dispatch hooks (tool-call, tool.execution.start/end)
;;   3. Run tool batch through scheduler
;;   4. Emit typed tool-execution-start/end events via emit-typed-event!
;;   5. Create tool-result messages
;;   6. Dispatch tool-result hooks
;;
;; ── LAYER EXCEPTION (same as iteration.rkt, ARCH-01 / #341) ──
;; Upward imports into tools/ and extensions/ layers. See iteration.rkt
;; header comment for full rationale.

(require racket/contract
         racket/contract
         racket/list
         racket/path
         json
         (only-in "../runtime/session-config.rkt"
                  session-config?
                  hash->session-config
                  config-settings
                  config-provider
                  config-model-name
                  config-session-index
                  config-parallel-tools)
         (only-in "../util/tool-types.rkt" tool-call?)
         (only-in "../tools/tool.rkt" tool-result? tool-registry?)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  make-tool-result-part
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-call
                  tool-call-id
                  tool-call-name
                  tool-result-is-error?
                  tool-result-content
                  make-event)
         "../agent/event-bus.rkt"
         ;; ARCH-01 upward import — runtime→tools
         (only-in "../tools/tool.rkt" make-exec-context make-error-result list-tools-jsexpr)
         ;; ARCH-01 upward import — runtime→tools: scheduler
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result scheduler-result-results)
         ;; ARCH-01 upward import — runtime→extensions: hooks
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../extensions/api.rkt" extension-registry?)
         (only-in "../agent/event-bus.rkt" event-bus?)
         (only-in "../runtime/session-store.rkt" append-entries!)
         (only-in "../runtime/settings.rkt" make-minimal-settings setting-ref setting-ref*)
         (only-in "../util/content-helpers.rkt" tool-result-content->string)
         (only-in "../util/ids.rkt" generate-id now-seconds)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         ;; QUAL-01 (v0.22.0): shared runtime helpers
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         ;; G13: typed event emission for tool execution
         (only-in "../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../agent/event-structs/tool-events.rkt"
                  make-tool-execution-start-event
                  make-tool-execution-end-event))

(provide (contract-out [extract-tool-calls-from-messages (-> (listof message?) (listof tool-call?))]
                       [make-tool-result-messages
                        (-> (listof tool-call?) (listof tool-result?) string? (listof message?))]
                       [handle-tool-calls-pending
                        (-> (listof message?) ; new-msgs
                            list? ; ctx-with-steering
                            (or/c extension-registry? #f) ; ext-reg
                            (or/c tool-registry? #f) ; reg
                            event-bus? ; bus
                            string? ; session-id
                            (or/c path-string? path?) ; log-path
                            (or/c cancellation-token? #f) ; token
                            any/c ; config
                            (listof message?))]))

;; v0.31.5 W1: export struct
(provide (struct-out tool-call-actions))

;; ============================================================
;; Helpers (QUAL-01: emit-session-event! and maybe-dispatch-hooks
;;           now imported from runtime-helpers.rkt)
;; ============================================================

;; now-seconds imported from util/ids.rkt

;; ============================================================
;; Exported functions
;; ============================================================

;; v0.31.5 W1: Pure function for tool-call actions
(struct tool-call-actions (calls-to-run blocked? final-calls) #:transparent)

(define (compute-tool-call-actions tool-calls amended hook-res)
  (if (and hook-res (eq? (hook-result-action hook-res) 'block))
      (tool-call-actions '() #t tool-calls)
      (tool-call-actions amended #f '())))

;; Extract tool-call structs from assistant messages.
(define (extract-tool-calls-from-messages messages)
  (for*/list ([msg (in-list messages)]
              #:when (eq? (message-role msg) 'assistant)
              [part (in-list (message-content msg))]
              #:when (tool-call-part? part))
    (make-tool-call (tool-call-part-id part)
                    (tool-call-part-name part)
                    (ensure-hash-args (tool-call-part-arguments part)))))

;; Create tool-result messages from scheduler results.
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

;; Handle tool-calls-pending: extract calls, run through scheduler, emit events,
;; and return updated context for the next loop iteration.
(define (handle-tool-calls-pending new-msgs
                                   ctx-with-steering
                                   ext-reg
                                   reg
                                   bus
                                   session-id
                                   log-path
                                   token
                                   config-raw)
  (define config
    (if (session-config? config-raw)
        config-raw
        (hash->session-config config-raw)))
  ;; Extract tool calls from assistant messages
  (define tool-calls (extract-tool-calls-from-messages new-msgs))

  ;; Find assistant message ID for tool-result parent
  (define assistant-msg-id
    (let ([asst-msgs (filter (lambda (m) (eq? (message-role m) 'assistant)) new-msgs)])
      (if (null? asst-msgs)
          #f
          (message-id (last asst-msgs)))))

  ;; Dispatch 'tool-call hook
  (define-values (amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-call tool-calls))
  (define actions (compute-tool-call-actions tool-calls amended hook-res))
  (define tool-calls-to-run (tool-call-actions-calls-to-run actions))
  (define tool-call-blocked? (tool-call-actions-blocked? actions))

  ;; Dispatch 'tool.execution.start hook before tool batch
  (when (and ext-reg (not (null? tool-calls-to-run)))
    (maybe-dispatch-hooks
     ext-reg
     'tool.execution.start
     (hasheq 'tools (map tool-call-name tool-calls-to-run) 'count (length tool-calls-to-run))))

  ;; Capture batch start time for per-batch duration (v0.29.16)
  (define batch-start-ms (current-inexact-milliseconds))
  ;; W-05: per-tool start time tracking for accurate duration-ms
  (define per-tool-start-ms (make-hash))

  ;; Run tool batch through scheduler (skip if blocked or no registry)
  (define sched-result
    (cond
      [tool-call-blocked? (scheduler-result '() (hasheq))]
      [(not reg) (scheduler-result '() (hasheq))]
      [else
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
           #:event-publisher
           (lambda (event-type payload)
             (cond
               [(equal? event-type "tool.execution.start")
                ;; W-05: store per-tool start-ms for accurate duration
                (define tcid (hash-ref payload 'tool-call-id))
                (define start-ms (hash-ref payload 'start-ms (current-inexact-milliseconds)))
                (hash-set! per-tool-start-ms tcid start-ms)
                (emit-typed-event! bus
                                   (make-tool-execution-start-event
                                    #:session-id session-id
                                    #:turn-id #f
                                    #:timestamp (current-inexact-milliseconds)
                                    #:tool-name (hash-ref payload 'tool-name)
                                    #:tool-call-id tcid))]
               [else (emit-session-event! bus session-id event-type payload)]))
           #:runtime-settings (or (config-settings config)
                                  (make-minimal-settings #:provider (config-provider config)
                                                         #:model (config-model-name config)))
           #:call-id (generate-id)
           #:session-metadata
           (hasheq 'session-id session-id 'session-index (config-session-index config)))
          #:parallel? (config-parallel-tools config)))]))

  ;; Dispatch 'tool.execution.end hook after tool batch
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

  ;; G13: Emit typed tool-execution-end events (canonical — replaces raw tool.call.completed/failed)
  (for ([tc (in-list tool-calls-to-run)]
        [tr (in-list (scheduler-result-results sched-result))])
    (emit-typed-event! bus
                       (make-tool-execution-end-event
                        #:session-id session-id
                        #:turn-id #f
                        #:timestamp (current-inexact-milliseconds)
                        #:tool-name (tool-call-name tc)
                        ;; W-05: use per-tool start-ms when available, fallback to batch-start
                        #:duration-ms (inexact->exact (floor (- (current-inexact-milliseconds)
                                                                (hash-ref per-tool-start-ms
                                                                          (tool-call-id tc)
                                                                          batch-start-ms))))
                        #:result-summary (if (tool-result-is-error? tr) 'error 'completed))))

  ;; Convert scheduler results to tool-result messages.
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

  ;; Dispatch 'tool-result hook
  (define tool-result-msgs-amended
    (let-values ([(amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-result tool-result-msgs)])
      (if (and hook-res (eq? (hook-result-action hook-res) 'block)) tool-result-msgs amended)))

  ;; F1: Validate — filter to only message? values
  (define validated-msgs (filter message? tool-result-msgs-amended))

  ;; Append validated tool results to log
  (append-entries! log-path validated-msgs)

  ;; Return updated context for next iteration
  (append ctx-with-steering new-msgs validated-msgs))
;; v0.31.5 W1: placeholder for pure function extraction
