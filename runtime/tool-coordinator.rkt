#lang racket/base

;; runtime/tool-coordinator.rkt — tool call extraction, scheduling, and result assembly
;;
;; Extracted from runtime/iteration.rkt (A-02, v0.16.1 Wave 4) for
;; single-responsibility separation. Handles the tool-call lifecycle
;; within an agent iteration:
;;   1. Extract tool calls from assistant messages
;;   2. Dispatch hooks (tool-call, tool.execution.start/end)
;;   3. Run tool batch through scheduler
;;   4. Emit events (tool.call.completed/failed)
;;   5. Create tool-result messages
;;   6. Dispatch tool-result hooks
;;
;; ── LAYER EXCEPTION (same as iteration.rkt, ARCH-01 / #341) ──
;; Upward imports into tools/ and extensions/ layers. See iteration.rkt
;; header comment for full rationale.

(require racket/contract
         racket/list
         racket/path
         json
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
         (only-in "../runtime/session-store.rkt" append-entries!)
         (only-in "../runtime/settings.rkt" make-minimal-settings setting-ref setting-ref*)
         (only-in "../util/ids.rkt" generate-id now-seconds)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?))

(provide extract-tool-calls-from-messages
         make-tool-result-messages
         handle-tool-calls-pending)

;; ============================================================
;; Helpers
;; ============================================================

;; now-seconds imported from util/ids.rkt

(define (emit-session-event! bus sid event-name payload)
  (define evt (make-event event-name (now-seconds) sid #f payload))
  (publish! bus evt)
  evt)

;; Safely dispatch hooks if extension-registry is present.
(define (maybe-dispatch-hooks ext-reg hook-point payload #:ctx [ctx #f])
  (if ext-reg
      (let ([result (dispatch-hooks hook-point payload ext-reg #:ctx ctx)])
        (values (hook-result-payload result) result))
      (values payload #f)))

;; ============================================================
;; Exported functions
;; ============================================================

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
                                   config)
  ;; Extract tool calls from assistant messages
  (define tool-calls (extract-tool-calls-from-messages new-msgs))

  ;; Find assistant message ID for tool-result parent
  (define assistant-msg-id
    (let ([asst-msgs (filter (lambda (m) (eq? (message-role m) 'assistant)) new-msgs)])
      (if (null? asst-msgs)
          #f
          (message-id (last asst-msgs)))))

  ;; Dispatch 'tool-call hook
  (define-values (tool-calls-to-run tool-call-blocked?)
    (let-values ([(amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-call tool-calls)])
      (if (and hook-res (eq? (hook-result-action hook-res) 'block))
          (values '() #t)
          (values amended #f))))

  ;; Dispatch 'tool.execution.start hook before tool batch
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

  ;; Emit tool.call.completed / tool.call.failed events
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
