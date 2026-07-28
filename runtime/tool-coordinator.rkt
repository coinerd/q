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
         racket/dict
         racket/list
         racket/path
         json
         (only-in "session/session-config.rkt"
                  session-config?
                  hash->session-config
                  config-settings
                  config-provider
                  config-model-name
                  config-session-index
                  config-parallel-tools
                  config-permission-config
                  config-project-dir)
         (only-in "../util/tool/tool-types.rkt" tool-call?)
         (only-in "layer-adapters.rkt" tool-result? tool-registry?)
         (only-in "../util/json/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/content/content-parts.rkt" make-tool-result-part tool-call-part?)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../util/message/message.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message)
         (only-in "../util/tool/tool-types.rkt"
                  make-tool-call
                  tool-call-id
                  tool-call-name
                  tool-call-arguments
                  tool-result-content)
         (only-in "../util/content/content-parts.rkt"
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments)
         (only-in "../util/tool/tool-types.rkt" tool-result-is-error?)
         "../util/event/event-bus.rkt"
         ;; ARCH-01 upward import — runtime→tools
         (only-in "layer-adapters.rkt" make-exec-context make-error-result list-tools-jsexpr)
         ;; Permission gate via adapter
         (only-in "layer-adapters.rkt" permission-config? make-default-permission-config)
         ;; ARCH-01 upward import via adapter
         (only-in "layer-adapters.rkt" run-tool-batch scheduler-result scheduler-result-results)
         ;; Hooks + extension registry via adapter
         (only-in "layer-adapters.rkt" dispatch-hooks extension-registry?)
         (only-in "../util/event/event-bus.rkt" event-bus?)
         (only-in "session/session-store.rkt" append-entries!)
         (only-in "../runtime/settings.rkt"
                  q-settings
                  q-settings?
                  q-settings-global
                  q-settings-project
                  q-settings-merged)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "../util/content/content-helpers.rkt" tool-result-content->string)
         (only-in "../util/ids.rkt" generate-id now-seconds)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../util/capability.rkt" current-session-capabilities)
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         ;; QUAL-01 (v0.22.0): shared runtime helpers
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         ;; G13: typed event emission for tool execution
         (only-in "../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../agent/event-structs/tool-events.rkt"
                  make-tool-execution-start-event
                  make-tool-execution-end-event
                  make-tool-execution-update-event)
         ;; P2: outcome classification for trace emissions
         (only-in "../util/outcome/outcome-types.rkt"
                  classify-tool-outcome
                  typed-tool-outcome-kind
                  typed-tool-outcome-status
                  typed-tool-outcome-payload))

(provide (contract-out [extract-tool-calls-from-messages (-> (listof message?) (listof tool-call?))]
                       [make-tool-result-messages
                        (-> (listof tool-call?) (listof tool-result?) string? (listof message?))]
                       [handle-tool-calls-pending
                        (->* ((listof message?) ; new-msgs
                              list? ; ctx-with-steering
                              (or/c extension-registry? #f) ; ext-reg
                              (or/c tool-registry? #f) ; reg
                              event-bus? ; bus
                              string? ; session-id
                              (or/c path-string? path?) ; log-path
                              (or/c cancellation-token? #f) ; token
                              session-config?) ; config
                             (#:permission-config (or/c permission-config? #f))
                             (listof message?))]
                       [handle-tool-calls-pending/outcome
                        (->* ((listof message?) list?
                                                (or/c extension-registry? #f)
                                                (or/c tool-registry? #f)
                                                event-bus?
                                                string?
                                                (or/c path-string? path?)
                                                (or/c cancellation-token? #f)
                                                session-config?)
                             (#:permission-config (or/c permission-config? #f))
                             tool-batch-outcome?)]))
(provide (struct-out tool-batch-outcome))
;; Pure helpers (W2 #4192)
(provide classify-tool-results
         build-blocked-tool-results
         permission-config-for-execution
         capabilities-for-tool-execution)

;; v0.31.5 W1: export struct
(provide tool-call-actions
         tool-call-actions?
         tool-call-actions-calls-to-run
         tool-call-actions-blocked?
         tool-call-actions-final-calls)

;; ============================================================
;; Tool runtime settings
;; ============================================================

(define (settings-for-tool-execution config)
  (define settings (config-settings config))
  (define provider (config-provider config))
  (define model (config-model-name config))
  (define merged
    (cond
      [(q-settings? settings) (q-settings-merged settings)]
      [(hash? settings) settings]
      [else (hash)]))
  (define with-provider
    (if (provider? provider)
        (hash-set merged 'provider provider)
        merged))
  (define with-model
    (if model
        (hash-set (hash-set with-provider 'model model) 'default-model model)
        with-provider))
  (if (q-settings? settings)
      (q-settings (q-settings-global settings) (q-settings-project settings) with-model)
      with-model))

;; Select the permission config used by normal agent tool execution.
;; A valid explicit per-call config takes precedence over the session-resolved
;; config; the final fallback remains deny-by-default.
(define (permission-config-for-execution config explicit)
  (or explicit (config-permission-config config) (make-default-permission-config)))

;; Resolve capability authority at the runtime boundary. A configured session
;; value wins; otherwise capture the current session parameter. make-exec-context
;; canonicalizes this value into an immutable fail-closed snapshot.
(define (capabilities-for-tool-execution config)
  (if (dict-has-key? config 'capabilities)
      (dict-ref config 'capabilities)
      (current-session-capabilities)))

;; ============================================================
;; Helpers (QUAL-01: emit-session-event! and maybe-dispatch-hooks
;;           now imported from runtime-helpers.rkt)
;; ============================================================

;; now-seconds imported from util/ids.rkt

;; ============================================================
;; Exported functions
;; ============================================================

;; The structured result keeps the complete next-iteration context separate
;; from just the calls/results produced by this batch.  This prevents callers
;; from mistaking historical tool messages in the context for current results.
(struct tool-batch-outcome (updated-context effective-current-calls current-result-messages)
  #:transparent)

;; v0.31.5 W1: Pure function for tool-call actions
(struct tool-call-actions (calls-to-run blocked? final-calls))

;; classify-tool-results : (listof tool-call?) (listof tool-result?) -> (listof hasheq?)
;; Pure: maps tool calls + results to status classification hashes.
(define (classify-tool-results tool-calls results)
  (for/list ([tc (in-list tool-calls)]
             [tr (in-list results)])
    (hasheq 'name (tool-call-name tc) 'status (if (tool-result-is-error? tr) 'error 'completed))))

;; build-blocked-tool-results : (listof tool-call?) -> (listof tool-result?)
;; Pure: creates error results for every tool call when execution is blocked.
(define (build-blocked-tool-results tool-calls)
  (for/list ([tc (in-list tool-calls)])
    (make-error-result (format "Tool call '~a' blocked by extension" (tool-call-name tc)))))

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

;; True for read results whose contents are durable planning artifacts.
;; planning-read addresses .planning indirectly by artifact name, while the
;; ordinary read tool must name a path within a .planning directory.
(define (planning-read-call? tc)
  (define name (tool-call-name tc))
  (define args (tool-call-arguments tc))
  (define raw-path (and (hash? args) (or (hash-ref args 'path #f) (hash-ref args "path" #f))))
  (define path-text
    (cond
      [(path? raw-path) (path->string raw-path)]
      [(string? raw-path) raw-path]
      [else #f]))
  (or (equal? name "planning-read")
      (and (equal? name "read")
           path-text
           (regexp-match? #px"(^|[/\\\\])\\.planning([/\\\\]|$)" path-text))))

;; Create tool-result messages from scheduler results.
(define (make-tool-result-messages tool-calls results parent-msg-id)
  (for/list ([tc (in-list tool-calls)]
             [tr (in-list results)])
    (define msg-id (generate-id))
    (define error? (tool-result-is-error? tr))
    (define base-meta (hasheq 'toolCallId (tool-call-id tc) 'isError error?))
    (define meta
      (if (and (not error?) (planning-read-call? tc))
          (hash-set* base-meta 'gsd-pin #t 'gsd-pin-reason "successful planning artifact read")
          base-meta))
    (make-message msg-id
                  parent-msg-id
                  'tool
                  'tool-result
                  (list (make-tool-result-part (tool-call-id tc) (tool-result-content tr) error?))
                  (now-seconds)
                  meta)))

;; Handle tool-calls-pending: extract calls, run through scheduler, emit events,
;; and return updated context for the next loop iteration.
;; Phase 1: Preparation + Hook dispatch (EFFECTFUL — calls extension hooks)
(define (prepare-tool-execution-phase new-msgs ext-reg)
  (define tool-calls (extract-tool-calls-from-messages new-msgs))
  (define assistant-msg-id
    (let ([asst-msgs (filter (lambda (m) (eq? (message-role m) 'assistant)) new-msgs)])
      (if (null? asst-msgs)
          #f
          (message-id (last asst-msgs)))))
  (define-values (amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-call tool-calls))
  (define actions (compute-tool-call-actions tool-calls amended hook-res))
  (values tool-calls assistant-msg-id actions))

;; Phase 2: Execution + Event emission (EFFECTFUL — scheduler + events)
(define (execute-tool-batch-phase tool-calls-to-run
                                  reg
                                  ext-reg
                                  bus
                                  session-id
                                  log-path
                                  token
                                  config
                                  per-tool-start-ms
                                  batch-start-ms
                                  perm-cfg)
  ;; Dispatch 'tool.execution.started hook before tool batch
  (when (and ext-reg (not (null? tool-calls-to-run)))
    (maybe-dispatch-hooks
     ext-reg
     'tool.execution.started
     (hasheq 'tools (map tool-call-name tool-calls-to-run) 'count (length tool-calls-to-run))))
  ;; B3 fix: Emit tool.execution.updated event so TUI shows progress
  ;; during long-running tool batches (e.g. bash commands with 120s timeouts).
  (when (not (null? tool-calls-to-run))
    (emit-typed-event!
     bus
     (make-tool-execution-update-event
      #:session-id session-id
      #:turn-id #f
      #:timestamp (current-inexact-milliseconds)
      #:tool-name (format "~a tools starting" (length tool-calls-to-run))
      #:progress (hasheq 'total (length tool-calls-to-run) 'running (length tool-calls-to-run)))))
  (define sched-result
    (cond
      [(not reg) (scheduler-result '() (hasheq))]
      [else
       ;; The coordinator already dispatched the batch-level 'tool-call hook in
       ;; preparation. Suppress the scheduler's per-call duplicate while still
       ;; forwarding execution lifecycle hooks.
       (let ([hook-dispatcher-fn (and ext-reg
                                      (lambda (hook-point payload)
                                        (and (not (eq? hook-point 'tool-call))
                                             (dispatch-hooks hook-point payload ext-reg))))])
         (run-tool-batch tool-calls-to-run
                         reg
                         #:hook-dispatcher hook-dispatcher-fn
                         #:exec-context
                         (make-exec-context
                          #:working-directory
                          (or (config-project-dir config) (current-directory) (path-only log-path))
                          #:cancellation-token token
                          #:event-publisher
                          (lambda (event-type payload)
                            (cond
                              [(equal? event-type "tool.execution.started")
                               (define tcid (hash-ref payload 'tool-call-id #f))
                               (define start-ms
                                 (hash-ref payload 'start-ms (current-inexact-milliseconds)))
                               (when tcid
                                 (hash-set! per-tool-start-ms tcid start-ms))
                               (emit-typed-event! bus
                                                  (make-tool-execution-start-event
                                                   #:session-id session-id
                                                   #:turn-id #f
                                                   #:timestamp (current-inexact-milliseconds)
                                                   #:tool-name (hash-ref payload 'tool-name "unknown")
                                                   #:tool-call-id (or tcid "unknown")))]
                              [else (emit-session-event! bus session-id event-type payload)]))
                          #:runtime-settings (settings-for-tool-execution config)
                          #:call-id (generate-id)
                          #:session-metadata
                          (hasheq 'session-id session-id 'session-index (config-session-index config))
                          #:permission-config (permission-config-for-execution config perm-cfg)
                          #:capabilities (capabilities-for-tool-execution config))
                         #:parallel? (config-parallel-tools config)))]))
  ;; Dispatch 'tool.execution.completed hook after tool batch
  (when (and ext-reg (not (null? tool-calls-to-run)))
    (maybe-dispatch-hooks ext-reg
                          'tool.execution.completed
                          (hasheq 'tools
                                  (classify-tool-results tool-calls-to-run
                                                         (scheduler-result-results sched-result))
                                  'count
                                  (length tool-calls-to-run))))
  ;; G13: Emit typed tool-execution-end events.
  ;; v0.99.50 W3: Also emit a correlated protocol record because the v1 typed
  ;; end-event schema predates tool-call identity and cannot be changed without
  ;; breaking its public/red-module contract.
  (for ([tc (in-list tool-calls-to-run)]
        [tr (in-list (scheduler-result-results sched-result))])
    (define duration-ms
      (inexact->exact (floor (- (current-inexact-milliseconds)
                                (hash-ref per-tool-start-ms (tool-call-id tc) batch-start-ms)))))
    (define result-summary (if (tool-result-is-error? tr) 'error 'completed))
    (emit-typed-event! bus
                       (make-tool-execution-end-event
                        #:session-id session-id
                        #:turn-id #f
                        #:timestamp (current-inexact-milliseconds)
                        #:tool-name (tool-call-name tc)
                        #:duration-ms duration-ms
                        #:result-summary result-summary
                        #:result-error (and (tool-result-is-error? tr) (tool-result-error-text tr))))
    (emit-session-event! bus
                         session-id
                         "tool.execution.correlated-completed"
                         (hasheq 'tool-name
                                 (tool-call-name tc)
                                 'tool-call-id
                                 (tool-call-id tc)
                                 'duration-ms
                                 duration-ms
                                 'result-present?
                                 #t
                                 'result-summary
                                 result-summary)))
  sched-result)

;; Helper: extract error text from tool-result content for event propagation.
;; Bug fix (v0.97.x): ALL tool failures showed "tool failed" because
;; tool-execution-end-event had no error text field.
(define (tool-result-error-text tr)
  (define content (tool-result-content tr))
  (cond
    [(string? content) content]
    [(pair? content)
     (define first-part (car content))
     (if (hash? first-part)
         (or (hash-ref first-part 'text #f) (format "~a" content))
         (format "~a" content))]
    [else (format "~a" content)]))

;; Phase 3: Assembly (pure result construction)
(define (assemble-tool-results-phase tool-calls
                                     tool-calls-to-run
                                     sched-result
                                     assistant-msg-id
                                     tool-call-blocked?
                                     ext-reg)
  (define tool-result-msgs
    (if tool-call-blocked?
        (make-tool-result-messages tool-calls
                                   (build-blocked-tool-results tool-calls)
                                   assistant-msg-id)
        (make-tool-result-messages tool-calls-to-run
                                   (scheduler-result-results sched-result)
                                   assistant-msg-id)))
  ;; Dispatch 'tool-result hook
  (define tool-result-msgs-amended
    (let-values ([(amended hook-res) (maybe-dispatch-hooks ext-reg 'tool-result tool-result-msgs)])
      (if (and hook-res (eq? (hook-result-action hook-res) 'block)) tool-result-msgs amended)))
  ;; F1: Validate -- filter to only message? values
  (filter message? tool-result-msgs-amended))

;; Orchestrator: ties the three phases together.
(define (handle-tool-calls-pending/outcome new-msgs
                                           ctx-with-steering
                                           ext-reg
                                           reg
                                           bus
                                           session-id
                                           log-path
                                           token
                                           config-raw
                                           #:permission-config [perm-cfg #f])
  (define config
    (if (session-config? config-raw)
        config-raw
        (hash->session-config config-raw)))
  ;; Phase 1: Preparation
  (define-values (tool-calls assistant-msg-id actions)
    (prepare-tool-execution-phase new-msgs ext-reg))
  (define tool-calls-to-run (tool-call-actions-calls-to-run actions))
  (define tool-call-blocked? (tool-call-actions-blocked? actions))
  ;; Phase 2: Execution
  (define batch-start-ms (current-inexact-milliseconds))
  (define per-tool-start-ms (make-hash))
  (define sched-result
    (if tool-call-blocked?
        (scheduler-result '() (hasheq))
        (execute-tool-batch-phase tool-calls-to-run
                                  reg
                                  ext-reg
                                  bus
                                  session-id
                                  log-path
                                  token
                                  config
                                  per-tool-start-ms
                                  batch-start-ms
                                  perm-cfg)))
  ;; Phase 3: Assembly
  (define validated-msgs
    (assemble-tool-results-phase tool-calls
                                 tool-calls-to-run
                                 sched-result
                                 assistant-msg-id
                                 tool-call-blocked?
                                 ext-reg))
  ;; P2: Emit outcome-capture trace for classified tool outcomes
  (unless tool-call-blocked?
    (for ([tc (in-list tool-calls-to-run)]
          [tr (in-list (scheduler-result-results sched-result))])
      (define tool-outcome (classify-tool-outcome tc tr))
      (when tool-outcome
        (emit-session-event! bus
                             session-id
                             "outcome.captured"
                             (hasheq 'tool-name
                                     (tool-call-name tc)
                                     'outcome-kind
                                     (typed-tool-outcome-kind tool-outcome)
                                     'status
                                     (typed-tool-outcome-status tool-outcome)
                                     'has-payload
                                     (positive? (hash-count (typed-tool-outcome-payload
                                                             tool-outcome))))))))
  ;; Append validated tool results to log.  Persistence remains owned here;
  ;; the compatibility wrapper below only projects the outcome value.
  (append-entries! log-path validated-msgs)
  (define effective-current-calls
    (if tool-call-blocked?
        (tool-call-actions-final-calls actions)
        tool-calls-to-run))
  (tool-batch-outcome (append ctx-with-steering new-msgs validated-msgs)
                      effective-current-calls
                      validated-msgs))

;; Backward-compatible API: preserve the historical list return value without
;; executing, persisting, or emitting anything a second time.
(define (handle-tool-calls-pending new-msgs
                                   ctx-with-steering
                                   ext-reg
                                   reg
                                   bus
                                   session-id
                                   log-path
                                   token
                                   config-raw
                                   #:permission-config [perm-cfg #f])
  (tool-batch-outcome-updated-context (handle-tool-calls-pending/outcome new-msgs
                                                                         ctx-with-steering
                                                                         ext-reg
                                                                         reg
                                                                         bus
                                                                         session-id
                                                                         log-path
                                                                         token
                                                                         config-raw
                                                                         #:permission-config
                                                                         perm-cfg)))
;; v0.31.5 W1: placeholder for pure function extraction
