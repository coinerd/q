#lang racket/base
;;
;; COMPOSITION ROOT: This module wires together dependencies from
;; lower layers. It should not be imported by other production modules.
;;

;; runtime/session-lifecycle.rkt — session prompt execution lifecycle
;; STABILITY: evolving
;;
;; Extracted from agent-session.rkt. Contains the core prompt execution
;; pipeline: context building, iteration dispatch, and the main run-prompt!
;; entry point.
;;
;; Provides:

(define-logger q-session-lifecycle)

;; Expected ownership contention is distinct from an internal session failure.
;; UI adapters use this type to avoid duplicating runtime-owned error events.
(struct exn:fail:session:busy exn:fail (session-id) #:transparent)

;;   run-prompt!             — main entry point for running a user prompt
;;   build-session-context-for-prompt — build context from history + system instructions
;;   dispatch-iteration      — model-select hook + iteration loop dispatch
;;   run-prompt-internal     — internal prompt execution (after input hook)

(require racket/contract
         (only-in "session-config.rkt"
                  config-working-set
                  config-token-budget-threshold
                  config-max-context-tokens
                  config-model-name
                  config-model-override
                  session-config?
                  session-config->hash
                  hash->session-config
                  resolve-max-iterations-hard)
         "session-mutation.rkt"
         racket/string
         racket/list
         (only-in racket/dict dict-ref dict-set)
         racket/path
         (only-in "../../util/loop-result.rkt" make-loop-result)
         (only-in "../../util/message/message.rkt" message-id message?)
         (only-in "../../util/loop-result.rkt" loop-result-termination-reason)
         (only-in "session-lifecycle-transitions.rkt"
                  build-user-message
                  compute-parent-id
                  inject-system-instructions)
         "session-context-boundary.rkt"
         "../../util/event/event-bus.rkt"
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result-payload)
         (only-in "../../util/error/errors.rkt" raise-session-error)
         "session-store.rkt"
         (only-in "../../util/exn.rkt" exn:fail:stream-error? exn:fail:stream-error-partial-messages)
         "../session-index/schema.rkt"
         "../session-index/mutations.rkt"
         "../session-index/query.rkt"
         (only-in "../../util/event/event-payloads.rkt" error-payload input-payload payload->hash)
         (only-in "../../util/telemetry.rkt" with-telemetry)
         (only-in "../working-set.rkt" compute-working-set-budget make-working-set working-set-reset!)
         "../../util/ids.rkt"
         (only-in "../runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "../../agent/iteration/main-loop.rkt" run-iteration-loop/v2)
         (only-in "../../agent/iteration/loop-config.rkt" make-loop-config)
         (only-in "../../agent/iteration/loop-state.rkt" iteration-snapshot)
         (only-in "../turn-orchestrator.rkt" run-provider-turn build-assembled-context)
         (only-in "../iteration/step-executor.rkt" interpret-step)
         (only-in "../../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../../agent/event-structs/session-events.rkt" make-context-event)
         "session-types.rkt"
         (only-in "session-controls.rkt" set-model! shutdown-requested? force-shutdown-requested?)
         (only-in "../../llm/token-budget.rkt" DEFAULT-TOKEN-BUDGET-THRESHOLD)
         (only-in "../compaction/session-compaction.rkt" maybe-compact-context)
         (only-in "../trace-logger.rkt" make-trace-logger start-trace-logger! stop-trace-logger!)
         (only-in "../auto-retry.rkt"
                  classify-error
                  retry-exhausted?
                  retry-exhausted-attempts
                  retry-exhausted-total-delay-ms
                  retry-exhausted-error-history
                  retry-exhausted-original-exn
                  retry-cancelled?
                  find-retry-exhausted)
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../context/context-pressure.rkt" check-context-pressure)
         "session-persistence.rkt"
         "session-interruption.rkt"
         (only-in "../../util/event/event.rkt" make-event)
         (only-in "session-prompt-scope.rkt" call-with-session-prompt-scope))

(provide (contract-out
          [run-prompt!
           (->* (agent-session? (or/c string? message?))
                (#:max-iterations (or/c exact-nonnegative-integer? #f)
                                  #:ensure-persisted! (or/c procedure? #f)
                                  #:buffer-or-append! (or/c procedure? #f))
                any)]
          [run-prompt-internal
           (->* (agent-session? (or/c string? message?)
                                (or/c exact-nonnegative-integer? #f)
                                (or/c exact-nonnegative-integer? #f)
                                (or/c procedure? #f)
                                (or/c procedure? #f))
                (#:prompt-turn-id (or/c string? #f))
                any)]
          [build-session-context-for-prompt
           (-> agent-session?
               (or/c string? message?)
               (or/c procedure? #f)
               (or/c procedure? #f)
               (listof message?))]
          [dispatch-iteration
           (->* (agent-session? (listof message?) exact-nonnegative-integer?)
                (#:prompt-turn-id (or/c string? #f))
                any)]
          [ensure-persisted! (-> agent-session? void?)]
          [buffer-or-append! (-> agent-session? message? void?)]
          [write-crash-log! (-> (or/c string? #f) string? string? void?)]
          [compute-parent-id (->* ((listof message?)) ((or/c session-index? #f)) (or/c string? #f))]
          [build-user-message (-> string? (or/c string? #f) message?)]
          [inject-system-instructions (-> (listof message?) (listof string?) (listof message?))])
         (struct-out exn:fail:session:busy))

;; ── Helpers ──

;; session-log-path imported from session-types.rkt
;; ensure-persisted!, buffer-or-append! from agent-session.rkt

;; ============================================================
;; Pure helpers moved to session-lifecycle-transitions.rkt (v0.74.4)

;; ── build-session-context-for-prompt ──

;; build-session-context-for-prompt — context preparation: converts user-message,
;; appends to log, builds/updates index, walks tree via context-assembly,
;; injects system instructions. Returns the context message list.
;; Wave 1 (#520): Uses session-index + context-assembly tree walk
;; instead of linear load-session-log.
(define (build-session-context-for-prompt sess user-message ensure-persisted!-fn buffer-or-append!-fn)
  (define log-path (session-log-path-for sess))
  (define idx-path
    (session-index-path (session-identity-facet-session-dir (session->identity-facet sess))))

  ;; E0: Ensure the durable index exists (build if first time).
  (unless (agent-session-index sess)
    (when (file-exists? log-path)
      (guarded-set-index! sess (build-index! log-path idx-path))))
  (define idx (agent-session-index sess))

  ;; Pre-load durable history for the pure plan (single read; equivalent to the
  ;; historical parent/linear reads in all reachable states).
  (define history
    (if (file-exists? log-path)
        (load-session-log log-path)
        '()))

  ;; E1: Reset the per-prompt working set before the pure plan reads it.
  (define ws (config-working-set (session-provider-facet-config (session->provider-facet sess))))
  (when ws
    (working-set-reset! ws))

  ;; Context-build boundary (v0.99.92 W2): an explicit request/result pair makes
  ;; the Context Assembly boundary testable without a live session. The pure
  ;; context-build computes the canonical message, post-append index, path
  ;; settings, context source, and system-injected context; the caller then
  ;; applies its effects below in the historical order.
  (define result
    (context-build (context-build-request user-message
                                          history
                                          idx
                                          (agent-session-system-instructions sess)
                                          (and (agent-session-provider sess) #t)
                                          ws
                                          DEFAULT-TOKEN-BUDGET-THRESHOLD)))

  ;; E2: Apply the canonical index append in the historical order. The pure
  ;; append leaves the shared active-leaf box untouched; setting it here
  ;; preserves alias semantics for any pre-append index reference, then the
  ;; session install and durable save keep index and log in lockstep.
  (define appended (context-build-result-appended-entry result))
  (when appended
    (define post-idx (context-build-result-post-append-index result))
    (set-box! (session-index-active-leaf-id idx) (message-id appended))
    (guarded-set-index! sess post-idx)
    (save-index! idx-path post-idx))

  ;; E3: Buffer/append the canonical user message (deferred persistence).
  (buffer-or-append!-fn sess (context-build-result-canonical-user-message result))

  ;; E4: Apply the path-derived model setting.
  ;; BUG-0018 W2 (R-B1): never clobber an explicit runtime /model override.
  ;; Previously this unconditionally overwrote the model name on EVERY prompt,
  ;; silently reverting `/model <name>` switches before the next request.
  (define model-name (context-build-result-model-name result))
  (when (and model-name
             (not (config-model-override (session-provider-facet-config
                                          (session->provider-facet sess)))))
    (guarded-set-model-name! sess model-name))

  (context-build-result-context-with-system result))

;; ── dispatch-iteration ──

;; dispatch-iteration — model-select hook + iteration loop dispatch.
;;   Runs the core agent loop with error handling. Returns a loop-result.
;;   v0.32.0: Starts trace logger for session diagnostics.
(define (dispatch-iteration sess
                            context-with-system
                            max-iterations
                            #:prompt-turn-id [prompt-turn-id #f])
  (define bus (session-tool-facet-event-bus (session->tool-facet sess)))
  (define prov (session-provider-facet-provider (session->provider-facet sess)))
  (define reg (session-tool-facet-tool-registry (session->tool-facet sess)))
  (define log-path (session-log-path-for sess))
  (define sid (session-identity-facet-session-id (session->identity-facet sess)))
  (define cfg (session-provider-facet-config (session->provider-facet sess)))
  (define cancellation-tok (dict-ref cfg 'cancellation-token #f))

  ;; BUG-0018 W2: single source of truth — the session's switched provider/model.
  ;; If the config hash's model-name diverges from the live session (e.g. a
  ;; /model switch that only reached one of the two slots), reconcile to the
  ;; session and surface it loudly instead of silently requesting on the old
  ;; provider/model pair.
  (let ([sess-model (agent-session-model-name sess)]
        [cfg-model (config-model-name cfg)])
    (when (and sess-model cfg-model (not (equal? sess-model cfg-model)))
      (log-warning "BUG-0018 W2: model divergence — session=~a config=~a; reconciling to session"
                   sess-model
                   cfg-model)
      (define bus-div (session-tool-facet-event-bus (session->tool-facet sess)))
      (when bus-div
        (publish! bus-div
                  (make-event "model.divergence.reconciled"
                              (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                              (session-identity-facet-session-id (session->identity-facet sess))
                              #f
                              (hasheq 'session-model sess-model 'config-model cfg-model))))
      ;; Reconcile the config hash to the session's switched model.
      (define cfg-hash
        (if (session-config? cfg)
            (session-config->hash cfg)
            cfg))
      (define reconciled (hash-set cfg-hash 'model-name sess-model))
      (guarded-set-config! sess
                           (if (session-config? cfg)
                               (hash->session-config reconciled)
                               reconciled))))

  ;; Dispatch 'model-select hook — extensions can override model
  (define-values (_model-hook-res model-hook-res)
    (maybe-dispatch-hooks (agent-session-extension-registry sess)
                          'model-select
                          (hasheq 'current-model (or (agent-session-model-name sess) "default"))))
  (when (and model-hook-res
             (eq? (hook-result-action model-hook-res) 'amend)
             (hash? (hook-result-payload model-hook-res))
             (hash-has-key? (hook-result-payload model-hook-res) 'model))
    (define override-model (hash-ref (hook-result-payload model-hook-res) 'model))
    (guarded-set-model-name! sess override-model))

  ;; Run the core agent loop with tool-call iteration
  ;; v0.32.0: Start trace logger for diagnostics
  (define session-dir (agent-session-session-dir sess))
  (define tracer (make-trace-logger bus session-dir #:enabled? #t))
  (start-trace-logger! tracer)
  (with-handlers ([retry-cancelled?
                   ;; W0-F5: a cancellation that aborted retry backoff terminates
                   ;; the prompt as cancelled, never as a provider error.
                   (lambda (e)
                     (stop-trace-logger! tracer)
                     (make-loop-result context-with-system
                                       'cancelled
                                       (hasheq 'reason "cancellation-token" 'retry-aborted #t)))]
                  [exn:fail?
                   (lambda (e)
                     ;; Flush partial messages from stream errors to session.jsonl.
                     ;; Recovery data travels explicitly via exn:fail:stream-error,
                     ;; replacing the former current-loop-state-for-error-recovery parameter
                     ;; (which was dead code — parameterize unwinds before handlers fire).
                     (define partial-msgs
                       (let loop ([ex e])
                         (cond
                           [(exn:fail:stream-error? ex) (exn:fail:stream-error-partial-messages ex)]
                           [(retry-exhausted? ex) (loop (retry-exhausted-original-exn ex))]
                           [else '()])))
                     (when (pair? partial-msgs)
                       (append-session-entries! sess partial-msgs))
                     ;; Emit runtime.error event with classified error-type
                     (define error-type (classify-error e))
                     ;; A3: Include retry metadata if retries were attempted.
                     ;; W0-F5: deep-unwrap so partial recovery wrapping cannot hide
                     ;; the retry-exhausted metadata inside exn:fail:stream-error.
                     (define retry-info (find-retry-exhausted e))
                     (define base-payload (error-payload (exn-message e) error-type))
                     (define payload
                       (if retry-info
                           (hash-set* (payload->hash base-payload)
                                      'retries-attempted
                                      (retry-exhausted-attempts retry-info)
                                      'total-retry-delay-ms
                                      (retry-exhausted-total-delay-ms retry-info)
                                      'errorHistory
                                      (retry-exhausted-error-history retry-info))
                           (payload->hash base-payload)))
                     (emit-session-event! bus sid "runtime.error" payload #:turn-id prompt-turn-id)
                     ;; The outer prompt lifecycle owns the canonical prompt terminal.
                     ;; Inner dispatch reports only runtime.error and its loop result.
                     ;; v0.32.0: Stop trace logger on error (flush before return)
                     (stop-trace-logger! tracer)
                     (make-loop-result context-with-system 'error payload))])
    (define ws (config-working-set cfg))
    ;; v0.99.87: Bind Runtime configuration into injected closures.
    ;; The Agent iteration loop no longer receives or imports session-config.
    ;; These closures capture cfg at the composition root and expose only
    ;; the semantic Agent inputs (working-set, context, session-id, etc.).
    (define max-iter-hard (resolve-max-iterations-hard cfg max-iterations))
    (define ctx-budget (or (config-token-budget-threshold cfg) (config-max-context-tokens cfg)))
    (define result
      (run-iteration-loop/v2
       (make-loop-config
        context-with-system
        prov
        bus
        reg
        (agent-session-extension-registry sess)
        log-path
        sid
        max-iterations
        #:cancellation-token cancellation-tok
        #:max-iterations-hard max-iter-hard
        #:context-budget ctx-budget
        #:working-set ws
        #:queue (agent-session-queue sess)
        #:shutdown-check (lambda () (agent-session-shutdown-requested? sess))
        #:force-shutdown-check (lambda () (agent-session-force-shutdown? sess))
        #:session sess
        ;; Injected runtime operations — closures capture cfg
        ;; so Agent iteration never imports session-config.
        #:build-context-fn
        (lambda (ctx-to-use ws-arg ext-reg-arg bus-arg sid-arg iter #:session sess-arg)
          (build-assembled-context ctx-to-use
                                   (dict-set cfg 'working-set ws-arg)
                                   ext-reg-arg
                                   bus-arg
                                   sid-arg
                                   iter
                                   #:session sess-arg))
        #:run-provider-turn-fn
        (lambda (ctx-final prov-arg bus-arg reg-arg ext-reg-arg sid-arg tid-arg tok-arg)
          (run-provider-turn ctx-final
                             prov-arg
                             bus-arg
                             reg-arg
                             ext-reg-arg
                             sid-arg
                             tid-arg
                             tok-arg
                             cfg))
        #:interpret-step-fn (lambda (step-res step-result new-msgs infra snapshot)
                              (interpret-step step-res
                                              step-result
                                              new-msgs
                                              infra
                                              (struct-copy iteration-snapshot snapshot [config cfg])))
        #:ensure-working-set-fn
        (lambda (ws-arg)
          (or ws-arg (make-working-set #:max-tokens (compute-working-set-budget ctx-budget)))))))
    ;; v0.32.0: Stop trace logger on normal completion
    (stop-trace-logger! tracer)
    result))

;; ── run-prompt-internal ──

;; Internal prompt execution, extracted for input hook gating.
(define (run-prompt-internal sess
                             user-message
                             max-iterations
                             token-budget-threshold
                             ensure-persisted!-fn
                             buffer-or-append!-fn
                             #:prompt-turn-id [prompt-turn-id #f])
  (define bus (agent-session-event-bus sess))
  (define log-path (session-log-path-for sess))
  (define idx-path (session-index-path (agent-session-session-dir sess)))
  (define sid (agent-session-session-id sess))
  ;; Create a context-relative working set and attach it to session config.
  (define base-cfg (session-provider-facet-config (session->provider-facet sess)))
  (define context-budget
    (or token-budget-threshold
        (dict-ref base-cfg 'token-budget-threshold #f)
        (dict-ref base-cfg 'max-context-tokens 128000)))
  (define ws (make-working-set #:max-tokens (compute-working-set-budget context-budget)))
  (guarded-set-config!
   sess
   (dict-set (dict-set base-cfg 'working-set ws) 'session-index (agent-session-index sess)))

  ;; 1. Build context: convert message, append to log, load history, inject system instructions
  (define context-with-system
    (build-session-context-for-prompt sess user-message ensure-persisted!-fn buffer-or-append!-fn))

  ;; 2. Check token budget and compact if needed
  (define context-after-compact
    (maybe-compact-context sess context-with-system token-budget-threshold))

  ;; 2a. Emit context-pressure event for TUI/extensions
  (define token-count (estimate-context-tokens context-after-compact))
  (check-context-pressure sess token-count token-budget-threshold)

  ;; 2b. v0.83.10: Emit context.built so TUI status bar shows token count
  (emit-typed-event! bus
                     (make-context-event #:session-id sid
                                         #:turn-id #f
                                         #:timestamp (current-inexact-milliseconds)
                                         #:token-count token-count
                                         #:window-size (length context-after-compact)))

  ;; 3. Ensure session directory exists before iteration writes assistant messages
  (ensure-persisted!-fn sess)

  ;; 4. Run the core agent loop (model-select hook + iteration dispatch)
  (define final-result
    (with-telemetry
     "dispatch-iteration"
     (dispatch-iteration sess context-after-compact max-iterations #:prompt-turn-id prompt-turn-id)))

  ;; 5. Rebuild index
  (guarded-set-index! sess (build-index! log-path idx-path))

  ;; 6. Emit session.updated
  (emit-session-event!
   bus
   sid
   "session.updated"
   (hasheq 'sessionId sid 'lastTurnTermination (loop-result-termination-reason final-result)))

  (values sess final-result))

;; ── run-prompt! ──

;;; run-prompt! : agent-session? (or/c string? message?)
;;;              [#:max-iterations (or/c integer? #f)]
;;;              -> (values agent-session? loop-result?)
;;;
;;; Main entry point for running a user prompt. Guards against closed
;;; sessions, dispatches 'input hook (extensions can block/amend input),
;;; builds context from history + system instructions, checks token budget
;;; and compacts if needed, ensures persistence, dispatches 'model-select
;;; hook and runs the iteration loop, rebuilds session index, emits
;;; session.updated. Returns updated session and loop-result.
(define (run-prompt! sess
                     user-message
                     #:max-iterations [max-iter-override #f]
                     #:ensure-persisted! [ensure-persisted!-fn #f]
                     #:buffer-or-append! [buffer-or-append!-fn #f])
  (define ep! (or ensure-persisted!-fn ensure-persisted!))
  (define ba! (or buffer-or-append!-fn buffer-or-append!))
  ;; B4: Guard — refuse operations on closed sessions
  (unless (agent-session-active? sess)
    (raise-session-error (format "run-prompt!: session ~a is closed" (agent-session-session-id sess))
                         (agent-session-session-id sess)))
  (define bus (session-tool-facet-event-bus (session->tool-facet sess)))
  (define sid (session-identity-facet-session-id (session->identity-facet sess)))
  ;; Stable outer prompt identity for the single canonical prompt terminal.
  (define active-prompt-turn-id (box #f))
  (define termination-reason (box 'error))
  ;; Atomically exclude both another prompt and manual/automatic compaction.
  ;; Acquisition belongs in the before-thunk: a denied contender must not run
  ;; cleanup, while every failure after a successful claim must release it.
  (dynamic-wind
   (lambda ()
     (unless (try-claim-prompt! sess)
       (define reason
         (if (agent-session-compacting? sess)
             "Session compaction is active — retry the prompt after it completes"
             (format "Prompt already running — session ~a is processing. Use /interrupt to cancel."
                     sid)))
       (emit-session-event! bus sid "runtime.error" (hasheq 'error reason))
       (raise
        (exn:fail:session:busy (format "run-prompt!: ~a" reason) (current-continuation-marks) sid))))
   (lambda ()
     ;; Bind one fresh prompt-turn identity to this prompt's cancellation token.
     ;; Inner model iterations have their own IDs, but user interruption targets
     ;; this stable outer turn.
     (define active-turn-id (begin-session-turn! sess))
     (set-box! active-prompt-turn-id active-turn-id)
     ;; F2: Emit turn.started immediately so TUI shows activity before context
     ;; build + compaction. Inner turn.started events remain idempotent.
     (publish! bus
               (make-event "turn.started"
                           (current-inexact-milliseconds)
                           sid
                           active-turn-id
                           (hasheq 'scope "prompt")))
     ;; begin-session-turn! may install a token when this session had none.
     (define base-cfg (session-provider-facet-config (session->provider-facet sess)))
     ;; #1391: Inject session index into config for session_recall tool access
     (define idx (agent-session-index sess))
     ;; v0.14.3: Handle both mutable (make-hash) and immutable (hasheq) configs.
     (define cfg
       (if idx
           (dict-set base-cfg 'session-index idx)
           base-cfg))
     (define max-iterations (or max-iter-override (dict-ref cfg 'max-iterations 50)))
     (define token-budget-threshold
       (dict-ref cfg 'token-budget-threshold DEFAULT-TOKEN-BUDGET-THRESHOLD))

     ;; #666: Dispatch 'input hook — intercept/transform user input before processing
     (define ext-reg (agent-session-extension-registry sess))
     (define-values (_processed-input input-hook-res)
       (maybe-dispatch-hooks ext-reg 'input (input-payload sid user-message)))
     (cond
       [(and input-hook-res (eq? (hook-result-action input-hook-res) 'block))
        ;; Input blocked by extension
        (set-box! termination-reason 'completed)
        (emit-session-event! bus sid "input.blocked" (hasheq 'reason "extension-block"))
        (values sess (make-loop-result '() 'completed (hasheq 'reason "input-blocked")))]
       [else
        (define effective-input
          (if (and input-hook-res (eq? (hook-result-action input-hook-res) 'amend))
              (hash-ref (hook-result-payload input-hook-res) 'message user-message)
              user-message))
        ;; NR-2 (v0.99.82): Persist last user prompt for /retry recovery.
        ;; This catches ALL submission paths (TUI, goal-runner, SDK) because
        ;; every prompt goes through run-prompt!.
        (when (string? effective-input)
          (guarded-set-config!
           sess
           (dict-set (agent-session-config sess) 'last-user-prompt effective-input)))
        (call-with-session-prompt-scope
         sess
         (lambda ()
           (call-with-values (lambda ()
                               (run-prompt-internal sess
                                                    effective-input
                                                    max-iterations
                                                    token-budget-threshold
                                                    ep!
                                                    ba!
                                                    #:prompt-turn-id active-turn-id))
                             (lambda (updated-session result)
                               (set-box! termination-reason (loop-result-termination-reason result))
                               (values updated-session result)))))]))
   ;; Cleanup owns the single outer prompt terminal. Publish it while prompt
   ;; ownership is still held so a later prompt cannot interleave, and release
   ;; ownership from the guaranteed after-thunk even if finish/tracing/publish fails.
   (lambda ()
     (dynamic-wind void
                   (lambda ()
                     ;; A finish failure must not suppress the canonical terminal:
                     ;; degrade the reason, publish while ownership is held, then
                     ;; re-raise the original finalization error after release.
                     (define finish-failure (box #f))
                     (define-values (_finished-turn-id interrupt-request-id)
                       (with-handlers ([exn:fail? (lambda (e)
                                                    (set-box! finish-failure e)
                                                    (values #f #f))])
                         (finish-session-turn! sess)))
                     (define prompt-turn-id (unbox active-prompt-turn-id))
                     (when prompt-turn-id
                       (define terminal-tracer
                         (make-trace-logger bus (agent-session-session-dir sess) #:enabled? #t))
                       (define base-payload
                         (hasheq 'scope
                                 "prompt"
                                 'reason
                                 (if (unbox finish-failure)
                                     "error"
                                     (or (symbol->string (unbox termination-reason)) "error"))
                                 'duration-ms
                                 0))
                       (define payload
                         (if interrupt-request-id
                             (hash-set* base-payload
                                        'request-id
                                        interrupt-request-id
                                        'target-session-id
                                        sid
                                        'target-turn-id
                                        prompt-turn-id)
                             base-payload))
                       (dynamic-wind (lambda () (start-trace-logger! terminal-tracer))
                                     (lambda ()
                                       (publish! bus
                                                 (make-event "turn.completed"
                                                             (current-inexact-milliseconds)
                                                             sid
                                                             prompt-turn-id
                                                             payload)))
                                     (lambda () (stop-trace-logger! terminal-tracer))))
                     (when (unbox finish-failure)
                       (raise (unbox finish-failure))))
                   (lambda () (release-prompt! sess)))
     ;; B3-A: Emergency persist — defense-in-depth if session not yet persisted
     (unless (agent-session-persisted? sess)
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-warning "session-lifecycle: emergency persist failed: ~a"
                                                 (exn-message e)))])
         (ensure-persisted! sess))))))
