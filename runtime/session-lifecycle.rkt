#lang racket/base

;; runtime/session-lifecycle.rkt — session prompt execution lifecycle
;; STABILITY: evolving
;;
;; Extracted from agent-session.rkt. Contains the core prompt execution
;; pipeline: context building, iteration dispatch, and the main run-prompt!
;; entry point.
;;
;; Provides:
;;   run-prompt!             — main entry point for running a user prompt
;;   build-session-context   — build context from history + system instructions
;;   dispatch-iteration      — model-select hook + iteration loop dispatch
;;   run-prompt-internal     — internal prompt execution (after input hook)

(require racket/string
         racket/file
         racket/list
         racket/path
         (only-in "../util/protocol-types.rkt"
                  message-id
                  message-role
                  message-content
                  message-kind
                  message-meta
                  message-parent-id
                  make-message
                  make-text-part
                  content-part->jsexpr
                  loop-result-termination-reason
                  make-loop-result)
         "../agent/event-bus.rkt"
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload)
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         (only-in "../extensions/message-inject.rkt" injection-event-topic)
         (only-in "../util/event-payloads.rkt" error-payload input-payload payload->hash)
         (only-in "../util/error-helpers.rkt" with-telemetry)
         (only-in "../runtime/context-assembly.rkt"
                  build-assembled-context
                  context-assembly-config?
                  make-context-assembly-config
                  context-result-messages
                  context-result-catalog
                  context-result?
                  catalog-entry
                  catalog-entry?
                  build-session-context)
         (only-in "../runtime/working-set.rkt" make-working-set working-set-reset!)
         (only-in "../runtime/session-context.rkt" extract-path-settings)
         "../util/ids.rkt"
         (only-in "iteration.rkt"
                  run-iteration-loop
                  emit-session-event!
                  maybe-dispatch-hooks
                  current-compact-proc
                  current-estimate-tokens
                  current-inject-topic)
         "session-types.rkt"
         (only-in "session-controls.rkt" set-model! shutdown-requested? force-shutdown-requested?)
         (only-in "../llm/token-budget.rkt" DEFAULT-TOKEN-BUDGET-THRESHOLD)
         (only-in "session-compaction.rkt" maybe-compact-context)
         (only-in "auto-retry.rkt"
                  classify-error
                  retry-exhausted?
                  retry-exhausted-attempts
                  retry-exhausted-total-delay-ms
                  retry-exhausted-error-history))

(provide run-prompt!
         run-prompt-internal
         build-session-context
         dispatch-iteration
         ensure-persisted!
         buffer-or-append!
         write-crash-log!)

;; ============================================================
;; Helpers
;; ============================================================

;; session-log-path imported from session-types.rkt
;; ensure-persisted!, buffer-or-append! from agent-session.rkt

(define (session-index-path dir)
  (build-path dir "session.index"))

;; ============================================================
;; build-session-context
;; ============================================================

;; build-session-context — context preparation:
;;   converts user-message, appends to log, builds/updates index,
;;   walks tree via context-assembly, injects system instructions.
;;   Returns the context message list.
;;
;;   Wave 1 (#520): Uses session-index + context-assembly tree walk
;;   instead of linear load-session-log.
(define (build-session-context sess user-message ensure-persisted!-fn buffer-or-append!-fn)
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (define idx-path (session-index-path (agent-session-session-dir sess)))

  ;; Ensure index exists (build if first time)
  (unless (agent-session-index sess)
    (when (file-exists? log-path)
      (set-agent-session-index! sess (build-index! log-path idx-path))))
  (define idx (agent-session-index sess))

  ;; Convert string to message struct if needed
  (define user-msg
    (if (string? user-message)
        (let ()
          ;; Determine parent from active leaf in index (#521: use stored IDs)
          (define parent-id
            (if idx
                (let ([leaf (active-leaf idx)])
                  (cond
                    [(not leaf) #f]
                    ;; Skip session-info entries for parent calculation
                    [(eq? (message-kind leaf) 'session-info) #f]
                    [else (message-id leaf)]))
                ;; No index yet — determine from log
                (let* ([raw-existing (if (file-exists? log-path)
                                         (load-session-log log-path)
                                         '())]
                       [existing (filter (lambda (m) (not (eq? (message-kind m) 'session-info)))
                                         raw-existing)])
                  (if (null? existing)
                      #f
                      (message-id (last existing))))))
          (make-message (generate-id)
                        parent-id
                        'user
                        'message
                        (list (make-text-part user-message))
                        (now-seconds)
                        (hasheq)))
        user-message))

  ;; v0.26.0: Reset working set on new user message
  (define ws (hash-ref (agent-session-config sess) 'working-set #f))
  (when ws
    (working-set-reset! ws))

  ;; #771: Buffer user message (deferred persistence) — flushed on first assistant response
  (buffer-or-append!-fn sess user-msg)

  ;; Update index with new entry
  (when idx
    (append-to-leaf! idx user-msg))

  ;; Build context: use context-assembly when provider available, else tree walk
  (define context-messages
    (if idx
        (cond
          [(agent-session-provider sess)
           ;; Use context-assembly for production pipeline with LLM summarization
           (define cfg (make-context-assembly-config #:recent-tokens DEFAULT-TOKEN-BUDGET-THRESHOLD))
           (define result
             (build-assembled-context idx
                                      cfg
                                      #:provider (agent-session-provider sess)
                                      #:model-name (agent-session-model-name sess)
                                      #:working-set ws))
           (context-result-messages result)]
          ;; Fallback: context-assembly tree walk (no LLM summarization)
          [else (build-session-context idx)])
        ;; Fallback: no index — use linear history (backward compat)
        (let ([existing (if (file-exists? log-path)
                            (load-session-log log-path)
                            '())])
          ;; BUG-39: Include buffered user message in context.
          (if (null? existing)
              (list user-msg)
              (append existing (list user-msg))))))

  ;; Extract settings from path entries (#522)
  (define settings (extract-path-settings context-messages))
  (when (hash-ref settings 'model #f)
    (set-agent-session-model-name! sess (hash-ref settings 'model)))

  ;; Inject system instructions as an ephemeral system message prefix
  (define system-instrs (agent-session-system-instructions sess))
  (if (null? system-instrs)
      context-messages
      (cons (make-message (generate-id)
                          #f
                          'system
                          'system-instruction
                          (list (make-text-part (string-join system-instrs "\n\n")))
                          (now-seconds)
                          (hasheq))
            context-messages)))

;; ============================================================
;; dispatch-iteration
;; ============================================================

;; dispatch-iteration — model-select hook + iteration loop dispatch.
;;   Runs the core agent loop with error handling. Returns a loop-result.
(define (dispatch-iteration sess context-with-system max-iterations)
  (define bus (agent-session-event-bus sess))
  (define prov (agent-session-provider sess))
  (define reg (agent-session-tool-registry sess))
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (define sid (agent-session-session-id sess))
  (define cfg (agent-session-config sess))
  (define cancellation-tok (hash-ref cfg 'cancellation-token #f))

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
    (set-agent-session-model-name! sess override-model))

  ;; Run the core agent loop with tool-call iteration
  (with-handlers ([exn:fail? (lambda (e)
                               ;; Emit runtime.error event with classified error-type
                               (define error-type (classify-error e))
                               ;; A3: Include retry metadata if retries were attempted
                               (define base-payload (error-payload (exn-message e) error-type))
                               (define payload
                                 (if (retry-exhausted? e)
                                     (hash-set* (payload->hash base-payload)
                                                'retries-attempted
                                                (retry-exhausted-attempts e)
                                                'total-retry-delay-ms
                                                (retry-exhausted-total-delay-ms e)
                                                'errorHistory
                                                (retry-exhausted-error-history e))
                                     base-payload))
                               (emit-session-event! bus sid "runtime.error" payload)
                               ;; Defense-in-depth: ensure turn.completed is emitted
                               (emit-session-event! bus sid "turn.completed" (hasheq 'reason "error"))
                               (make-loop-result context-with-system 'error payload))])
    (define ws (hash-ref cfg 'working-set #f))
    (run-iteration-loop context-with-system
                        prov
                        bus
                        reg
                        (agent-session-extension-registry sess)
                        log-path
                        sid
                        max-iterations
                        #:cancellation-token cancellation-tok
                        #:config cfg
                        #:working-set ws
                        #:shutdown-check (lambda () (agent-session-shutdown-requested? sess))
                        #:force-shutdown-check (lambda () (agent-session-force-shutdown? sess))
                        #:session sess)))

;; ============================================================
;; run-prompt-internal
;; ============================================================

;; Internal prompt execution, extracted for input hook gating.
(define (run-prompt-internal sess
                             user-message
                             max-iterations
                             token-budget-threshold
                             ensure-persisted!-fn
                             buffer-or-append!-fn)
  (define bus (agent-session-event-bus sess))
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (define idx-path (session-index-path (agent-session-session-dir sess)))
  (define sid (agent-session-session-id sess))

  ;; v0.26.0: Create working set for this prompt and attach to session config
  (define ws (make-working-set))
  (define base-cfg (agent-session-config sess))
  (cond
    [(and (hash? base-cfg) (not (immutable? base-cfg))) (hash-set! base-cfg 'working-set ws)]
    [(hash? base-cfg) (set-agent-session-config! sess (hash-set base-cfg 'working-set ws))]
    [else (set-agent-session-config! sess (hasheq 'working-set ws))])

  ;; 1. Build context: convert message, append to log, load history, inject system instructions
  (define context-with-system
    (build-session-context sess user-message ensure-persisted!-fn buffer-or-append!-fn))

  ;; 2. Check token budget and compact if needed
  (define context-after-compact
    (maybe-compact-context sess context-with-system token-budget-threshold))

  ;; 3. Ensure session directory exists before iteration writes assistant messages
  (ensure-persisted!-fn sess)

  ;; 4. Run the core agent loop (model-select hook + iteration dispatch)
  (define final-result
    (with-telemetry "dispatch-iteration"
                    (dispatch-iteration sess context-after-compact max-iterations)))

  ;; 5. Rebuild index
  (set-agent-session-index! sess (build-index! log-path idx-path))

  ;; 6. Emit session.updated
  (emit-session-event!
   bus
   sid
   "session.updated"
   (hasheq 'sessionId sid 'lastTurnTermination (loop-result-termination-reason final-result)))

  (values sess final-result))

;; ============================================================
;; run-prompt!
;; ============================================================

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
    (raise (exn:fail (format "run-prompt!: session ~a is closed" (agent-session-session-id sess))
                     (current-continuation-marks))))
  ;; Guard against concurrent prompt execution
  (when (agent-session-prompt-running? sess)
    (emit-session-event! (agent-session-event-bus sess)
                         (agent-session-session-id sess)
                         "runtime.error"
                         (hasheq 'error "Prompt already running — ignoring concurrent submission"))
    (raise (exn:fail (format "run-prompt!: session ~a already has a prompt running"
                             (agent-session-session-id sess))
                     (current-continuation-marks))))
  (set-agent-session-prompt-running?! sess #t)
  (define bus (agent-session-event-bus sess))
  (define sid (agent-session-session-id sess))
  ;; F2: Emit turn.started immediately so TUI shows activity
  ;; before context build + compaction. The handler in state-events.rkt
  ;; is idempotent (just sets busy? #t), so the second turn.started
  ;; from agent/loop.rkt during iteration is safe.
  (emit-session-event! bus sid "turn.started" (hasheq 'turnId #f 'sessionId sid))
  (define base-cfg (agent-session-config sess))
  ;; #1391: Inject session index into config for session_recall tool access
  (dynamic-wind
   void
   (lambda ()
     ;; #1391: Inject session index into config for session_recall tool access
     (define idx (agent-session-index sess))
     ;; v0.14.3: Handle both mutable (make-hash) and immutable (hasheq) configs.
     (define cfg
       (if idx
           (if (and (hash? base-cfg) (not (immutable? base-cfg)))
               (begin
                 (hash-set! base-cfg 'session-index idx)
                 base-cfg)
               (hash-set base-cfg 'session-index idx))
           base-cfg))
     (define max-iterations (or max-iter-override (hash-ref cfg 'max-iterations 50)))
     (define token-budget-threshold
       (hash-ref cfg 'token-budget-threshold DEFAULT-TOKEN-BUDGET-THRESHOLD))

     ;; #666: Dispatch 'input hook — intercept/transform user input before processing
     (define ext-reg (agent-session-extension-registry sess))
     (define-values (_processed-input input-hook-res)
       (maybe-dispatch-hooks ext-reg 'input (input-payload sid user-message)))
     (cond
       [(and input-hook-res (eq? (hook-result-action input-hook-res) 'block))
        ;; Input blocked by extension
        (emit-session-event! bus sid "input.blocked" (hasheq 'reason "extension-block"))
        (values sess (make-loop-result '() 'completed (hasheq 'reason "input-blocked")))]
       [else
        (define effective-input
          (if (and input-hook-res (eq? (hook-result-action input-hook-res) 'amend))
              (hash-ref (hook-result-payload input-hook-res) 'message user-message)
              user-message))
        (run-prompt-internal sess effective-input max-iterations token-budget-threshold ep! ba!)]))
   ;; Cleanup: always reset prompt-running? even on error
   ;; B3-A: Emergency persist — defense-in-depth if session not yet persisted
   (lambda ()
     (set-agent-session-prompt-running?! sess #f)
     (unless (agent-session-persisted? sess)
       (with-handlers ([exn:fail? void])
         (ensure-persisted! sess))))))

;; ============================================================
;; Crash logging (B3-A)
;; ============================================================

;; Write crash log entry to ~/.q/crash-<timestamp>.jsonl
(define (write-crash-log! sid error-msg phase)
  (with-handlers ([exn:fail? void])
    (define q-dir (build-path (find-system-path 'home-dir) ".q"))
    (make-directory* q-dir)
    (define crash-path (build-path q-dir (format "crash-~a.jsonl" (current-seconds))))
    (call-with-output-file
     crash-path
     (lambda (out)
       (fprintf out
                "{\"ts\":~a,\"session\":\"~a\",\"error\":\"~a\",\"phase\":\"~a\"}\n"
                (current-seconds)
                (or sid "unknown")
                error-msg
                phase))
     #:mode 'text
     #:exists 'append)))

;; ============================================================
;; Persistence helpers (re-exported for agent-session.rkt)
;; ============================================================

(define (ensure-persisted! sess)
  (unless (agent-session-persisted? sess)
    (make-directory* (agent-session-session-dir sess))
    (set-agent-session-persisted?! sess #t)
    (define log-path (session-log-path (agent-session-session-dir sess)))
    (write-session-version-header! log-path)
    (when (not (null? (agent-session-pending-entries sess)))
      (for ([entry (in-list (reverse (agent-session-pending-entries sess)))])
        (append-entry! log-path entry))
      (set-agent-session-pending-entries! sess '()))))

(define (buffer-or-append! sess entry)
  (if (agent-session-persisted? sess)
      (append-entry! (session-log-path (agent-session-session-dir sess)) entry)
      (set-agent-session-pending-entries! sess (cons entry (agent-session-pending-entries sess)))))
