#lang racket/base

;; runtime/agent-session.rkt — session lifecycle orchestration
;; STABILITY: evolving
;;
;; Central runtime layer that ties together session store, core loop,
;; provider, tool registry, and event bus into a coherent agent session.
;; This is the main entry point for running agent prompts.
;;
;; Provides:
;;   make-agent-session     — create new session (generate ID, init log)
;;   resume-agent-session   — resume existing session (load log + index)
;;   fork-session           — fork session at a given point
;;   run-prompt!            — run a user prompt through the full agent loop
;;   session-id             — get the session ID
;;   session-history        — get full message history (replayed from log)
;;   session-active?        — check if session is active (not closed)
;;   close-session!         — close/deactivate a session
;;   agent-session?         — predicate

(require racket/contract
         racket/string
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
                  event-ev
                  event-payload
                  loop-result-termination-reason
                  make-loop-result)
         "../agent/queue.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload)
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         (only-in "../extensions/message-inject.rkt" injection-event-topic)
         "../runtime/compactor.rkt"
         (only-in "../extensions/api.rkt" extension-name list-extensions)
         ;; TR BOUNDARY: event-payloads.rkt is #lang typed/racket. Untyped consumers
         ;; like this module receive auto-generated contracts from TR's boundary.
         ;; Struct accessors are contract-wrapped at the module boundary,
         ;; providing runtime type safety for all payload constructors.
         (only-in "../util/event-payloads.rkt"
                  session-start-payload
                  session-end-payload
                  session-switch-payload
                  session-id-payload
                  error-payload
                  input-payload
                  payload->hash)
         (only-in "../runtime/context-builder.rkt"
                  (build-session-context context-builder:build-session-context))
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
         (only-in "session-events.rkt" wire-session-event-handlers!)
         (only-in "../llm/token-budget.rkt" DEFAULT-TOKEN-BUDGET-THRESHOLD estimate-context-tokens)
         (only-in "session-compaction.rkt" maybe-compact-context)
         (only-in "session-controls.rkt"
                  set-model!
                  cycle-model!
                  thinking-levels
                  thinking-level?
                  thinking-level->budget
                  set-thinking-level!
                  request-shutdown!
                  force-shutdown!
                  shutdown-requested?
                  force-shutdown-requested?
                  reset-shutdown-flags!)
         (only-in "auto-retry.rkt"
                  classify-error
                  retry-exhausted?
                  retry-exhausted-attempts
                  retry-exhausted-total-delay-ms
                  retry-exhausted-error-history))

(provide agent-session?
         agent-session-session-dir
         agent-session-queue
         agent-session-index
         agent-session-extension-registry
         agent-session-model-name
         ;; FEAT-65: runtime model control
         set-model!
         cycle-model!
         agent-session-system-instructions
         agent-session-compacting?
         set-agent-session-compacting?!
         agent-session-last-compaction-time
         set-agent-session-last-compaction-time!
         agent-session-persisted?
         set-agent-session-persisted?!
         agent-session-pending-entries
         set-agent-session-pending-entries!
         agent-session-prompt-running?
         set-agent-session-prompt-running?!
         ensure-persisted!
         buffer-or-append!
         make-agent-session
         resume-agent-session
         fork-session
         run-prompt!
         session-id
         session-history
         session-active?
         close-session!
         maybe-compact-context
         ;; Thinking level control (#1153)
         thinking-levels
         thinking-level?
         thinking-level->budget
         agent-session-thinking-level
         set-thinking-level!
         ;; Graceful shutdown (#1158)
         agent-session-shutdown-requested?
         agent-session-force-shutdown?
         request-shutdown!
         force-shutdown!
         shutdown-requested?
         force-shutdown-requested?
         reset-shutdown-flags!)

;; ============================================================
;; ARCH-05: struct definition moved to session-types.rkt
;; ============================================================
;; agent-session struct is imported from session-types.rkt.
;; This allows extracted sub-modules (session-events, etc.) to
;; access the struct without circular dependencies.

;; ============================================================
;; Helpers
;; ============================================================
;; session-log-path imported from session-types.rkt (REV-05 DRY)

;; #771: Ensure session directory exists and flush pending entries.
;; ensure-persisted! : agent-session? -> void?
;; Idempotently creates the session directory, writes the version header,
;; and flushes all buffered pending-entries to session.jsonl. No-op if
;; already persisted.
;; Called before the first write to the session log.
;; Idempotent — no-op if already persisted.
(define (ensure-persisted! sess)
  (unless (agent-session-persisted? sess)
    (make-directory* (agent-session-session-dir sess))
    (set-agent-session-persisted?! sess #t)
    ;; #773: Write version header to new session log
    (define log-path (session-log-path (agent-session-session-dir sess)))
    (write-session-version-header! log-path)
    ;; Flush any buffered entries
    (when (not (null? (agent-session-pending-entries sess)))
      (for ([entry (in-list (reverse (agent-session-pending-entries sess)))])
        (append-entry! log-path entry))
      (set-agent-session-pending-entries! sess '()))))

;; #771: Buffer an entry for later persistence, or write immediately if already persisted.
;; buffer-or-append! : agent-session? message? -> void?
;; If the session is already persisted, appends the entry to session.jsonl
;; immediately. Otherwise, pushes it onto pending-entries for later flush.
(define (buffer-or-append! sess entry)
  (if (agent-session-persisted? sess)
      (append-entry! (session-log-path (agent-session-session-dir sess)) entry)
      (set-agent-session-pending-entries! sess (cons entry (agent-session-pending-entries sess)))))

(define (session-index-path dir)
  (build-path dir "session.index"))

;; ============================================================
;; make-agent-session
;; ============================================================

;; config hash keys:
;;   'provider       → provider?  (required)
;;   'tool-registry  → tool-registry? (required)
;;   'event-bus      → event-bus? (required)
;;   'session-dir    → path-string (base directory, required)
;;   'max-iterations → integer (optional, default 20)
;;   'token-budget-threshold → integer (optional, default 100000)

;;; make-agent-session : (hash/c symbol? any/c) -> agent-session?
;;;
;;; Creates a new agent session from a config hash. Required keys:
;;;   'provider, 'tool-registry, 'event-bus, 'session-dir
;;; Optional keys: 'max-iterations, 'token-budget-threshold, 'model-name,
;;;   'system-instructions, 'extension-registry
;;; Generates a unique session ID, emits session.started, dispatches
;;; 'session-start hook, wires event handlers, and emits resources.discover.
(define (make-agent-session config)
  (define sid (generate-id))
  (define base-dir (hash-ref config 'session-dir))
  (define dir (build-path base-dir sid))
  ;; Create directory and version header eagerly so resume works immediately.
  ;; Deferred persistence of entries is still handled via buffer-or-append!.

  ;; Capture session creation time for duration tracking
  (define session-created-at (now-seconds))

  (define sess
    (agent-session sid
                   dir
                   (hash-ref config 'provider)
                   (hash-ref config 'tool-registry)
                   (hash-ref config 'event-bus)
                   (hash-ref config 'extension-registry #f)
                   (hash-ref config 'model-name #f)
                   (hash-ref config 'system-instructions '())
                   #f ; index — built on first use
                   (make-queue)
                   config
                   #t ; active
                   session-created-at
                   #f ; compacting?
                   #f ; last-compaction-time
                   #f ; persisted?
                   '() ; pending-entries
                   (hash-ref config 'thinking-level 'medium) ; thinking-level (#1153)
                   #f ; shutdown-requested? (#1158)
                   #f ; force-shutdown?
                   #f)) ; prompt-running?

  ;; Emit session.started
  (emit-session-event! (agent-session-event-bus sess) sid "session.started" (session-id-payload sid))

  ;; Eagerly persist session directory and version header so resume works immediately.
  (ensure-persisted! sess)

  ;; Dispatch 'session-start hook (R2-7: payload with session-id, config, and reason)
  (define-values (_start-payload _session-start-res)
    (maybe-dispatch-hooks (hash-ref config 'extension-registry #f)
                          'session-start
                          (session-start-payload sid config 'new)))

  ;; Subscribe to fork.requested and compact.requested events from TUI/CLI
  (wire-session-event-handlers! sess fork-session)

  ;; #668: Emit resources.discover event — extensions can discover skill/prompt/theme paths
  (let ([ext-reg (hash-ref config 'extension-registry #f)])
    (emit-session-event! (agent-session-event-bus sess)
                         sid
                         "resources.discover"
                         (hasheq 'session-id
                                 sid
                                 'extensions
                                 (if ext-reg
                                     (map extension-name (list-extensions ext-reg))
                                     '()))))

  ;; DI-01 (v0.22.7): Set DI parameters for concrete implementations
  ;; so iteration.rkt can use them without direct imports
  (current-compact-proc compact-history)
  (current-estimate-tokens estimate-context-tokens)
  (current-inject-topic injection-event-topic)

  sess)

;; ============================================================
;; resume-agent-session
;; ============================================================

;;; resume-agent-session : string? (hash/c symbol? any/c) -> agent-session?
;;;
;;; Resumes an existing session by ID. Validates directory exists,
;;; rebuilds session index from log, dispatches 'session-before-switch hook
;;; (extensions can block), emits session.resumed, dispatches 'session-start
;;; with reason 'resume, and wires event handlers.
(define (resume-agent-session session-id config)
  (define base-dir (hash-ref config 'session-dir))
  (define dir (build-path base-dir session-id))

  ;; For resume, the directory must already exist (session was previously
  ;; written to). Lazy persistence only defers creation in make-agent-session.
  (unless (directory-exists? dir)
    (error 'resume-agent-session "session directory not found: ~a" dir))

  ;; #773: Ensure session version header is up to date
  (define log-path (session-log-path dir))
  (when (file-exists? log-path)
    (ensure-session-version-header! log-path))

  (define idx-path (session-index-path dir))

  ;; Rebuild index from log (if log exists)
  (define idx
    (if (file-exists? log-path)
        (build-index! log-path idx-path)
        #f))

  ;; Dispatch 'session-before-switch hook — extensions can block session resume
  (define switch-payload (session-switch-payload session-id 'resume))
  (define-values (_amended-switch switch-res)
    (maybe-dispatch-hooks (hash-ref config 'extension-registry #f)
                          'session-before-switch
                          switch-payload))
  (when (and switch-res (eq? (hook-result-action switch-res) 'block))
    (error 'resume-agent-session "session resume blocked by extension"))

  (define sess
    (agent-session session-id
                   dir
                   (hash-ref config 'provider)
                   (hash-ref config 'tool-registry)
                   (hash-ref config 'event-bus)
                   (hash-ref config 'extension-registry #f)
                   (hash-ref config 'model-name #f)
                   (hash-ref config 'system-instructions '())
                   idx
                   (make-queue)
                   config
                   #t
                   (now-seconds)
                   #f ; compacting?
                   #f ; last-compaction-time
                   #t ; persisted? (resumed sessions already have directory)
                   '() ; pending-entries
                   (hash-ref config 'thinking-level 'medium) ; thinking-level (#1153)
                   #f ; shutdown-requested? (#1158)
                   #f ; force-shutdown?
                   #f)) ; prompt-running?

  ;; Emit session.resumed
  (emit-session-event! (agent-session-event-bus sess)
                       session-id
                       "session.resumed"
                       (session-id-payload session-id))

  ;; Dispatch 'session-start hook with reason 'resume
  (define resume-start-payload (session-start-payload session-id config 'resume))
  (maybe-dispatch-hooks (hash-ref config 'extension-registry #f) 'session-start resume-start-payload)

  ;; Subscribe to fork/compact events from TUI/CLI
  (wire-session-event-handlers! sess fork-session)

  ;; DI-03 (v0.22.8): Set DI parameters for resumed sessions too
  (current-compact-proc compact-history)
  (current-estimate-tokens estimate-context-tokens)
  (current-inject-topic injection-event-topic)

  sess)

;; ============================================================
;; fork-session
;; ============================================================

;;; fork-session : agent-session? [(or/c string? #f)] -> agent-session?
;;;
;;; Forks the session at a given entry ID (or latest point). Creates a new
;;; session directory, copies log entries up to fork point, builds fresh
;;; index, emits session.forked, dispatches 'session-before-fork and
;;; 'session-start hooks. Raises error if source session is closed.
(define (fork-session sess [parent-entry-id #f])
  ;; Guard — refuse fork on closed session
  (unless (session-active? sess)
    (raise (exn:fail (format "fork-session: session ~a is closed" (agent-session-session-id sess))
                     (current-continuation-marks))))
  ;; #669: Dispatch 'session-before-fork hook — extensions can block fork
  (let* ([ext-reg (agent-session-extension-registry sess)]
         [fork-payload (hasheq 'session-id
                               (agent-session-session-id sess)
                               'parent-entry-id
                               parent-entry-id
                               'reason
                               "user-fork")])
    (maybe-dispatch-hooks ext-reg 'session-before-fork fork-payload)
    ;; For now: proceed with fork (block support would short-circuit here)
    (fork-session-internal sess parent-entry-id)))

(define (fork-session-internal sess parent-entry-id)
  (define new-id (generate-id))
  (define base-dir (path-only (simple-form-path (agent-session-session-dir sess))))
  (define new-dir (build-path base-dir new-id))
  (make-directory* new-dir)

  ;; Load existing log
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (define entries
    (if (file-exists? log-path)
        (load-session-log log-path)
        '()))

  ;; Filter entries up to fork point
  (define entries-to-copy
    (if parent-entry-id
        ;; Validate that parent-entry-id exists in the log
        (let ()
          (define id-exists?
            (for/or ([e (in-list entries)])
              (equal? (message-id e) parent-entry-id)))
          (cond
            ;; Entry not found — emit warning and copy all entries instead
            [(not id-exists?) entries]
            [else
             ;; Copy entries up to and including parent-entry-id
             (let loop ([remaining entries]
                        [acc '()])
               (cond
                 [(null? remaining) (reverse acc)]
                 [(equal? (message-id (car remaining)) parent-entry-id)
                  (reverse (cons (car remaining) acc))]
                 [else (loop (cdr remaining) (cons (car remaining) acc))]))]))
        ;; Copy all entries
        entries))

  ;; Write entries to new session log
  (define new-log-path (session-log-path new-dir))
  (append-entries! new-log-path entries-to-copy)

  ;; Build index for new session
  (define new-idx-path (session-index-path new-dir))
  (define new-idx
    (if (null? entries-to-copy)
        #f
        (build-index! new-log-path new-idx-path)))

  (define new-sess
    (agent-session new-id
                   new-dir
                   (agent-session-provider sess)
                   (agent-session-tool-registry sess)
                   (agent-session-event-bus sess)
                   (agent-session-extension-registry sess)
                   (agent-session-model-name sess)
                   (agent-session-system-instructions sess)
                   new-idx
                   (make-queue)
                   (agent-session-config sess)
                   #t
                   (now-seconds)
                   #f ; compacting?
                   #f ; last-compaction-time
                   #t ; persisted? (fork already created directory and wrote entries)
                   '() ; pending-entries
                   (agent-session-thinking-level sess) ; inherit thinking-level (#1153)
                   #f ; shutdown-requested? (#1158)
                   #f ; force-shutdown?
                   #f)) ; prompt-running?

  ;; Emit session.forked on original session's bus
  (emit-session-event! (agent-session-event-bus sess)
                       (agent-session-session-id sess)
                       "session.forked"
                       (hasheq 'newSessionId
                               new-id
                               'parentSessionId
                               (agent-session-session-id sess)
                               'forkPoint
                               (or parent-entry-id "latest")))

  ;; Dispatch 'session-start hook with reason 'fork
  (when (agent-session-extension-registry sess)
    (maybe-dispatch-hooks
     (agent-session-extension-registry sess)
     'session-start
     (hasheq 'session-id new-id 'reason 'fork 'parent-session-id (agent-session-session-id sess))))

  new-sess)

;; ============================================================
;; Private helpers extracted from run-prompt!
;; ============================================================

;; build-session-context — context preparation:
;;   converts user-message, appends to log, builds/updates index,
;;   walks tree via context-builder, injects system instructions.
;;   Returns the context message list.
;;
;;   Wave 1 (#520): Uses session-index + context-builder tree walk
;;   instead of linear load-session-log.
(define (build-session-context sess user-message)
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

  ;; #771: Buffer user message (deferred persistence) — flushed on first assistant response
  (buffer-or-append! sess user-msg)

  ;; Update index with new entry
  (when idx
    (append-to-leaf! idx user-msg))

  ;; Build context: tree walk via context-builder when index available
  (define context-messages
    (if idx
        (context-builder:build-session-context idx)
        ;; Fallback: no index — use linear history (backward compat)
        (let ([existing (if (file-exists? log-path)
                            (load-session-log log-path)
                            '())])
          ;; BUG-39: Include buffered user message in context.
          ;; On first prompt, the user message was buffered by buffer-or-append!
          ;; but not yet persisted. Without this, context is empty → 0 tokens →
          ;; should-compact? returns #f → compaction/hooks never fire on turn 1.
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

;; extract-path-settings imported from runtime/session-context.rkt

;; ARCH-05: maybe-compact-context extracted to session-compaction.rkt

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
                        #:shutdown-check (lambda () (agent-session-shutdown-requested? sess))
                        #:force-shutdown-check (lambda () (agent-session-force-shutdown? sess)))))

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
(define (run-prompt! sess user-message #:max-iterations [max-iter-override #f])
  ;; B4: Guard — refuse operations on closed sessions
  (unless (session-active? sess)
    (raise (exn:fail (format "run-prompt!: session ~a is closed" (agent-session-session-id sess))
                     (current-continuation-marks))))
  ;; Guard against concurrent prompt execution
  ;; If a prompt is already running, reject this call to prevent consecutive
  ;; user messages in the API (which causes 400 errors on GLM/OpenAI).
  (when (agent-session-prompt-running? sess)
    (emit-session-event! (agent-session-event-bus sess)
                         (agent-session-session-id sess)
                         "runtime.error"
                         (hasheq 'error "Prompt already running — ignoring concurrent submission"))
    (raise (exn:fail (format "run-prompt!: session ~a already has a prompt running"
                             (agent-session-session-id sess))
                     (current-continuation-marks))))
  (set-agent-session-prompt-running?! sess #t)
  (define cleanup-thunk (lambda () (set-agent-session-prompt-running?! sess #f)))
  (define bus (agent-session-event-bus sess))
  (define sid (agent-session-session-id sess))
  (define base-cfg (agent-session-config sess))
  ;; #1391: Inject session index into config for session_recall tool access
  (dynamic-wind
   void
   (lambda ()
     ;; #1391: Inject session index into config for session_recall tool access
     (define idx (agent-session-index sess))
     ;; v0.14.3: Handle both mutable (make-hash) and immutable (hasheq) configs.
     ;; Production uses make-hash (mutable), but some tests use hasheq (immutable).
     (define cfg
       (if idx
           (if (and (hash? base-cfg) (not (immutable? base-cfg)))
               (begin
                 (hash-set! base-cfg 'session-index idx)
                 base-cfg)
               (hash-set base-cfg 'session-index idx))
           base-cfg))
     (define max-iterations (or max-iter-override (hash-ref cfg 'max-iterations 50)))
     ;; v0.14.4 Wave 1: Increased default from 20→50 for slow exploration-heavy models
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
        (run-prompt-internal sess effective-input max-iterations token-budget-threshold)]))
   ;; Cleanup: always reset prompt-running? even on error
   (lambda () (set-agent-session-prompt-running?! sess #f))))

;; ============================================================
;; Iteration loop lives in iteration.rkt
;; ============================================================
;; run-iteration-loop is imported from iteration.rkt

;; ============================================================
;; Accessors
;; ============================================================

;; session-id : agent-session? -> string?
;; Returns the session's unique ID string.
(define (session-id sess)
  (agent-session-session-id sess))

;; #666: Internal prompt execution, extracted for input hook gating.
(define (run-prompt-internal sess user-message max-iterations token-budget-threshold)
  (define bus (agent-session-event-bus sess))
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (define idx-path (session-index-path (agent-session-session-dir sess)))
  (define sid (agent-session-session-id sess))

  ;; 1. Build context: convert message, append to log, load history, inject system instructions
  (define context-with-system (build-session-context sess user-message))

  ;; 2. Check token budget and compact if needed
  (define context-after-compact
    (maybe-compact-context sess context-with-system token-budget-threshold))

  ;; 3. Ensure session directory exists before iteration writes assistant messages
  (ensure-persisted! sess)

  ;; 4. Run the core agent loop (model-select hook + iteration dispatch)
  (define final-result (dispatch-iteration sess context-after-compact max-iterations))

  ;; 5. Rebuild index
  (set-agent-session-index! sess (build-index! log-path idx-path))

  ;; 6. Emit session.updated
  (emit-session-event!
   bus
   sid
   "session.updated"
   (hasheq 'sessionId sid 'lastTurnTermination (loop-result-termination-reason final-result)))

  (values sess final-result))

;; session-history : agent-session? -> (listof message?)
;; Loads and returns the full message history from session.jsonl,
;; filtering out session-info (version header) entries. Returns '() if
;; no log file exists.
(define (session-history sess)
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (if (file-exists? log-path)
      ;; #773: Filter out session-info (version header) entries
      (filter (lambda (m) (not (eq? (message-kind m) 'session-info))) (load-session-log log-path))
      '()))

;; session-active? : agent-session? -> boolean?
;; Returns #t if the session has not been closed.
(define (session-active? sess)
  (agent-session-active? sess))

;;; close-session! : agent-session? -> void?
;;;
;;; Closes/deactivates the session. Flushes pending entries, emits
;;; session.closed, dispatches 'session-shutdown hook with duration,
;;; and sets active? to #f. Idempotent — no-op if already closed.
(define (close-session! sess)
  ;; B5: Idempotency guard — only close once
  (when (session-active? sess)
    ;; BUG-40: Flush buffered entries before deactivating.
    ;; Without this, entries buffered via buffer-or-append! are lost
    ;; and resume-agent-session fails with "directory not found".
    (ensure-persisted! sess)
    (emit-session-event! (agent-session-event-bus sess)
                         (agent-session-session-id sess)
                         "session.closed"
                         (session-id-payload (agent-session-session-id sess)))
    ;; Dispatch 'session-shutdown hook (R2-7: payload with session-id and duration)
    (define session-duration (- (now-seconds) (agent-session-start-time sess)))
    (define shutdown-payload (session-end-payload (agent-session-session-id sess) session-duration))
    (define-values (_shutdown-payload _shutdown-res)
      (maybe-dispatch-hooks (agent-session-extension-registry sess)
                            'session-shutdown
                            shutdown-payload))
    (set-agent-session-active?! sess #f)))

;; ============================================================
;; Event bus wiring — ARCH-05b: extracted to session-events.rkt
;; ============================================================
;; wire-session-event-handlers! is imported from session-events.rkt.
;; It now receives fork-handler as a parameter to avoid circular deps.

;; ============================================================
;; ARCH-05: Model/thinking/shutdown controls extracted to session-controls.rkt
;; ============================================================
