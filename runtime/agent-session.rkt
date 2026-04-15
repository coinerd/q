#lang racket/base

;; runtime/agent-session.rkt — session lifecycle orchestration
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
         "../llm/token-budget.rkt"
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload)
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/compactor.rkt"
         (only-in "../extensions/api.rkt"
                  extension-name
                  list-extensions)
         (only-in "../runtime/context-builder.rkt"
                  (build-session-context context-builder:build-session-context))
         "../util/ids.rkt"
         (only-in "iteration.rkt" run-iteration-loop emit-session-event! maybe-dispatch-hooks))

(provide agent-session?
         agent-session-session-dir
         agent-session-extension-registry
         agent-session-model-name
         agent-session-system-instructions
         agent-session-compacting?
         set-agent-session-compacting?!
         agent-session-last-compaction-time
         set-agent-session-last-compaction-time!
         agent-session-persisted?
         set-agent-session-persisted?!
         agent-session-pending-entries
         set-agent-session-pending-entries!
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
         maybe-compact-context)

;; ============================================================
;; Internal struct
;; ============================================================

(struct agent-session
        (session-id ; string
         session-dir ; path
         provider ; provider?
         tool-registry ; tool-registry?
         event-bus ; event-bus?
         extension-registry ; extension-registry? or #f
         [model-name #:mutable] ; string or #f
         system-instructions ; (listof string)
         [index #:mutable] ; session-index? or #f
         queue ; queue?
         config ; hash (runtime settings)
         [active? #:mutable] ; boolean
         [start-time #:mutable] ; integer (seconds since epoch)
         [compacting? #:mutable] ; boolean — guard against recursive compaction
         [last-compaction-time #:mutable] ; integer or #f — timestamp of last compaction
         [persisted? #:mutable] ; boolean — #f until directory + first write
         [pending-entries #:mutable]) ; (listof message?) — buffered before persistence
  #:transparent)

;; ============================================================
;; Helpers
;; ============================================================

(define (session-log-path dir)
  (build-path dir "session.jsonl"))

;; #771: Ensure session directory exists and flush pending entries.
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
(define (buffer-or-append! sess entry)
  (if (agent-session-persisted? sess)
      (append-entry! (session-log-path (agent-session-session-dir sess)) entry)
      (set-agent-session-pending-entries!
       sess (cons entry (agent-session-pending-entries sess)))))

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
;;   'max-iterations → integer (optional, default 10)
;;   'token-budget-threshold → integer (optional, default 100000)

(define (make-agent-session config)
  (define sid (generate-id))
  (define base-dir (hash-ref config 'session-dir))
  (define dir (build-path base-dir sid))
  ;; #771: Don't create directory yet — deferred until first assistant response

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
                   '())) ; pending-entries

  ;; Emit session.started
  (emit-session-event! (agent-session-event-bus sess) sid "session.started" (hasheq 'sessionId sid))

  ;; Dispatch 'session-start hook (R2-7: payload with session-id, config, and reason)
  (define session-start-payload (hasheq 'session-id sid 'config config 'reason 'new))
  (define-values (_session-start-payload _session-start-res)
    (maybe-dispatch-hooks (hash-ref config 'extension-registry #f)
                          'session-start
                          session-start-payload))

  ;; Subscribe to fork.requested and compact.requested events from TUI/CLI
  (wire-session-event-handlers! sess)

  ;; #668: Emit resources.discover event — extensions can discover skill/prompt/theme paths
  (let ([ext-reg (hash-ref config 'extension-registry #f)])
    (emit-session-event!
     (agent-session-event-bus sess)
     sid
     "resources.discover"
     (hasheq 'session-id sid
             'extensions (if ext-reg
                             (map extension-name (list-extensions ext-reg))
                             '()))))

  sess)

;; ============================================================
;; resume-agent-session
;; ============================================================

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
  (define switch-payload (hasheq 'session-id session-id 'operation 'resume))
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
                   '())) ; pending-entries

  ;; Emit session.resumed
  (emit-session-event! (agent-session-event-bus sess)
                       session-id
                       "session.resumed"
                       (hasheq 'sessionId session-id))

  ;; Dispatch 'session-start hook with reason 'resume
  (define resume-start-payload (hasheq 'session-id session-id 'config config 'reason 'resume))
  (maybe-dispatch-hooks (hash-ref config 'extension-registry #f)
                        'session-start
                        resume-start-payload)

  ;; Subscribe to fork/compact events from TUI/CLI
  (wire-session-event-handlers! sess)

  sess)

;; ============================================================
;; fork-session
;; ============================================================

(define (fork-session sess [parent-entry-id #f])
  ;; #669: Dispatch 'session-before-fork hook — extensions can block fork
  (let* ([ext-reg (agent-session-extension-registry sess)]
         [fork-payload (hasheq 'session-id (agent-session-session-id sess)
                               'parent-entry-id parent-entry-id
                               'reason "user-fork")])
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
                   '())) ; pending-entries

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
    (maybe-dispatch-hooks (agent-session-extension-registry sess)
                          'session-start
                          (hasheq 'session-id new-id
                                  'reason 'fork
                                  'parent-session-id (agent-session-session-id sess))))

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
      (set-agent-session-index!
       sess (build-index! log-path idx-path))))
  (define idx (agent-session-index sess))

  ;; Convert string to message struct if needed
  (define user-msg
    (if (string? user-message)
        (let ()
          ;; Determine parent from active leaf in index (#521: use stored IDs)
          (define parent-id
            (if idx
                (let ([leaf (active-leaf idx)])
                  (and leaf (message-id leaf)))
                ;; No index yet — determine from log
                (let ([existing (if (file-exists? log-path)
                                    (load-session-log log-path)
                                    '())])
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
        (load-session-log log-path)))

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

;; extract-path-settings — walk context messages to find latest
;;   model-change and thinking-level-change entries.
;;   Returns a hash with 'model and 'thinking-level keys.
(define (extract-path-settings messages)
  (for/fold ([settings (hasheq)])
            ([msg (in-list messages)])
    (define kind (message-kind msg))
    (cond
      [(and (eq? kind 'model-change)
            (hash? (message-meta msg)))
       (hash-set settings 'model (hash-ref (message-meta msg) 'model #f))]
      [(and (eq? kind 'thinking-level-change)
            (hash? (message-meta msg)))
       (hash-set settings 'thinking-level (hash-ref (message-meta msg) 'level #f))]
      [else settings])))

;; maybe-compact-context — token budget check and compaction triggering.
;;   May mutate context (return a compacted version). Returns the
;;   (possibly compacted) context message list.
(define (maybe-compact-context sess context-with-system token-budget-threshold)
  (define bus (agent-session-event-bus sess))
  (define sid (agent-session-session-id sess))

  (define raw-messages
    (for/list ([msg (in-list context-with-system)])
      (hasheq 'role
              (symbol->string (message-role msg))
              'content
              (map content-part->jsexpr (message-content msg)))))
  (define token-count (estimate-context-tokens raw-messages))
  (cond
    [(not (should-compact? token-count token-budget-threshold)) context-with-system]
    [else
     ;; #770: Stale usage guard — skip if compaction just completed
     (define last-compact (agent-session-last-compaction-time sess))
     (define now-ms (current-inexact-milliseconds))
     (cond
       [(and last-compact (< (- now-ms last-compact) 2000))
        context-with-system] ; too soon after last compaction
       [(agent-session-compacting? sess)
        context-with-system] ; recursive compaction guard
       [else
        (set-agent-session-compacting?! sess #t)
        (emit-session-event! bus sid "compaction.start"
                             (hasheq 'tokenCount token-count 'budgetThreshold token-budget-threshold))
        (dynamic-wind
         (lambda () (void))
         (lambda ()
           (maybe-compact-context-internal sess context-with-system token-count token-budget-threshold bus sid))
         (lambda ()
           (set-agent-session-compacting?! sess #f)
           (set-agent-session-last-compaction-time! sess (current-inexact-milliseconds))
           (emit-session-event! bus sid "compaction.end"
                                (hasheq 'tokenCount token-count))))])]))

;; Internal compaction logic (extracted from maybe-compact-context for dynamic-wind)
(define (maybe-compact-context-internal sess context-with-system token-count token-budget-threshold bus sid)
  ;; Dispatch 'session-before-compact hook
  (define compact-payload
    (hasheq 'session-id
            sid
            'token-count
            token-count
            'budget-threshold
            token-budget-threshold
            'message-count
            (length context-with-system)))
  (define-values (_amended-compact compact-hook-res)
    (maybe-dispatch-hooks (agent-session-extension-registry sess)
                          'session-before-compact
                          compact-payload))
  (cond
    [(and compact-hook-res (eq? (hook-result-action compact-hook-res) 'block)) context-with-system]
    [else
     (emit-session-event! bus sid "compaction.warning"
                          (hasheq 'tokenCount token-count 'budgetThreshold token-budget-threshold))
     (define compact-result (compact-history context-with-system))
     (emit-session-event! bus sid "compaction.completed"
                          (hasheq 'removedCount
                                  (compaction-result-removed-count compact-result)
                                  'keptCount
                                  (length (compaction-result-kept-messages compact-result))
                                  'tokenCount token-count))
     (compaction-result->message-list compact-result)]))

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
  (with-handlers ([exn:fail?
                   (lambda (e)
                     ;; Emit runtime.error event
                     (emit-session-event! bus sid "runtime.error" (hasheq 'error (exn-message e)))
                     ;; Classify error: distinguish iteration limits from provider failures
                     (define error-type
                       (if (regexp-match? #rx"max.iterations" (exn-message e))
                           'max-iterations-exceeded
                           'provider-error))
                     (make-loop-result context-with-system
                                       'error
                                       (hasheq 'error (exn-message e) 'errorType error-type)))])
    (run-iteration-loop context-with-system
                        prov
                        bus
                        reg
                        (agent-session-extension-registry sess)
                        log-path
                        sid
                        max-iterations
                        #:cancellation-token cancellation-tok
                        #:config cfg)))

;; ============================================================
;; run-prompt!
;; ============================================================

(define (run-prompt! sess user-message #:max-iterations [max-iter-override #f])
  (define bus (agent-session-event-bus sess))
  (define sid (agent-session-session-id sess))
  (define cfg (agent-session-config sess))
  (define max-iterations (or max-iter-override (hash-ref cfg 'max-iterations 10)))
  (define token-budget-threshold
    (hash-ref cfg 'token-budget-threshold DEFAULT-TOKEN-BUDGET-THRESHOLD))

  ;; #666: Dispatch 'input hook — intercept/transform user input before processing
  (define ext-reg (agent-session-extension-registry sess))
  (define-values (_processed-input input-hook-res)
    (maybe-dispatch-hooks ext-reg 'input
                          (hasheq 'session-id sid 'message user-message)))
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

;; ============================================================
;; Iteration loop lives in iteration.rkt
;; ============================================================
;; run-iteration-loop is imported from iteration.rkt

;; ============================================================
;; Accessors
;; ============================================================

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

(define (session-history sess)
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (if (file-exists? log-path)
      ;; #773: Filter out session-info (version header) entries
      (filter (lambda (m) (not (eq? (message-kind m) 'session-info)))
              (load-session-log log-path))
      '()))

(define (session-active? sess)
  (agent-session-active? sess))

(define (close-session! sess)
  (emit-session-event! (agent-session-event-bus sess)
                       (agent-session-session-id sess)
                       "session.closed"
                       (hasheq 'sessionId (agent-session-session-id sess)))
  ;; Dispatch 'session-shutdown hook (R2-7: payload with session-id and duration)
  (define session-duration (- (now-seconds) (agent-session-start-time sess)))
  (define shutdown-payload
    (hasheq 'session-id (agent-session-session-id sess) 'duration session-duration))
  (define-values (_shutdown-payload _shutdown-res)
    (maybe-dispatch-hooks (agent-session-extension-registry sess) 'session-shutdown shutdown-payload))
  (set-agent-session-active?! sess #f))

;; ============================================================
;; Event bus wiring — handle fork/compact requests from TUI/CLI
;; ============================================================

;; Wire event-bus subscribers for fork.requested and compact.requested events.
;; These events are published by TUI/CLI commands and need runtime handlers.
(define (wire-session-event-handlers! sess)
  (define bus (agent-session-event-bus sess))
  (when bus
    ;; Subscribe to fork.requested — call fork-session with the entry-id
    (subscribe! bus
                (lambda (evt)
                  (define payload (event-payload evt))
                  (define entry-id (and (hash? payload) (hash-ref payload 'entry-id #f)))
                  (with-handlers ([exn:fail? (lambda (e)
                                               (emit-session-event! bus
                                                                    (agent-session-session-id sess)
                                                                    "session.fork.failed"
                                                                    (hasheq 'error
                                                                            (exn-message e))))])
                    (define new-sess (fork-session sess entry-id))
                    (emit-session-event! bus
                                         (agent-session-session-id sess)
                                         "session.fork.completed"
                                         (hasheq 'newSessionId
                                                 (agent-session-session-id new-sess)
                                                 'forkPoint
                                                 (or entry-id "latest")))))
                #:filter (lambda (evt) (equal? (event-ev evt) "fork.requested")))

    ;; Subscribe to compact.requested — compact the session context
    (subscribe!
     bus
     (lambda (evt)
       (with-handlers ([exn:fail? (lambda (e)
                                    (emit-session-event! bus
                                                         (agent-session-session-id sess)
                                                         "session.compact.failed"
                                                         (hasheq 'error (exn-message e))))])
         (define log-path (session-log-path (agent-session-session-dir sess)))
         (when (file-exists? log-path)
           (define history (load-session-log log-path))
           (when (and (not (null? history))
                      (not (agent-session-compacting? sess)))
             (set-agent-session-compacting?! sess #t)
             (define compact-result (compact-history history))
             (set-agent-session-compacting?! sess #f)
             (emit-session-event! bus
                                  (agent-session-session-id sess)
                                  "session.compact.completed"
                                  (hasheq 'removedCount
                                          (compaction-result-removed-count compact-result)))))))
     #:filter (lambda (evt) (equal? (event-ev evt) "compact.requested")))))
