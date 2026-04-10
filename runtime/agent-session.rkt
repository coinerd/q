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
         (only-in "../agent/types.rkt"
                  message-id message-role message-content
                  make-message make-text-part
                  content-part->jsexpr
                  loop-result-termination-reason
                  make-loop-result)
         "../agent/queue.rkt"
         "../llm/token-budget.rkt"
         (only-in "../extensions/hooks.rkt"
                  hook-result-action hook-result-payload)
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/compactor.rkt"
         "../util/ids.rkt"
         (only-in "iteration.rkt"
                  run-iteration-loop
                  emit-session-event!
                  maybe-dispatch-hooks))

(provide
 agent-session?
 agent-session-session-dir
 agent-session-extension-registry
 agent-session-model-name
 agent-session-system-instructions
 make-agent-session
 resume-agent-session
 fork-session
 run-prompt!
 session-id
 session-history
 session-active?
 close-session!)

;; ============================================================
;; Internal struct
;; ============================================================

(struct agent-session (session-id       ; string
                       session-dir      ; path
                       provider         ; provider?
                       tool-registry    ; tool-registry?
                       event-bus        ; event-bus?
                       extension-registry ; extension-registry? or #f
                       [model-name #:mutable] ; string or #f
                       system-instructions ; (listof string)
                       [index #:mutable] ; session-index? or #f
                       queue            ; queue?
                       config           ; hash (runtime settings)
                       [active? #:mutable] ; boolean
                       [start-time #:mutable]) ; integer (seconds since epoch)
  #:transparent)

;; ============================================================
;; Helpers
;; ============================================================

(define (session-log-path dir)
  (build-path dir "session.jsonl"))

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
  (make-directory* dir)

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
                   #f   ; index — built on first use
                   (make-queue)
                   config
                   #t    ; active
                   session-created-at))

  ;; Emit session.started
  (emit-session-event! (agent-session-event-bus sess)
                       sid "session.started"
                       (hasheq 'sessionId sid))

  ;; Dispatch 'session-start hook (R2-7: payload with session-id and config)
  (define session-start-payload
    (hasheq 'session-id sid
            'config config))
  (define-values (_session-start-payload _session-start-res)
    (maybe-dispatch-hooks (hash-ref config 'extension-registry #f) 'session-start session-start-payload))

  sess)

;; ============================================================
;; resume-agent-session
;; ============================================================

(define (resume-agent-session session-id config)
  (define base-dir (hash-ref config 'session-dir))
  (define dir (build-path base-dir session-id))

  (unless (directory-exists? dir)
    (error 'resume-agent-session "session directory not found: ~a" dir))

  (define log-path (session-log-path dir))
  (define idx-path (session-index-path dir))

  ;; Rebuild index from log (if log exists)
  (define idx
    (if (file-exists? log-path)
        (build-index! log-path idx-path)
        #f))

  ;; Dispatch 'session-before-switch hook — extensions can block session resume
  (define switch-payload
    (hasheq 'session-id session-id 'operation 'resume))
  (define-values (_amended-switch switch-res)
    (maybe-dispatch-hooks (hash-ref config 'extension-registry #f)
                          'session-before-switch switch-payload))
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
                   (now-seconds)))

  ;; Emit session.resumed
  (emit-session-event! (agent-session-event-bus sess)
                       session-id "session.resumed"
                       (hasheq 'sessionId session-id))

  sess)

;; ============================================================
;; fork-session
;; ============================================================

(define (fork-session sess [parent-entry-id #f])
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
            [(not id-exists?)
             ;; Entry not found — emit warning and copy all entries instead
             entries]
            [else
             ;; Copy entries up to and including parent-entry-id
             (let loop ([remaining entries] [acc '()])
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
                   (now-seconds)))

  ;; Emit session.forked on original session's bus
  (emit-session-event! (agent-session-event-bus sess)
                       (agent-session-session-id sess)
                       "session.forked"
                       (hasheq 'newSessionId new-id
                               'parentSessionId (agent-session-session-id sess)
                               'forkPoint (or parent-entry-id "latest")))

  new-sess)

;; ============================================================
;; run-prompt!
;; ============================================================

(define (run-prompt! sess user-message
                     #:max-iterations [max-iter-override #f])
  (define bus (agent-session-event-bus sess))
  (define prov (agent-session-provider sess))
  (define reg (agent-session-tool-registry sess))
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (define idx-path (session-index-path (agent-session-session-dir sess)))
  (define sid (agent-session-session-id sess))
  (define cfg (agent-session-config sess))
  (define max-iterations
    (or max-iter-override
        (hash-ref cfg 'max-iterations 10)))
  (define token-budget-threshold
    (hash-ref cfg 'token-budget-threshold 100000))

  ;; Convert string to message struct if needed
  (define user-msg
    (if (string? user-message)
        (let ()
          ;; Determine parent from existing history
          (define existing
            (if (file-exists? log-path)
                (load-session-log log-path)
                '()))
          (define parent-id
            (if (null? existing) #f (message-id (last existing))))
          (make-message (generate-id) parent-id 'user 'message
                        (list (make-text-part user-message))
                        (now-seconds)
                        (hasheq)))
        user-message))

  ;; 1. Append user message to session log
  (append-entry! log-path user-msg)

  ;; 2. Build context from full session history
  (define history (load-session-log log-path))

  ;; 3. Inject system instructions as an ephemeral system message prefix
  (define system-instrs (agent-session-system-instructions sess))
  (define context-with-system
    (if (null? system-instrs)
        history
        (cons (make-message (generate-id) #f 'system 'system-instruction
                            (list (make-text-part (string-join system-instrs "\n\n")))
                            (now-seconds)
                            (hasheq))
              history)))

  ;; 4. Check token budget (on full context including system message)
  (define raw-messages
    (for/list ([msg (in-list context-with-system)])
      (hasheq 'role (symbol->string (message-role msg))
              'content (map content-part->jsexpr (message-content msg)))))
  (define token-count (estimate-context-tokens raw-messages))
  (when (should-compact? token-count token-budget-threshold)
    ;; Dispatch 'session-before-compact hook — extensions can amend or block compaction
    (define compact-payload
      (hasheq 'session-id sid
              'token-count token-count
              'budget-threshold token-budget-threshold
              'message-count (length context-with-system)))
    (define-values (amended-compact compact-hook-res)
      (maybe-dispatch-hooks (agent-session-extension-registry sess) 'session-before-compact compact-payload))
    ;; Proceed with compaction unless blocked
    (unless (and compact-hook-res (eq? (hook-result-action compact-hook-res) 'block))
      (emit-session-event! bus sid "compaction.warning"
                           (hasheq 'tokenCount token-count
                                   'budgetThreshold token-budget-threshold))
      ;; R2-6: Auto-compact when threshold exceeded
      (define compact-result (compact-history context-with-system))
      (set! context-with-system (compaction-result->message-list compact-result))
      (emit-session-event! bus sid "compaction.completed"
                           (hasheq 'removedCount (compaction-result-removed-count compact-result)
                                   'keptCount (length (compaction-result-kept-messages compact-result))
                                   'tokenCount token-count))))

  ;; 5. Extract cancellation token from config
  (define cancellation-tok
    (hash-ref cfg 'cancellation-token #f))

  ;; 5b. Dispatch 'model-select hook — extensions can override model
  (define-values (_model-hook-res model-hook-res)
    (maybe-dispatch-hooks (agent-session-extension-registry sess) 'model-select
                          (hasheq 'current-model (or (agent-session-model-name sess) "default"))))
  (when (and model-hook-res
             (eq? (hook-result-action model-hook-res) 'amend)
             (hash? (hook-result-payload model-hook-res))
             (hash-has-key? (hook-result-payload model-hook-res) 'model))
    (define override-model (hash-ref (hook-result-payload model-hook-res) 'model))
    (set-agent-session-model-name! sess override-model))

  ;; 6. Run the core agent loop with tool-call iteration
  (define final-result
    (with-handlers ([exn:fail?
                     (lambda (e)
                       ;; Emit runtime.error event
                       (emit-session-event! bus sid "runtime.error"
                                            (hasheq 'error (exn-message e)))
                       ;; Return error result instead of crashing
                       (make-loop-result context-with-system
                                         'error
                                         (hasheq 'error (exn-message e)
                                                 'errorType 'provider-error)))])
      (run-iteration-loop context-with-system prov bus reg
                          (agent-session-extension-registry sess)
                          log-path sid max-iterations
                          #:cancellation-token cancellation-tok
                          #:config cfg)))

  ;; 7. Rebuild index
  (set-agent-session-index! sess (build-index! log-path idx-path))

  ;; 8. Emit session.updated
  (emit-session-event! bus sid "session.updated"
                       (hasheq 'sessionId sid
                               'lastTurnTermination
                               (loop-result-termination-reason final-result)))

  (values sess final-result))

;; ============================================================
;; Iteration loop lives in iteration.rkt
;; ============================================================
;; run-iteration-loop is imported from iteration.rkt

;; ============================================================
;; Accessors
;; ============================================================

(define (session-id sess)
  (agent-session-session-id sess))

(define (session-history sess)
  (define log-path (session-log-path (agent-session-session-dir sess)))
  (if (file-exists? log-path)
      (load-session-log log-path)
      '()))

(define (session-active? sess)
  (agent-session-active? sess))

(define (close-session! sess)
  (emit-session-event! (agent-session-event-bus sess)
                       (agent-session-session-id sess)
                       "session.closed"
                       (hasheq 'sessionId (agent-session-session-id sess)))
  ;; Dispatch 'session-shutdown hook (R2-7: payload with session-id and duration)
  (define session-duration
    (- (now-seconds) (agent-session-start-time sess)))
  (define shutdown-payload
    (hasheq 'session-id (agent-session-session-id sess)
            'duration session-duration))
  (define-values (_shutdown-payload _shutdown-res)
    (maybe-dispatch-hooks (agent-session-extension-registry sess) 'session-shutdown shutdown-payload))
  (set-agent-session-active?! sess #f))
