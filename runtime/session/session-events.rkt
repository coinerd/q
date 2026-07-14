#lang racket/base

;; runtime/session-events.rkt — session event bus wiring
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05b).
;; Handles fork.requested and compact.requested events from TUI/CLI.

(require racket/contract
         "../../util/event/event-bus.rkt"
         "session-store.rkt"
         "../compaction/compactor.rkt"
         "../session-index/schema.rkt"
         "../session-index/mutations.rkt"
         "../session-index/query.rkt"
         (only-in "../../util/event/event.rkt" event-ev)
         (only-in "../../util/message/message.rkt" message-id message-kind)
         (only-in "../../util/event/event.rkt" event-payload)
         (only-in "../runtime-helpers.rkt" emit-session-event!)
         "session-types.rkt"
         (only-in "../context-assembly/task-conclusion.rkt" task-conclusion task-conclusion-id)
         (only-in "../memory/conclusion-bridge.rkt"
                  current-conclusion-to-memory-bridge-enabled
                  persist-high-value-conclusions!)
         (only-in "../memory/service.rkt" current-memory-backend)
         racket/set
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../context-assembly/session-walk.rkt" build-session-context)
         (only-in "../trace-logger.rkt" make-trace-logger start-trace-logger! stop-trace-logger!)
         "session-interruption.rkt"
         (only-in "../../util/event/event.rkt" make-event event-session-id event-turn-id))
(require "session-mutation.rkt")
(require (only-in "../context-assembly/state-inference.rkt"
                  infer-task-state-from-tools
                  current-state-inference-threshold))
(require (only-in "../../util/fsm/fsm.rkt" fsm-state-name))
;; LF1 fix: removed dead import current-ws-evolution-enabled? — never used in this file.
;; v0.77.10 M1: evolve-working-set-for-state import removed — subscriber now emits
;; context.ws-evolve-requested event for turn-orchestrator to handle.
;; (require (only-in "../context-assembly/ws-evolution.rkt" evolve-working-set-for-state))

(provide (contract-out
          [wire-session-event-handlers! (-> agent-session? procedure? void?)]
          [compact-session-durably!
           (->* (agent-session?)
                (#:request-id string? #:load-history procedure? #:compact-and-persist procedure?)
                symbol?)])
         current-mid-session-bridge-enabled
         current-mid-session-persisted-ids
         major-forward-transition?
         maybe-persist-mid-session!)

;; ============================================================
;; Event bus wiring
;; ============================================================

;; v0.97.5 GAP-F: Mid-session bridge on major forward transitions
(define current-mid-session-bridge-enabled (make-parameter #f))

;; GAP-E v0.97.11: Dedup set — tracks conclusion IDs already persisted mid-session
;; to avoid redundant backend writes on repeated forward transitions.
(define current-mid-session-persisted-ids (make-parameter (set)))

(define major-forward-transitions
  '((exploration . planning) (planning . implementation) (implementation . verification)))

(define (major-forward-transition? old-state new-state)
  (and (symbol? old-state)
       (symbol? new-state)
       (not (eq? old-state new-state))
       (pair? (member (cons old-state new-state) major-forward-transitions))))

(define (maybe-persist-mid-session! sess old-state new-state)
  (when (and (current-mid-session-bridge-enabled)
             (current-conclusion-to-memory-bridge-enabled)
             (major-forward-transition? old-state new-state))
    (define backend (current-memory-backend))
    (when backend
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-warning "mid-session bridge failed: ~a" (exn-message e)))])
        ;; GAP-E v0.97.11: Dedup — only persist conclusions not yet stored
        (define already-persisted (current-mid-session-persisted-ids))
        (define all-conclusions (agent-session-task-conclusions sess))
        (define new-conclusions
          (filter (lambda (c) (not (set-member? already-persisted (task-conclusion-id c))))
                  all-conclusions))
        (when (pair? new-conclusions)
          (persist-high-value-conclusions! new-conclusions
                                           #:backend backend
                                           #:session-id (agent-session-session-id sess))
          (current-mid-session-persisted-ids (for/fold ([s already-persisted])
                                                       ([c (in-list new-conclusions)])
                                               (set-add s (task-conclusion-id c)))))))))

;; Durable manual compaction lifecycle. Shared claim/release primitives prevent
;; manual and automatic compaction from owning the session guard concurrently.
(define (emit-compact-lifecycle! sess phase payload)
  (emit-session-event! (agent-session-event-bus sess) (agent-session-session-id sess) phase payload))

(define (effective-compaction-history sess durable-history)
  (if (and (agent-session-index sess)
           (for/or ([message (in-list durable-history)])
             (eq? (message-kind message) 'compaction-summary)))
      (build-session-context (agent-session-index sess))
      durable-history))

(define (stop-compact-tracer! tracer)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "compaction trace cleanup failed: ~a" (exn-message e)))])
    (stop-trace-logger! tracer)))

(define (compact-session-durably! sess
                                  #:request-id [request-id (generate-id)]
                                  #:load-history [load-history load-session-log]
                                  #:compact-and-persist [compact-proc compact-and-persist!])
  (define claimed? (try-claim-compaction! sess))
  (cond
    [(not claimed?)
     (define active-owner (if (agent-session-prompt-running? sess) 'prompt 'compaction))
     (emit-compact-lifecycle! sess
                              "session.compact.already-running"
                              (hasheq 'request-id
                                      request-id
                                      'persisted?
                                      #f
                                      'outcome
                                      'already-running
                                      'active-owner
                                      active-owner))
     'already-running]
    [else
     (define persisted? (box #f))
     (define tracer
       (make-trace-logger (agent-session-event-bus sess)
                          (agent-session-session-dir sess)
                          #:enabled? #t))
     (with-handlers ([exn:fail? (lambda (e)
                                  (emit-compact-lifecycle! sess
                                                           "session.compact.failed"
                                                           (hasheq 'request-id
                                                                   request-id
                                                                   'persisted?
                                                                   (unbox persisted?)
                                                                   'outcome
                                                                   'failed
                                                                   'error
                                                                   (exn-message e)))
                                  (release-compaction! sess)
                                  (stop-compact-tracer! tracer)
                                  'failed)])
       (start-trace-logger! tracer)
       (emit-compact-lifecycle! sess
                                "session.compact.started"
                                (hasheq 'request-id request-id 'persisted? #f 'outcome 'started))
       (define outcome
         (dynamic-wind void
                       (lambda ()
                         (define log-path (session-log-path-for sess))
                         (define durable-history (load-history log-path))
                         (define history (effective-compaction-history sess durable-history))
                         (cond
                           [(null? history)
                            (emit-compact-lifecycle! sess
                                                     "session.compact.nothing-to-compact"
                                                     (hasheq 'request-id
                                                             request-id
                                                             'persisted?
                                                             #f
                                                             'outcome
                                                             'nothing-to-compact
                                                             'before-count
                                                             0
                                                             'after-count
                                                             0))
                            'nothing-to-compact]
                           [else
                            (define compact-result (compact-proc history log-path))
                            (define removed (compaction-result-removed-count compact-result))
                            (define kept (length (compaction-result-kept-messages compact-result)))
                            (cond
                              [(zero? removed)
                               (emit-compact-lifecycle! sess
                                                        "session.compact.nothing-to-compact"
                                                        (hasheq 'request-id
                                                                request-id
                                                                'persisted?
                                                                #f
                                                                'outcome
                                                                'nothing-to-compact
                                                                'before-count
                                                                (length history)
                                                                'after-count
                                                                (length history)
                                                                'removed-count
                                                                0
                                                                'kept-count
                                                                kept))
                               'nothing-to-compact]
                              [else
                               (set-box! persisted? #t)
                               (define rebuilt-index
                                 (build-index! log-path
                                               (session-index-path (agent-session-session-dir sess))))
                               (define summary (compaction-result-summary-message compact-result))
                               (when summary
                                 (mark-active-leaf! rebuilt-index (message-id summary)))
                               (guarded-set-index! sess rebuilt-index)
                               (emit-compact-lifecycle! sess
                                                        "session.compact.completed"
                                                        (hasheq 'request-id
                                                                request-id
                                                                'persisted?
                                                                #t
                                                                'outcome
                                                                'completed
                                                                'before-count
                                                                (length history)
                                                                'after-count
                                                                (add1 kept)
                                                                'removed-count
                                                                removed
                                                                'kept-count
                                                                kept))
                               'completed])]))
                       (lambda () (release-compaction! sess))))
       (stop-compact-tracer! tracer)
       outcome)]))

;; Wire event-bus subscribers for fork.requested, compact.requested,
;; tool execution, conclusions, and state transitions.

;; v0.78.1 G1: current-ws-evolution-enabled? moved to state-aware-builder.rkt
;; to avoid cycle (session-config → session-events → session-store → ...)
;; These events are published by TUI/CLI commands and need runtime handlers.
(define (wire-session-event-handlers! sess fork-handler)
  (define bus (agent-session-event-bus sess))
  (when bus
    ;; Subscribe to fork.requested — delegate to fork-handler from agent-session
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
                    (define new-sess (fork-handler sess entry-id))
                    (emit-session-event! bus
                                         (agent-session-session-id sess)
                                         "session.fork.completed"
                                         (hasheq 'newSessionId
                                                 (agent-session-session-id new-sess)
                                                 'forkPoint
                                                 (or entry-id "latest")))))
                #:filter (lambda (evt) (equal? (event-ev evt) "fork.requested")))

    ;; Bind interruption to this session's exact active prompt turn. Unrelated
    ;; session requests are ignored so a shared bus cannot cross-cancel.
    (subscribe!
     bus
     (lambda (evt)
       ;; Defer handling until the original request reaches every observer so
       ;; structured traces preserve request-before-accepted ordering.
       (thread (lambda ()
                 (define payload (event-payload evt))
                 (define request-id (and (hash? payload) (hash-ref payload 'request-id #f)))
                 (define target-session-id
                   (and (hash? payload) (hash-ref payload 'target-session-id (event-session-id evt))))
                 (define target-turn-id
                   (and (hash? payload) (hash-ref payload 'target-turn-id (event-turn-id evt))))
                 (when (and (string? request-id) (string? target-session-id) (string? target-turn-id))
                   (define status
                     (request-session-interrupt! sess target-session-id target-turn-id request-id))
                   (unless (eq? status 'unrelated)
                     (define response-event
                       (make-event (case status
                                     [(accepted) "interrupt.accepted"]
                                     [(already-requested) "interrupt.already-requested"]
                                     [else "interrupt.no-active"])
                                   (current-inexact-milliseconds)
                                   target-session-id
                                   target-turn-id
                                   (hasheq 'request-id
                                           request-id
                                           'target-session-id
                                           target-session-id
                                           'target-turn-id
                                           target-turn-id
                                           'status
                                           (symbol->string status))))
                     (publish! bus response-event)
                     (when (eq? status 'accepted)
                       (with-handlers ([exn:fail? (lambda (error)
                                                    (log-warning "interrupt callback failed: ~a"
                                                                 (exn-message error)))])
                         (signal-session-interrupt! sess request-id))))))))
     #:filter (lambda (evt) (equal? (event-ev evt) "interrupt.requested")))

    ;; Run durable I/O off the synchronous event-bus/render path;
    ;; compact-session-durably! owns all correlated terminal events.
    (subscribe! bus
                (lambda (evt)
                  (define payload (event-payload evt))
                  (define request-id
                    (if (hash? payload)
                        (hash-ref payload 'request-id (generate-id))
                        (generate-id)))
                  (thread (lambda ()
                            (parameterize ([current-prompt-operation-session #f])
                              (compact-session-durably! sess #:request-id request-id)))))
                #:filter (lambda (evt)
                           (member (event-ev evt)
                                   '("session.compact.requested" "compact.requested"))))
    ;; Subscribe to tool.execution.completed — infer task state
    (subscribe! bus
                (lambda (evt)
                  (define payload (event-payload evt))
                  (define tool-name (and (hash? payload) (hash-ref payload 'tool-name #f)))
                  (when tool-name
                    ;; v0.75.6: Accumulate tool calls for better inference
                    (define current-recent (agent-session-recent-tool-calls sess))
                    (define updated-recent (append current-recent (list tool-name)))
                    ;; Keep last 10 tool calls
                    (guarded-set-recent-tool-calls! sess
                                                    (if (> (length updated-recent) 10)
                                                        (list-tail updated-recent
                                                                   (- (length updated-recent) 10))
                                                        updated-recent))
                    (define-values (inferred-state confidence)
                      (infer-task-state-from-tools (agent-session-recent-tool-calls sess)))
                    (when (and inferred-state (>= confidence (current-state-inference-threshold)))
                      (define old-state (agent-session-task-fsm-state sess))
                      (guarded-set-task-fsm-state! sess (fsm-state-name inferred-state))
                      ;; v0.97.5 GAP-F: Mid-session bridge on major forward transitions
                      (maybe-persist-mid-session! sess old-state (fsm-state-name inferred-state))
                      ;; v0.75.6: Persist task state change
                      (define log-path (session-log-path (agent-session-session-dir sess)))
                      (when (file-exists? log-path)
                        (append-task-state! log-path (fsm-state-name inferred-state)))
                      (emit-session-event! bus
                                           (agent-session-session-id sess)
                                           "task.state.inferred"
                                           (hasheq 'state
                                                   (fsm-state-name inferred-state)
                                                   'old-state
                                                   old-state
                                                   'confidence
                                                   confidence
                                                   'tool
                                                   tool-name)))))
                #:filter (lambda (evt) (equal? (event-ev evt) "tool.execution.completed")))
    ;; Subscribe to tool.record_conclusion.completed — persist conclusion
    (subscribe! bus
                (lambda (evt)
                  (define payload (event-payload evt))
                  (define text (and (hash? payload) (hash-ref payload 'text #f)))
                  (when text
                    (define current-state (or (agent-session-task-fsm-state sess) 'idle))
                    (define id (and (hash? payload) (hash-ref payload 'conclusion-id #f)))
                    (define category-raw (and (hash? payload) (hash-ref payload 'category "fact")))
                    (define tags (and (hash? payload) (hash-ref payload 'tags '())))
                    (define deps-raw (and (hash? payload) (hash-ref payload 'dependencies '())))
                    (define category-sym
                      (if (symbol? category-raw)
                          category-raw
                          (if (string? category-raw)
                              (string->symbol category-raw)
                              category-raw)))
                    (define tag-syms
                      (for/list ([t (in-list (if (list? tags)
                                                 tags
                                                 '()))])
                        (if (string? t)
                            (string->symbol t)
                            t)))
                    (define deps
                      (for/list ([d (in-list (if (list? deps-raw)
                                                 deps-raw
                                                 '()))])
                        (if (string? d)
                            d
                            (format "~a" d))))
                    ;; v0.76.7 C2: Extract origin-message-id from event payload
                    (define origin-id (and (hash? payload) (hash-ref payload 'origin-message-id #f)))
                    (define origin-ids
                      (if (and origin-id (string? origin-id) (not (string=? origin-id "")))
                          (list origin-id)
                          '()))
                    (define c
                      (task-conclusion (or id (format "c~a" (current-inexact-milliseconds)))
                                       text
                                       category-sym
                                       current-state
                                       origin-ids ; was: '() — now wired from tool event
                                       (current-seconds)
                                       tag-syms
                                       deps))
                    ;; Add to session
                    (define current-conclusions (agent-session-task-conclusions sess))
                    (guarded-set-task-conclusions! sess (append current-conclusions (list c)))
                    ;; Persist to log
                    (define log-path (session-log-path-for sess))
                    (when (file-exists? log-path)
                      (append-conclusion! log-path c))
                    ;; Emit confirmation
                    (emit-session-event! bus
                                         (agent-session-session-id sess)
                                         "task.conclusion.recorded"
                                         (hasheq 'conclusion-id
                                                 (task-conclusion-id c)
                                                 'category
                                                 category-sym
                                                 'state
                                                 current-state))))
                #:filter (lambda (evt) (equal? (event-ev evt) "tool.record_conclusion.completed")))

    ;; v0.76.7 C3: Subscribe to tool.set-task-state.completed — explicit state transition
    (subscribe!
     bus
     (lambda (evt)
       (define payload (event-payload evt))
       (define target (and (hash? payload) (hash-ref payload 'target-state #f)))
       (when (and target (or (string? target) (symbol? target)))
         (define target-sym
           (if (string? target)
               (string->symbol target)
               target))
         (define old-state (agent-session-task-fsm-state sess))
         (guarded-set-task-fsm-state! sess target-sym)
         ;; v0.97.5 GAP-F: Mid-session bridge on major forward transitions
         (maybe-persist-mid-session! sess old-state target-sym)
         ;; Persist task state change
         (define log-path (session-log-path-for sess))
         (when (file-exists? log-path)
           (append-task-state! log-path target-sym))
         (emit-session-event! bus
                              (agent-session-session-id sess)
                              "task.state.transitioned"
                              (hasheq 'state target-sym 'old-state old-state 'source "explicit"))))
     #:filter (lambda (evt) (equal? (event-ev evt) "tool.set-task-state.completed")))

    ;; v0.77.1 W1.3: WS evolution flag exported for turn-orchestrator wiring
    ;; (subscriber deferred — working-set lives in turn scope, not session)
    ;;
    ;; v0.78.6 W2: Removed orphaned context.ws-evolve-requested subscriber.
    ;; WS evolution is handled inline in turn-orchestrator.rkt's build-assembled-context.
    ;; The event-driven approach was abandoned because the subscriber in session-events
    ;; doesn't have access to the working-set (which lives in session config scope).

    ;; GAP-E v0.97.11: session.closed — persist all session conclusions on shutdown.
    ;; (Event name is "session.closed" from agent-session.rkt, not "session.ended".)
    ;; Belt-and-suspenders with close-session! in agent-session.rkt which also
    ;; calls persist-high-value-conclusions!. This handler catches cases where
    ;; close-session! is bypassed or the session ends via event bus.
    (subscribe!
     bus
     (lambda (evt)
       (when (and (current-mid-session-bridge-enabled) (current-conclusion-to-memory-bridge-enabled))
         (define backend (current-memory-backend))
         (when backend
           (with-handlers ([exn:fail? (lambda (e)
                                        (log-warning "session.ended bridge failed: ~a"
                                                     (exn-message e)))])
             (persist-high-value-conclusions! (agent-session-task-conclusions sess)
                                              #:backend backend
                                              #:session-id (agent-session-session-id sess))))))
     #:filter (lambda (evt) (equal? (event-ev evt) "session.closed")))

    (void))

  ;; ============================================================
  ;; Helpers
  ;; ============================================================
  ;; session-log-path, session-log-path-for imported from session-types.rkt (REV-05 DRY)
  )
