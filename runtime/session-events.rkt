#lang racket/base

;; runtime/session-events.rkt — session event bus wiring
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05b).
;; Handles fork.requested and compact.requested events from TUI/CLI.

(require racket/contract
         "../agent/event-bus.rkt"
         "../runtime/session-store.rkt"
         "../runtime/compactor.rkt"
         "../runtime/session-index.rkt"
         (only-in "../util/protocol-types.rkt" event-ev event-payload message-id)
         (only-in "runtime-helpers.rkt" emit-session-event!)
         "session-types.rkt")
(require "session-mutation.rkt")
(require (submod "session-types.rkt" internal))
(require (only-in "context-assembly/state-inference.rkt"
                  infer-task-state-from-tools
                  current-state-inference-threshold))
(require (only-in "../util/fsm.rkt" fsm-state-name))

(provide (contract-out [wire-session-event-handlers! (-> agent-session? procedure? void?)]))

;; ============================================================
;; Event bus wiring
;; ============================================================

;; Wire event-bus subscribers for fork.requested and compact.requested events.
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

    ;; Subscribe to compact.requested — compact the session context
    (subscribe! bus
                (lambda (evt)
                  (with-handlers ([exn:fail? (lambda (e)
                                               (emit-session-event! bus
                                                                    (agent-session-session-id sess)
                                                                    "session.compact.failed"
                                                                    (hasheq 'error
                                                                            (exn-message e))))])
                    (define log-path (session-log-path-for sess))
                    (when (file-exists? log-path)
                      (define history (load-session-log log-path))
                      (when (and (not (null? history)) (not (agent-session-compacting? sess)))
                        (guarded-set-compacting! sess #t)
                        (define compact-result (compact-history history))
                        (guarded-set-compacting! sess #f)
                        (emit-session-event! bus
                                             (agent-session-session-id sess)
                                             "session.compact.completed"
                                             (hasheq 'removedCount
                                                     (compaction-result-removed-count compact-result)
                                                     'keptCount
                                                     (length (compaction-result-kept-messages
                                                              compact-result))))))))
                #:filter (lambda (evt) (equal? (event-ev evt) "compact.requested")))
    ;; Subscribe to tool.execution.completed — infer task state
    (subscribe!
     bus
     (lambda (evt)
       (define payload (event-payload evt))
       (define tool-name (and (hash? payload) (hash-ref payload 'tool-name #f)))
       (when tool-name
         ;; v0.75.6: Accumulate tool calls for better inference
         (define current-recent (agent-session-recent-tool-calls sess))
         (define updated-recent (append current-recent (list tool-name)))
         ;; Keep last 10 tool calls
         (set-agent-session-recent-tool-calls! sess
                                               (if (> (length updated-recent) 10)
                                                   (list-tail updated-recent
                                                              (- (length updated-recent) 10))
                                                   updated-recent))
         (define-values (inferred-state confidence)
           (infer-task-state-from-tools (agent-session-recent-tool-calls sess)))
         (when (and inferred-state (>= confidence (current-state-inference-threshold)))
           (guarded-set-task-fsm-state! sess (fsm-state-name inferred-state))
           ;; v0.75.6: Persist task state change
           (define log-path (session-log-path (agent-session-session-dir sess)))
           (when (file-exists? log-path)
             (append-task-state! log-path (fsm-state-name inferred-state)))
           (emit-session-event!
            bus
            (agent-session-session-id sess)
            "task.state.inferred"
            (hasheq 'state (fsm-state-name inferred-state) 'confidence confidence 'tool tool-name)))))
     #:filter (lambda (evt) (equal? (event-ev evt) "tool.execution.completed")))
    (void)))

;; ============================================================
;; Helpers
;; ============================================================
;; session-log-path, session-log-path-for imported from session-types.rkt (REV-05 DRY)
