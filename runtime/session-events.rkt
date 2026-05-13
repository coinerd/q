#lang racket/base

;; runtime/session-events.rkt — session event bus wiring
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05b).
;; Handles fork.requested and compact.requested events from TUI/CLI.

(require "../agent/event-bus.rkt"
         "../runtime/session-store.rkt"
         "../runtime/compactor.rkt"
         "../runtime/session-index.rkt"
         (only-in "../util/protocol-types.rkt" event-ev event-payload message-id)
         (only-in "runtime-helpers.rkt" emit-session-event!)
         "session-types.rkt")

(provide wire-session-event-handlers!)

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
    (subscribe!
     bus
     (lambda (evt)
       (with-handlers ([exn:fail? (lambda (e)
                                    (emit-session-event! bus
                                                         (agent-session-session-id sess)
                                                         "session.compact.failed"
                                                         (hasheq 'error (exn-message e))))])
         (define log-path (session-log-path-for sess))
         (when (file-exists? log-path)
           (define history (load-session-log log-path))
           (when (and (not (null? history)) (not (agent-session-compacting? sess)))
             (set-agent-session-compacting?! sess #t)
             (define compact-result (compact-history history))
             (set-agent-session-compacting?! sess #f)
             (emit-session-event! bus
                                  (agent-session-session-id sess)
                                  "session.compact.completed"
                                  (hasheq 'removedCount
                                          (compaction-result-removed-count compact-result)))))))
     #:filter (lambda (evt) (equal? (event-ev evt) "compact.requested")))))

;; ============================================================
;; Helpers
;; ============================================================
;; session-log-path, session-log-path-for imported from session-types.rkt (REV-05 DRY)
