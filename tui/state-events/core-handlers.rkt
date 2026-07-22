#lang racket/base

;; tui/state-events/core-handlers.rkt -- Core event handlers for UI state
;; STABILITY: internal
;;
;; All non-goal event handlers + registration at module load time.
;;
;; W7 v0.99.35: Streaming, tool, and approval handlers extracted to
;; specialist modules that register their own reducers at load time.

(require racket/string
         racket/match
         racket/list
         (only-in "../../util/event/event.rkt"
                  event
                  event-ev
                  event-payload
                  event-time
                  event-session-id
                  event-turn-id
                  event?)
         "../state-types.rkt"
         (only-in "../../runtime/gsd-query.rkt" current-gsd-mode-query)
         ;; W7 v0.99.35: Pure helpers extracted from this module
         "handler-helpers.rkt"
         "helpers.rkt"
         "registry.rkt"
         ;; Specialist modules (registration side-effects at load time)
         "streaming-events.rkt"
         "tool-events.rkt"
         "approval-events.rkt")

;; ============================================================
;; Re-exports from specialist modules
;; ============================================================

(require (only-in "approval-events.rkt"
                  handle-spawn-approval-requested
                  handle-spawn-approval-terminal
                  approval-overlay-remove-request))

(provide verification-payload-ref
         handle-spawn-approval-requested
         handle-spawn-approval-terminal
         approval-overlay-remove-request)

;; ============================================================
;; Session / turn handlers
;; ============================================================

(define (event-for-current-session? state evt)
  (define state-session (ui-state-session-id state))
  (define event-session (event-session-id evt))
  (or (not state-session) (not event-session) (equal? state-session event-session)))

(define (event-for-active-turn? state evt)
  (define event-turn (event-turn-id evt))
  (define prompt-turn (ui-state-active-turn-id state))
  (define model-turn (ui-state-active-model-turn-id state))
  (or (not event-turn)
      (and (not prompt-turn) (not model-turn))
      (equal? event-turn prompt-turn)
      (equal? event-turn model-turn)))

(define (handle-session-started state evt)
  (define payload (event-payload evt))
  (define sid (hash-ref payload 'sessionId ""))
  (define s1 (struct-copy ui-state state [session-id sid]))
  (append-entry s1 (make-entry 'system (format "Session started: ~a" sid) (event-time evt) (hash))))

(define (handle-session-resumed state evt)
  (define payload (event-payload evt))
  (define sid (hash-ref payload 'sessionId ""))
  (define s1 (struct-copy ui-state state [session-id sid]))
  (append-entry s1 (make-entry 'system (format "Session resumed: ~a" sid) (event-time evt) (hash))))

(define (handle-turn-started state evt)
  (define payload (event-payload evt))
  (define prompt-scope?
    (and (hash? payload) (equal? (hash-ref payload 'scope #f) "prompt") (event-turn-id evt)))
  (cond
    [(not (event-for-current-session? state evt)) state]
    ;; Inner iteration starts must not erase pending feedback or partial output.
    [(and (ui-state-interrupt-request-id state) (not prompt-scope?)) state]
    [else
     (define started
       (set-status-message (clear-streaming (set-pending-tool-name
                                             (set-streaming-phase (set-busy-since (set-busy state #t)
                                                                                  (event-time evt))
                                                                  'thinking)
                                             #f))
                           #f))
     (cond
       [prompt-scope?
        (set-active-model-turn-id (set-active-turn-id (set-interrupt-request-id started #f)
                                                      (event-turn-id evt))
                                  #f)]
       [(event-turn-id evt) (set-active-model-turn-id started (event-turn-id evt))]
       [else started])]))

(define (handle-turn-completed state evt)
  (if (or (not (event-for-current-session? state evt))
          (not (event-for-active-turn? state evt))
          (ui-state-interrupt-request-id state))
      state
      (set-streaming-phase
       (clear-streaming (set-pending-tool-name (set-busy-since (set-busy state #f) #f) #f))
       'idle)))

(define (clear-after-turn-terminal state)
  (set-active-model-turn-id
   (set-active-turn-id (set-interrupt-request-id
                        (set-status-message (set-streaming-phase
                                             (clear-streaming (set-pending-tool-name
                                                               (set-busy-since (set-busy state #f) #f)
                                                               #f))
                                             'idle)
                                            #f)
                        #f)
                       #f)
   #f))

(define (handle-turn-cancelled state evt)
  (define pending-request-id (ui-state-interrupt-request-id state))
  (define payload (event-payload evt))
  (define acknowledged-request-id (and (hash? payload) (hash-ref payload 'request-id #f)))
  (define correlated?
    (and pending-request-id
         (equal? pending-request-id acknowledged-request-id)
         (equal? (ui-state-session-id state) (event-session-id evt))
         (equal? (ui-state-active-turn-id state) (event-turn-id evt))))
  (cond
    [correlated?
     (append-entry
      (clear-after-turn-terminal state)
      (make-entry 'system "[interrupt completed: turn cancelled]" (event-time evt) (hash)))]
    ;; While a user interrupt is pending, unrelated/inner cancellation events
    ;; must not clear the visible turn before its correlated acknowledgement.
    [pending-request-id state]
    [(and (event-for-current-session? state evt) (event-for-active-turn? state evt))
     (clear-after-turn-terminal state)]
    [else state]))

(define (handle-interrupt-terminal state evt)
  (define payload (event-payload evt))
  (define request-id (and (hash? payload) (hash-ref payload 'request-id #f)))
  (if (and request-id
           (equal? request-id (ui-state-interrupt-request-id state))
           (equal? (ui-state-session-id state) (event-session-id evt))
           (equal? (ui-state-active-turn-id state) (event-turn-id evt)))
      (append-entry (clear-after-turn-terminal state)
                    (make-entry 'system
                                (if (string=? (event-ev evt) "interrupt.failed")
                                    "[interrupt failed: turn did not cancel]"
                                    "[interrupt: no matching active turn]")
                                (event-time evt)
                                (hash)))
      state))

(define (handle-stream-turn-completed state evt)
  (cond
    [(or (not (event-for-current-session? state evt))
         (not (event-for-active-turn? state evt))
         (ui-state-interrupt-request-id state))
     state]
    [else
     (define cleared
       (set-streaming-phase
        (clear-streaming (set-pending-tool-name (set-busy-since (set-busy state #f) #f) #f))
        'idle))
     (define goal (ui-state-active-goal cleared))
     (if (and goal (eq? (goal-display-info-status goal) 'active))
         (set-busy (set-status-message cleared
                                       (format "Goal turn ~a/~a: evaluating..."
                                               (goal-display-info-turns-used goal)
                                               (goal-display-info-max-turns goal)))
                   #t)
         cleared)]))

;; ============================================================
;; Error / compaction handlers
;; ============================================================

(define (handle-runtime-error state evt)
  (cond
    [(or (not (event-for-current-session? state evt))
         (not (event-for-active-turn? state evt))
         (ui-state-interrupt-request-id state))
     state]
    [else
     (define payload (event-payload evt))
     (define err (hash-ref payload 'error "unknown error"))
     (define ts (event-time evt))
     (define error-type (classify-error-type err payload))
     (define retries-attempted (hash-ref payload 'retries-attempted #f))
     (define error-history (hash-ref payload 'errorHistory '()))
     (define history-types (remove-duplicates error-history))
     (define hint (format-error-hint error-type retries-attempted history-types))
     (define streamed (ui-state-streaming-text state))
     (define s0
       (if (and streamed (> (string-length (string-trim streamed)) 0))
           (append-entry state (make-entry 'assistant streamed ts (hasheq 'partial #t)))
           state))
     (define s1
       (set-status-message (clear-streaming (set-pending-tool-name (set-busy s0 #f) #f))
                           (truncate-status-msg err)))
     (define s2 (append-entry s1 (make-entry 'error (format "Error: ~a" err) ts (hash))))
     (append-entry s2 (make-entry 'system hint ts (hash)))]))

(define (handle-compaction-warning state evt)
  (define payload (event-payload evt))
  (define tc (hash-ref payload 'tokenCount "?"))
  (append-entry
   state
   (make-entry 'system (format "[compaction warning: ~a tokens]" tc) (event-time evt) (hash))))

(define (handle-session-forked state evt)
  (define payload (event-payload evt))
  (define new-sid (hash-ref payload 'newSessionId ""))
  (append-entry state
                (make-entry 'system (format "[session forked: ~a]" new-sid) (event-time evt) (hash))))

(define (handle-compaction state evt)
  (define payload (event-payload evt))
  (define reason (hash-ref payload 'reason ""))
  (if (string=? reason "compaction-complete")
      (set-status-message state #f)
      (set-status-message state "Compacting...")))

(define (handle-injection state evt)
  (define payload (event-payload evt))
  (define content-type (hash-ref payload 'content-type "unknown"))
  (define msg (format "[injected ~a message]" content-type))
  (clear-streaming (append-entry state (make-entry 'system msg (event-time evt) (hash)))))

(define (handle-queue-status-update state evt)
  (define payload (event-payload evt))
  (struct-copy ui-state state [queue-counts payload]))

(define (handle-iteration-soft-warning state evt)
  (define payload (event-payload evt))
  (define iter (hash-ref payload 'iteration "?"))
  (define remaining (hash-ref payload 'remaining "?"))
  (define label (if (eq? ((current-gsd-mode-query)) 'executing) "executing" "exploring"))
  (append-entry
   state
   (make-entry 'system
               (format "[~a... iteration ~a, ~a remaining before hard stop]" label iter remaining)
               (event-time evt)
               (hash))))

(define (handle-exploration-progress state evt)
  (define payload (event-payload evt))
  (define count (hash-ref payload 'consecutive-tools "?"))
  (define tool-names (hash-ref payload 'tool-names '()))
  (define label (if (eq? ((current-gsd-mode-query)) 'executing) "executing" "exploring"))
  (append-entry state
                (make-entry 'system
                            (format "[~a... ~a tool calls: ~a]"
                                    label
                                    count
                                    (string-join (map (lambda (s)
                                                        (if (string? s)
                                                            s
                                                            (format "~a" s)))
                                                      tool-names)
                                                 ", "))
                            (event-time evt)
                            (hash))))

(define (handle-gsd-plan-archived state evt)
  (define payload (event-payload evt))
  (define path (hash-ref payload 'path "?"))
  (append-entry state (make-entry 'system (format "[archived] ~a" path) (event-time evt) (hash))))

;; ============================================================
;; Verification event handlers (W6 v0.99.5)
;; ============================================================

(define (handle-verification-started state evt)
  (define artifact-count (verification-payload-ref evt 'artifact-count 0))
  (set-status-message state (format "Verifying ~a artifacts..." artifact-count)))

(define (handle-verification-completed state evt)
  (define verdict (verification-payload-ref evt 'verdict "unknown"))
  (define reason (verification-payload-ref evt 'reason #f))
  (match verdict
    ;; Approval: clear status message
    [(or "approve" 'approve) (set-status-message state #f)]
    [(or "reject" 'reject)
     ;; Rejection: add transcript entry with reason
     (append-entry state
                   (make-entry 'system
                               (format "[Verification: REJECTED~a]"
                                       (if reason
                                           (format " — ~a" reason)
                                           ""))
                               (event-time evt)
                               (hasheq 'verification #t 'rejected #t)))]
    [else
     ;; Escalate or unknown: add generic transcript entry
     (append-entry state
                   (make-entry 'system
                               (format "[Verification: ~a~a]"
                                       verdict
                                       (if reason
                                           (format " — ~a" reason)
                                           ""))
                               (event-time evt)
                               (hasheq 'verification #t)))]))

(define (handle-verification-escalated state evt)
  (define reason (verification-payload-ref evt 'reason "unknown"))
  (define risk-level (verification-payload-ref evt 'risk-level "unknown"))
  (append-entry (set-status-message state "Verification requires approval")
                (make-entry 'system
                            (format "[Verification: ~a — risk: ~a]" reason risk-level)
                            (event-time evt)
                            (hasheq 'verification #t 'escalation #t))))

(define (handle-context-pressure state evt)
  (define payload (event-payload evt))
  (struct-copy ui-state
               state
               [context-pressure-level (hash-ref payload 'level #f)]
               [context-pressure-percent (hash-ref payload 'usage-percent #f)]))

(define (handle-context-mid-turn-over-budget state evt)
  (define payload (event-payload evt))
  (define estimated (hash-ref payload 'estimated-tokens "?"))
  (define budget (hash-ref payload 'budget "?"))
  (append-entry state
                (make-entry 'system
                            (format "[context growing: ~a/~a tokens used]" estimated budget)
                            (event-time evt)
                            (hash))))

(define (handle-compaction-lifecycle state evt)
  (define ev (event-ev evt))
  (define payload (event-payload evt))
  (define ts (event-time evt))
  (define evt-request-id (and (hash? payload) (hash-ref payload 'request-id #f)))
  (define pending-compact (ui-state-compact-request-id state))
  ;; F-06: A lifecycle event is correlated if its request-id matches the
  ;; pending compact request, or if no pending request exists (legacy /
  ;; automatic compaction paths that predate correlation).  Foreign or
  ;; stale terminals must not clear the status or report completion.
  (define correlated?
    (and (event-for-current-session? state evt)
         (or (not pending-compact) (and evt-request-id (equal? evt-request-id pending-compact)))))
  (define (clear-compact-pending! s)
    (set-compact-request-id (set-status-message s #f) #f))
  (define (terminal message)
    (if correlated?
        (append-entry (clear-compact-pending! state) (make-entry 'system message ts (hash)))
        state))
  (match ev
    [(or "session.compact.started" "compaction.started" "compaction.start")
     (if correlated?
         (set-status-message state "Compacting...")
         state)]
    ["session.compact.completed"
     (define removed (hash-ref-multi payload 'removed-count 'removedCount "?"))
     (define kept (hash-ref-multi payload 'kept-count 'keptCount "?"))
     (terminal (format "[compact completed: removed ~a, kept ~a]" removed kept))]
    ["session.compact.nothing-to-compact" (terminal "[compact: nothing to compact]")]
    ["session.compact.already-running"
     (if (eq? (hash-ref payload 'active-owner 'compaction) 'prompt)
         (terminal "[compact: active prompt is blocking compaction]")
         ;; Another compaction still owns the guard; retain its active status.
         (if correlated?
             (append-entry state (make-entry 'system "[compact: already running]" ts (hash)))
             state))]
    ["session.compact.failed"
     (define prefix
       (if (hash-ref payload 'persisted? #f)
           "compact failed after summary persistence"
           "compact failed"))
     (terminal (format "[~a: ~a]" prefix (hash-ref payload 'error "unknown error")))]
    [(or "compaction.completed" "compaction.end")
     ;; Legacy automatic-compaction names — clear status only when correlated.
     (if correlated?
         (clear-compact-pending! state)
         state)]
    [_ state]))

;; ============================================================
;; Register core handlers at module load time
;; ============================================================

;; M2 fix (v0.99.6): verification-payload-ref is provided from handler-helpers.rkt
;; and re-exported here. Approval handler exports come from approval-events.rkt.

(register-event-reducer! "runtime.error" handle-runtime-error)
(register-event-reducer! "session.started" handle-session-started)
(register-event-reducer! "session.resumed" handle-session-resumed)
(register-event-reducer! "turn.started" handle-turn-started)
(register-event-reducer! "turn.completed" handle-turn-completed)
(register-event-reducer! "turn.cancelled" handle-turn-cancelled)
(register-event-reducer! "interrupt.no-active" handle-interrupt-terminal)
(register-event-reducer! "interrupt.failed" handle-interrupt-terminal)
(register-event-reducer! "compaction.warning" handle-compaction-warning)
(register-event-reducer! "session.forked" handle-session-forked)
(register-event-reducer! "compaction" handle-compaction)
(register-event-reducer! "injection" handle-injection)
(register-event-reducer! "queue.status-update" handle-queue-status-update)
(register-event-reducer! "iteration.soft-warning" handle-iteration-soft-warning)
(register-event-reducer! "exploration.progress" handle-exploration-progress)
(register-event-reducer! "gsd.plan.archived" handle-gsd-plan-archived)
(register-event-reducer! "context.mid-turn-over-budget" handle-context-mid-turn-over-budget)
(register-event-reducer! "session.compact.started" handle-compaction-lifecycle)
(register-event-reducer! "session.compact.completed" handle-compaction-lifecycle)
(register-event-reducer! "session.compact.nothing-to-compact" handle-compaction-lifecycle)
(register-event-reducer! "session.compact.already-running" handle-compaction-lifecycle)
(register-event-reducer! "session.compact.failed" handle-compaction-lifecycle)
;; Legacy SDK/automatic-compaction names remain read-compatible at the UI boundary.
(register-event-reducer! "compaction.started" handle-compaction-lifecycle)
(register-event-reducer! "compaction.start" handle-compaction-lifecycle)
(register-event-reducer! "compaction.completed" handle-compaction-lifecycle)
(register-event-reducer! "compaction.end" handle-compaction-lifecycle)
(register-event-reducer! "stream.turn.completed" handle-stream-turn-completed)
(register-event-reducer! "context.pressure" handle-context-pressure)
(register-event-reducer! "gsd.verification.started" handle-verification-started)
(register-event-reducer! "gsd.verification.completed" handle-verification-completed)
(register-event-reducer! "gsd.verification.escalated" handle-verification-escalated)
