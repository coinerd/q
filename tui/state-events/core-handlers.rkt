#lang racket/base

;; tui/state-events/core-handlers.rkt -- Core event handlers for UI state
;; STABILITY: internal
;;
;; All non-goal event handlers + registration at module load time.

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
         (only-in "../../util/message/message.rkt" message)
         "../../util/cost-tracker.rkt"
         "../../util/content/content-helpers.rkt"
         "../state-types.rkt"
         (only-in "../../runtime/gsd-query.rkt" current-gsd-mode-query)
         (only-in "../render/message-layout.rkt" plain-line)
         (only-in "../approval-channel.rkt" approval-request-view approval-request-pending?)
         ;; W7 v0.99.35: Pure helpers extracted from this module
         "handler-helpers.rkt"
         "helpers.rkt"
         "registry.rkt")

;; ============================================================
;; Message / streaming handlers
;; ============================================================

(define (handle-assistant-message-completed state evt)
  (define payload (event-payload evt))
  (define streamed (ui-state-streaming-text state))
  (define content (or streamed (hash-ref payload 'content "")))
  (define ts (event-time evt))
  (define thinking (ui-state-streaming-thinking state))
  (define s0
    (if (and thinking
             (> (string-length (string-trim thinking)) 0)
             (string=? (string-trim content) ""))
        (append-entry state (make-entry 'thinking thinking ts (hash)))
        state))
  (clear-streaming (set-pending-tool-name
                    (set-busy (append-entry s0 (make-entry 'assistant content ts (hash))) #f)
                    #f)))

(define (handle-model-stream-delta state evt)
  (define payload (event-payload evt))
  (define delta (hash-ref payload 'delta ""))
  (define current-streaming (ui-state-streaming-text state))
  (define new-streaming (string-append (or current-streaming "") delta))
  ;; BF1b (v0.99.4): Record delta timestamp for streaming stall watchdog
  (define now (current-inexact-milliseconds))
  (set-last-delta-ms (set-streaming-phase (set-streaming-text (set-busy state #t) new-streaming)
                                          'streaming)
                     now))

(define (handle-model-stream-thinking state evt)
  (define payload (event-payload evt))
  (define delta (hash-ref payload 'delta ""))
  (define current-thinking (ui-state-streaming-thinking state))
  (define new-thinking (string-append (or current-thinking "") delta))
  ;; BF1b (v0.99.4): Record thinking timestamp for streaming stall watchdog
  (define now (current-inexact-milliseconds))
  (set-last-delta-ms (set-streaming-thinking (set-busy state #t) new-thinking) now))

(define (handle-model-stream-completed state evt)
  (define payload (event-payload evt))
  (define raw-usage (and (hash? payload) (hash-ref payload 'usage (hasheq))))
  (define usage
    (if (hash? raw-usage)
        raw-usage
        (hasheq)))
  (define in-tok (hash-ref usage 'prompt_tokens (hash-ref usage 'input_tokens 0)))
  (define out-tok (hash-ref usage 'completion_tokens (hash-ref usage 'output_tokens 0)))
  (define ct (ui-state-cost-tracker state))
  (when (and ct (or (positive? in-tok) (positive? out-tok)))
    (cost-tracker-update! ct in-tok out-tok (ui-model-label state)))
  (clear-streaming state))

(define (handle-model-request-started state evt)
  (set-busy state #t))

(define (handle-context-built state evt)
  (define payload (event-payload evt))
  (define tok
    (and (hash? payload) (or (hash-ref payload 'tokenCount #f) (hash-ref payload 'token-count #f))))
  (if tok
      (struct-copy ui-state state [context-tokens tok])
      state))

;; ============================================================
;; Tool handlers
;; ============================================================

(define (handle-tool-call-started state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'name "?"))
  ;; MF2 (v0.99.5): Clear stale streaming text when transitioning to tool
  ;; execution. Without this, old streaming-text triggers false watchdog
  ;; stall detection during tool calls.
  (define cleared-stream (clear-streaming state))
  (if (recent-tool-start? state name)
      (set-streaming-phase (set-pending-tool-name (set-busy cleared-stream #t) name) 'tool-pending)
      (let* ([args-raw (hash-ref payload 'arguments #f)]
             [arg-summary (if args-raw
                              (extract-arg-summary args-raw)
                              "")]
             [text arg-summary]
             [ts (event-time evt)]
             [meta (hasheq 'name name 'arguments (or args-raw ""))]
             [new-state (append-entry cleared-stream (make-entry 'tool-start text ts meta))])
        (if (ui-state-pending-tool-name state)
            (set-busy new-state #t)
            (set-pending-tool-name (set-busy new-state #t) name)))))

(define (handle-tool-execution-started state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'toolName "?"))
  (if (recent-tool-start? state name)
      (set-streaming-phase (set-pending-tool-name (set-busy state #t) name) 'tool-pending)
      (let* ([args-raw (hash-ref payload 'arguments #f)]
             [arg-summary (if args-raw
                              (extract-arg-summary args-raw)
                              "")]
             [text arg-summary]
             [ts (event-time evt)]
             [meta (hasheq 'name name 'arguments (or args-raw ""))]
             [new-state (append-entry state (make-entry 'tool-start text ts meta))])
        (if (ui-state-pending-tool-name state)
            (set-busy new-state #t)
            (set-pending-tool-name (set-busy new-state #t) name)))))

(define (handle-tool-execution-completed state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'toolName (lambda () (hash-ref payload 'name "?"))))
  (define result-summary
    (hash-ref payload 'resultSummary (lambda () (if (hash-ref payload 'error #f) 'error 'completed))))
  (define result-raw (hash-ref payload 'result #f))
  (define error-raw (or (hash-ref payload 'resultError #f) (hash-ref payload 'error #f)))
  (define ts (event-time evt))
  (if (recent-tool-end? state name)
      (set-pending-tool-name state #f)
      (if (eq? result-summary 'completed)
          (let* ([result-text
                  (if result-raw
                      (string-replace (truncate-string (tool-result-content->string result-raw) 80)
                                      "\n"
                                      " | ")
                      "")]
                 [text result-text]
                 [meta (hasheq 'name name 'result (or result-raw ""))])
            (set-pending-tool-name (append-entry state (make-entry 'tool-end text ts meta)) #f))
          (let* ([err (or error-raw "tool failed")]
                 [text (string-replace err "\n" " | ")]
                 [meta (hasheq 'name name 'error err)])
            (set-pending-tool-name (append-entry state (make-entry 'tool-fail text ts meta)) #f)))))

;; B3 fix: Show progress during long-running tool batches
(define (handle-tool-execution-update state evt)
  (define payload (event-payload evt))
  (define tool-name (hash-ref payload 'toolName "?"))
  (define progress (hash-ref payload 'progress (hasheq)))
  (define total (hash-ref progress 'total 0))
  (define running (hash-ref progress 'running 0))
  (define status-text (tool-progress-status-text tool-name total running))
  (set-status-message state status-text))

(define (handle-tool-call-blocked state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'name "?"))
  (define reason (hash-ref payload 'reason "blocked by extension"))
  (set-pending-tool-name (append-entry state
                                       (make-entry 'system
                                                   (format "[tool blocked: ~a -- ~a]" name reason)
                                                   (event-time evt)
                                                   (hasheq 'name name)))
                         #f))

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
;; Error / compaction / retry handlers
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

(define (handle-auto-retry state evt)
  (define payload (event-payload evt))
  (define attempt (hash-ref payload 'attempt "?"))
  (define max-attempts (hash-ref payload 'max-attempts "?"))
  (define error-type (hash-ref payload 'error-type #f))
  (define type-label (retry-error-type-label error-type))
  (define msg
    (if type-label
        (format "[retry: ~a, ~a/~a...]" type-label attempt max-attempts)
        (format "[retry: attempt ~a/~a]" attempt max-attempts)))
  (clear-streaming (append-entry state (make-entry 'system msg (event-time evt) (hash)))))

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

;; W7 v0.99.35: kebab->camel, hash-ref-multi, verification-payload-ref
;; extracted to handler-helpers.rkt (imported at top).

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
    (or (not pending-compact) (and evt-request-id (equal? evt-request-id pending-compact))))
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

(define (handle-auto-retry-lifecycle state evt)
  (define ev (event-ev evt))
  (define payload (event-payload evt))
  (match ev
    ["auto-retry.start"
     (define attempt (hash-ref payload 'attempt "?"))
     (define max-attempts (hash-ref payload 'maxRetries "?"))
     (define error-type (hash-ref payload 'errorType #f))
     (define type-label (retry-error-type-label error-type))
     (define msg
       (if type-label
           (format "[retry: ~a, ~a/~a...]" type-label attempt max-attempts)
           (format "[retry: attempt ~a/~a]" attempt max-attempts)))
     (clear-streaming (append-entry state (make-entry 'system msg (event-time evt) (hash))))]
    ["auto-retry.context-reduced"
     (define original (hash-ref payload 'original-messages 0))
     (define reduced (hash-ref payload 'reduced-messages 0))
     (append-entry state
                   (make-entry 'system
                               (format "[retry: reduced context ~a -> ~a messages]" original reduced)
                               (event-time evt)
                               (hash)))]
    [_ state]))

;; ============================================================
;; HITL Approval handler (v0.99.25 §5.3)
;; ============================================================

;; Approval requests stay in the overlay's extra field to preserve the ui-state
;; and overlay-state ABIs.  The active request keeps the historical extra keys;
;; queued requests are normalized hashes under 'approval-queue.
(define (approval-request-from-payload payload)
  (and (hash? payload)
       (let* ([request-id (hash-ref payload 'request-id #f)]
              [commitment-digest (hash-ref payload 'commitment-digest #f)]
              [event-presentation-digest (hash-ref payload 'presentation-digest #f)]
              [authoritative-view (and (string? request-id)
                                       (string? commitment-digest)
                                       (string? event-presentation-digest)
                                       (approval-request-view request-id commitment-digest))]
              [authoritative-presentation-digest
               (and authoritative-view (hash-ref authoritative-view 'presentation-digest #f))])
         ;; Event-carried previews are untrusted. Correlation and both digests
         ;; must match before the broker-owned immutable view is displayed.
         (and authoritative-view
              (string? authoritative-presentation-digest)
              (string=? event-presentation-digest authoritative-presentation-digest)
              (let* ([raw-capabilities (hash-ref authoritative-view 'capabilities '())]
                     [capabilities (if (list? raw-capabilities)
                                       raw-capabilities
                                       '())]
                     [raw-preview (hash-ref authoritative-view 'task-preview "")]
                     [task-preview (if (string? raw-preview)
                                       raw-preview
                                       (format "~a" raw-preview))])
                (hasheq 'request-id
                        request-id
                        'commitment-digest
                        commitment-digest
                        'presentation-digest
                        authoritative-presentation-digest
                        'approval-view
                        authoritative-view
                        'capabilities
                        capabilities
                        'task-preview
                        task-preview))))))

(define (approval-request-id request)
  (and (hash? request) (hash-ref request 'request-id #f)))

(define (approval-overlay-queue ov)
  (define extra (overlay-state-extra ov))
  (define queue (and (hash? extra) (hash-ref extra 'approval-queue '())))
  (if (list? queue)
      queue
      '()))

(define (approval-overlay-seen-ids ov)
  (define extra (overlay-state-extra ov))
  (define stored (and (hash? extra) (hash-ref extra 'approval-seen-request-ids #f)))
  (if (list? stored)
      stored
      (filter string?
              (cons (and (hash? extra) (hash-ref extra 'request-id #f))
                    (map approval-request-id (approval-overlay-queue ov))))))

(define (approval-detail-lines view)
  (define (detail label key [default ""])
    (plain-line (format "  ~a: ~a" label (hash-ref view key default))))
  (define common
    (list (detail "Plan" 'plan-kind)
          (detail "Model" 'model-preview)
          (detail "Tools" 'effective-tools '())
          (detail "Working directory" 'cwd-preview)
          (detail "Provider" 'provider-preview)
          (detail "Safe mode" 'safe-mode #f)
          (detail "Max turns" 'max-turns)
          (detail "Parent session" 'parent-session-preview)
          (detail "Parent call" 'parent-call-preview)
          (detail "Tool call" 'tool-call-id)
          (detail "Child" 'child-id)
          (detail "Child session" 'session-id)))
  (define jobs (hash-ref view 'jobs '()))
  (append
   common
   (if (list? jobs)
       (for/list ([job (in-list jobs)])
         (plain-line
          (format "  Job ~a [~a]: model=~a turns=~a caps=~a tools=~a call=~a child=~a session=~a"
                  (hash-ref job 'batch-order "?")
                  (hash-ref job 'job-id "?")
                  (hash-ref job 'model-preview "")
                  (hash-ref job 'max-turns "")
                  (hash-ref job 'effective-capabilities '())
                  (hash-ref job 'effective-tools '())
                  (hash-ref job 'tool-call-id "")
                  (hash-ref job 'child-id "")
                  (hash-ref job 'session-id ""))))
       '())))

(define (approval-request-overlay request queue seen-ids)
  (define capabilities (hash-ref request 'capabilities '()))
  (define task-preview (hash-ref request 'task-preview ""))
  (define caps-str
    (string-join (map (lambda (capability)
                        (if (symbol? capability)
                            (symbol->string capability)
                            (format "~a" capability)))
                      capabilities)
                 ", "))
  (define preview-lines
    (let ([lines (string-split task-preview "\n" #:trim? #f)])
      (if (null? lines)
          (list "")
          lines)))
  (define authoritative-view (hash-ref request 'approval-view (hasheq)))
  (define content
    (append (list (plain-line "\u26a1 Subagent Approval Required")
                  (plain-line (format "  Capabilities: ~a" caps-str)))
            (for/list ([line (in-list preview-lines)]
                       [index (in-naturals)])
              (plain-line (format "  ~a: ~a" (if (zero? index) "Task" "    ") line)))
            (approval-detail-lines authoritative-view)
            (list (plain-line "") (plain-line "  [y] Approve   [n] Deny   [Esc] Cancel"))))
  (overlay-state 'approval-prompt
                 content
                 ""
                 'top-left
                 #f
                 #f
                 0
                 (hasheq 'capabilities
                         capabilities
                         'task-preview
                         task-preview
                         'request-id
                         (hash-ref request 'request-id)
                         'commitment-digest
                         (hash-ref request 'commitment-digest)
                         'presentation-digest
                         (hash-ref request 'presentation-digest "")
                         'approval-view
                         authoritative-view
                         'approval-queue
                         queue
                         'approval-seen-request-ids
                         seen-ids)))

;; Remove exactly one correlated request.  If it is active, promote the next
;; queued request in FIFO order; if it is queued, leave the active request in
;; place.  This helper is shared with the key handler so a failed/stale channel
;; delivery can never dismiss a newer prompt.
(define (approval-overlay-remove-request state request-id)
  (define ov (ui-state-active-overlay state))
  (cond
    [(not (and (string? request-id) ov (eq? (overlay-state-type ov) 'approval-prompt))) state]
    [else
     (define extra (overlay-state-extra ov))
     (define active-id (and (hash? extra) (hash-ref extra 'request-id #f)))
     (define queue (approval-overlay-queue ov))
     (define seen-ids (approval-overlay-seen-ids ov))
     (cond
       [(equal? request-id active-id)
        (if (null? queue)
            (struct-copy ui-state state [active-overlay #f])
            (struct-copy ui-state
                         state
                         [active-overlay
                          (approval-request-overlay (car queue) (cdr queue) seen-ids)]))]
       [(ormap (lambda (request) (equal? request-id (approval-request-id request))) queue)
        (define remaining
          (filter (lambda (request) (not (equal? request-id (approval-request-id request)))) queue))
        (define new-extra (hash-set extra 'approval-queue remaining))
        (struct-copy ui-state
                     state
                     [active-overlay (struct-copy overlay-state ov [extra new-extra])])]
       [else state])]))

;; Handle a correlated spawn approval request.  Existing approval prompts are
;; never replaced: new requests queue FIFO and duplicate deliveries are ignored.
(define (handle-spawn-approval-requested state evt)
  (define request (approval-request-from-payload (event-payload evt)))
  (cond
    [(not request) state]
    [else
     (define request-id (approval-request-id request))
     (define ov (ui-state-active-overlay state))
     (cond
       [(and ov (eq? (overlay-state-type ov) 'approval-prompt))
        (define seen-ids (approval-overlay-seen-ids ov))
        (if (member request-id seen-ids)
            state
            (let* ([queue (approval-overlay-queue ov)]
                   [old-extra (overlay-state-extra ov)]
                   [base-extra (if (hash? old-extra)
                                   old-extra
                                   (hasheq))]
                   [new-extra
                    (hash-set (hash-set base-extra 'approval-queue (append queue (list request)))
                              'approval-seen-request-ids
                              (append seen-ids (list request-id)))])
              (struct-copy ui-state
                           state
                           [active-overlay (struct-copy overlay-state ov [extra new-extra])])))]
       [else
        (struct-copy ui-state
                     state
                     [active-overlay (approval-request-overlay request '() (list request-id))])])]))

(define (approval-overlay-request-digest state request-id)
  (define overlay (ui-state-active-overlay state))
  (and overlay
       (eq? (overlay-state-type overlay) 'approval-prompt)
       (let* ([extra (overlay-state-extra overlay)]
              [active-id (and (hash? extra) (hash-ref extra 'request-id #f))]
              [active-digest (and (hash? extra) (hash-ref extra 'commitment-digest #f))])
         (if (equal? request-id active-id)
             active-digest
             (for/or ([request (in-list (approval-overlay-queue overlay))])
               (and (equal? request-id (approval-request-id request))
                    (hash-ref request 'commitment-digest #f)))))))

(define (handle-spawn-approval-terminal state evt)
  (define payload (event-payload evt))
  (define request-id (and (hash? payload) (hash-ref payload 'request-id #f)))
  (define commitment-digest (and (hash? payload) (hash-ref payload 'commitment-digest #f)))
  (define displayed-digest
    (and (string? request-id) (approval-overlay-request-digest state request-id)))
  (if (and (string? commitment-digest)
           (equal? commitment-digest displayed-digest)
           (not (approval-request-pending? request-id commitment-digest)))
      (approval-overlay-remove-request state request-id)
      state))

;; ============================================================
;; Register all core handlers at module load time
;; ============================================================

;; M2 fix (v0.99.6): Exported for testing.
(provide verification-payload-ref
         handle-spawn-approval-requested
         handle-spawn-approval-terminal
         approval-overlay-remove-request)

(register-event-reducer! "assistant.message.completed" handle-assistant-message-completed)
(register-event-reducer! "tool.call.started" handle-tool-call-started)
(register-event-reducer! "tool.execution.started" handle-tool-execution-started)
(register-event-reducer! "tool.execution.completed" handle-tool-execution-completed)
(register-event-reducer! "tool.execution.updated" handle-tool-execution-update)
(register-event-reducer! "runtime.error" handle-runtime-error)
(register-event-reducer! "session.started" handle-session-started)
(register-event-reducer! "session.resumed" handle-session-resumed)
(register-event-reducer! "model.stream.delta" handle-model-stream-delta)
(register-event-reducer! "turn.started" handle-turn-started)
(register-event-reducer! "turn.completed" handle-turn-completed)
(register-event-reducer! "turn.cancelled" handle-turn-cancelled)
(register-event-reducer! "interrupt.no-active" handle-interrupt-terminal)
(register-event-reducer! "interrupt.failed" handle-interrupt-terminal)
(register-event-reducer! "compaction.warning" handle-compaction-warning)
(register-event-reducer! "session.forked" handle-session-forked)
(register-event-reducer! "compaction" handle-compaction)
(register-event-reducer! "auto-retry" handle-auto-retry)
(register-event-reducer! "injection" handle-injection)
(register-event-reducer! "model.request.started" handle-model-request-started)
(register-event-reducer! "context.built" handle-context-built)
(register-event-reducer! "tool.call.blocked" handle-tool-call-blocked)
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
(register-event-reducer! "auto-retry.start" handle-auto-retry-lifecycle)
(register-event-reducer! "auto-retry.context-reduced" handle-auto-retry-lifecycle)
(register-event-reducer! "model.stream.thinking" handle-model-stream-thinking)
(register-event-reducer! "model.stream.completed" handle-model-stream-completed)
(register-event-reducer! "stream.turn.completed" handle-stream-turn-completed)
(register-event-reducer! "context.pressure" handle-context-pressure)
(register-event-reducer! "gsd.verification.started" handle-verification-started)
(register-event-reducer! "gsd.verification.completed" handle-verification-completed)
(register-event-reducer! "gsd.verification.escalated" handle-verification-escalated)
(register-event-reducer! "mas.spawn-approval-requested" handle-spawn-approval-requested)
(register-event-reducer! "mas.spawn-approval-terminal" handle-spawn-approval-terminal)
