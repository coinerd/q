#lang racket/base

;; q/gui/state-sync.rkt — GUI state synchronization
;;
;; Extracted from gui/main.rkt to reduce nesting and improve testability.
;; Contains:
;;   - make-gui-event-subscriber  (event-bus → state-box accumulator)
;;   - make-notify-gui-callback   (factory for notify-gui! closure)
;;   - gui-state-lock             (global semaphore for state updates)
;;   - drop-right                 (list helper)

(require racket/class
         racket/string
         racket/list
         "../util/event/event.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../ui-core/theme-protocol.rkt"
         "../ui-core/conversation-reducer.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/ui-intents.rkt"
         "../gui/gui-types.rkt")

(provide make-gui-event-subscriber
         gui-state-lock
         drop-right
         make-notify-gui-callback
         box-cell-semaphore)

;; --------------------------------------------------
;; Global lock for state-box mutations (fast ops, single lock OK)
;; --------------------------------------------------
(define gui-state-lock (make-semaphore 1))

(define (box-cell-semaphore b)
  ;; Per-box semaphore would be cleaner, but a global lock is fine
  ;; since all GUI state updates are fast.
  gui-state-lock)

;; drop-right — re-exported from racket/list

;; --------------------------------------------------
;; GUI event subscriber
;;
;; Listens to event-bus events and accumulates messages
;; into the gui-state struct that the GUI polls.
;; --------------------------------------------------
(define (arg-brief-text args)
  (cond
    [(not args) ""]
    [(hash? args)
     (define keys (hash-keys args))
     (if (null? keys)
         ""
         (format " ~a"
                 (string-join (for/list ([k (in-list keys)])
                                (define v (hash-ref args k))
                                (format "~a: ~a"
                                        k
                                        (if (string? v)
                                            (if (> (string-length v) 30)
                                                (string-append (substring v 0 27) "...")
                                                v)
                                            v)))
                              ", ")))]
    [else ""]))

(define (event->conversation-fact evt)
  (define payload (event-payload evt))
  (define base
    (hasheq 'event-type
            (event-ev evt)
            'session-id
            (event-session-id evt)
            'turn-id
            (event-turn-id evt)))
  (if (hash? payload)
      (for/fold ([fact base]) ([(key value) (in-hash payload)])
        (if (memq key '(session-id sessionId turn-id turnId event-type))
            fact
            (hash-set fact key value)))
      base))

;; Only artifacts carrying a canonical full body are projected into the GUI
;; transcript.  The body is stored verbatim — (conversation-artifact-body a) —
;; and folding to a short preview happens at render time, never here.
(define (projectable-artifact? artifact)
  (and artifact (string? (conversation-artifact-body artifact))))

(define (reduce-gui-conversation state evt)
  (define session-id (event-session-id evt))
  (define turn-id (event-turn-id evt))
  (define next-reducer
    (reduce-event (gui-state-conversation-reducer state) (event->conversation-fact evt)))
  (define reduced (gui-state-set-conversation-reducer state next-reducer))
  (if (and (string? session-id)
           (not (string=? session-id ""))
           (string? turn-id)
           (not (string=? turn-id "")))
      (for/fold ([current reduced])
                ([artifact
                  (in-list
                   (filter projectable-artifact?
                           (list (reducer-thinking-artifact next-reducer session-id turn-id)
                                 (reducer-assistant-artifact next-reducer session-id turn-id))))])
        (gui-state-upsert-artifact current artifact))
      reduced))

(define (canonical-event-identity? evt)
  (and (string? (event-session-id evt))
       (not (string=? (event-session-id evt) ""))
       (string? (event-turn-id evt))
       (not (string=? (event-turn-id evt) ""))))

(define (event-for-active-turn? state evt)
  (if (or (gui-state-active-session-id state) (gui-state-active-turn-id state))
      (and (canonical-event-identity? evt)
           (equal? (gui-state-active-session-id state) (event-session-id evt))
           (equal? (gui-state-active-turn-id state) (event-turn-id evt)))
      ;; Preserve legacy terminal recovery only while idle. A processing status
      ;; without identity is still active UI state and must not be cleared by a
      ;; late terminal from an unrelated invocation.
      (not (eq? (gui-state-status state) 'processing))))

(define (event-can-activate-gui-turn? state evt)
  (and (canonical-event-identity? evt)
       (or (and (not (gui-state-active-session-id state)) (not (gui-state-active-turn-id state)))
           (event-for-active-turn? state evt))))

(define (activate-gui-turn state evt [force? #f])
  (if (and (canonical-event-identity? evt)
           (or force?
               (and (not (gui-state-active-session-id state)) (not (gui-state-active-turn-id state)))
               (event-for-active-turn? state evt)))
      (struct-copy gui-state
                   state
                   [active-session-id (event-session-id evt)]
                   [active-turn-id (event-turn-id evt)])
      state))

(define (finish-gui-turn state)
  (struct-copy gui-state
               (gui-state-set-status state 'idle)
               [active-session-id #f]
               [active-turn-id #f]))

(define (make-gui-event-subscriber state-box [notify-callback-box #f])
  ;; A parameter is thread/invocation local. Concurrent deliveries cannot
  ;; clear or steal one another's pending notification.
  (define notification-pending? (make-parameter #f))
  (define (notify!)
    (notification-pending? #t))
  (define (deliver-notification!)
    (define cb
      (and notify-callback-box
           (call-with-semaphore gui-state-lock (lambda () (unbox notify-callback-box)))))
    (when (and (notification-pending?) cb)
      (cb)))
  (define (add-system-msg! text [meta (hasheq)])
    (call-with-semaphore
     gui-state-lock
     (lambda ()
       (define old (unbox state-box))
       (set-box! state-box
                 (gui-state-add-message old (make-gui-message "system" text meta #:kind 'system)))
       (notify!))))
  (lambda (evt)
    (parameterize ([notification-pending? #f])
      (define ev (event-ev evt))
      (define payload (event-payload evt))
      (cond
        ;; User sent a message → add to transcript
        [(equal? ev "user.input")
         (define text (hash-ref payload 'text ""))
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (set-box! state-box
                                          (gui-state-add-message old (make-gui-message "user" text)))
                                (notify!)))]

        ;; Stream deltas are projected from the shared canonical reducer.  The
        ;; envelope's session/turn identity is used verbatim.
        [(equal? ev "model.stream.delta")
         (define delta (hash-ref payload 'delta ""))
         (when (> (string-length delta) 0)
           (call-with-semaphore
            gui-state-lock
            (lambda ()
              (define old (unbox state-box))
              (when (event-can-activate-gui-turn? old evt)
                (set-box! state-box (activate-gui-turn (reduce-gui-conversation old evt) evt))
                (notify!)))))]

        [(equal? ev "model.stream.thinking")
         (define delta (hash-ref payload 'delta ""))
         (when (> (string-length delta) 0)
           (call-with-semaphore gui-state-lock
                                (lambda ()
                                  (define old (unbox state-box))
                                  (when (event-can-activate-gui-turn? old evt)
                                    (define reduced
                                      (activate-gui-turn (reduce-gui-conversation old evt) evt))
                                    (set-box! state-box (gui-state-set-status reduced 'processing))
                                    (notify!)))))]

        ;; Either terminal order updates the same session+turn artifacts.  No
        ;; destructive shortening of the reasoning body is performed here; the
        ;; folded presentation is derived from the full artifact body by
        ;; gui-types.rkt.
        [(equal? ev "model.stream.completed")
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (define reduced (reduce-gui-conversation old evt))
                                (set-box! state-box
                                          (if (event-for-active-turn? old evt)
                                              (finish-gui-turn reduced)
                                              reduced))
                                (notify!)))]

        [(equal? ev "assistant.message.completed")
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (define reduced (reduce-gui-conversation old evt))
                                (set-box! state-box
                                          (if (event-for-active-turn? old evt)
                                              (finish-gui-turn reduced)
                                              reduced))
                                (notify!)))]

        ;; Turn started → set processing
        [(and (equal? ev "turn.started") (canonical-event-identity? evt))
         (call-with-semaphore
          gui-state-lock
          (lambda ()
            (define old (unbox state-box))
            (set-box! state-box (gui-state-set-status (activate-gui-turn old evt #t) 'processing))
            (notify!)))]

        ;; Turn completed → set idle
        [(equal? ev "turn.completed")
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (when (event-for-active-turn? old evt)
                                  (set-box! state-box (finish-gui-turn old))
                                  (notify!))))]

        ;; Tool call started → show in transcript with args as tool-start
        [(equal? ev "tool.call.started")
         (define name (hash-ref payload 'name "unknown"))
         (define args (hash-ref payload 'arguments #f))
         (define arg-brief (arg-brief-text args))
         (call-with-semaphore
          gui-state-lock
          (lambda ()
            (define old (unbox state-box))
            (set-box! state-box
                      (gui-state-add-message old
                                             (make-gui-message "tool"
                                                               (format "[~a]~a" name arg-brief)
                                                               (hasheq 'name name 'arguments args)
                                                               #:kind 'tool-start)))
            (notify!)))]

        ;; Tool execution completed → add new tool-end/tool-fail entry with result
        [(equal? ev "tool.execution.completed")
         (define name (hash-ref payload 'toolName "unknown"))
         (define result-note (hash-ref payload 'resultSummary 'completed))
         (define is-error (eq? result-note 'error))
         (define result-text
           (let ([raw (if (string? result-note)
                          result-note
                          (format "~a" result-note))])
             (if (> (string-length raw) 80)
                 (string-append (substring raw 0 77) "...")
                 raw)))
         (call-with-semaphore
          gui-state-lock
          (lambda ()
            (define old (unbox state-box))
            (set-box! state-box
                      (gui-state-add-message
                       old
                       (make-gui-message "tool"
                                         (format "[~a] → ~a" name result-text)
                                         (hasheq 'name name 'result result-note)
                                         #:kind (if is-error 'tool-fail 'tool-end))))
            (notify!)))]

        ;; ─── Compaction events ───
        [(equal? ev "compaction.warning")
         (define tokens (hash-ref payload 'tokenCount 0))
         (add-system-msg! (format "[compaction warning: ~a tokens]" tokens))]

        [(equal? ev "compaction.started")
         (add-system-msg! "[compacting context...]" (hasheq 'compaction #t))]

        [(equal? ev "compaction.completed")
         (define reduction (hash-ref payload 'reduction "context compressed"))
         (add-system-msg! (format "[compaction done: ~a]" reduction) (hasheq 'compaction #t))]

        ;; ─── Retry events ───
        [(equal? ev "auto-retry.start")
         (define reason (hash-ref payload 'reason "rate limited"))
         (define attempt (hash-ref payload 'attempt 1))
         (define max-attempts (hash-ref payload 'maxAttempts 3))
         (add-system-msg! (format "[retry: ~a, ~a/~a...]" reason attempt max-attempts))]

        ;; ─── Iteration / exploration events ───
        [(equal? ev "iteration.soft-warning")
         (define iter (hash-ref payload 'iteration 0))
         (define remaining (hash-ref payload 'remaining "?"))
         (add-system-msg! (format "[exploring... iteration ~a, ~a remaining]" iter remaining))]

        ;; ─── Context pressure events ───
        [(equal? ev "context.pressure")
         (define level (hash-ref payload 'level "low"))
         (define pct (hash-ref payload 'usagePercent 0))
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (define info
                                  (hasheq 'level
                                          (if (string? level)
                                              (string->symbol level)
                                              level)
                                          'percent
                                          pct))
                                (set-box! state-box (gui-state-set-context-info old info))
                                (notify!)))]

        [(equal? ev "context.mid-turn-over-budget")
         (define used (hash-ref payload 'tokensUsed 0))
         (define budget (hash-ref payload 'tokenBudget 0))
         (add-system-msg! (format "[context growing: ~a/~a tokens]" used budget))]

        ;; ─── Session events ───
        [(equal? ev "session.started") (add-system-msg! "[session started]")]

        [(equal? ev "session.forked")
         (define sid (hash-ref payload 'sessionId "?"))
         (add-system-msg! (format "[session forked: ~a]" sid))]

        ;; ─── Goal events ───
        [(equal? ev "goal.started")
         (define desc (hash-ref payload 'description ""))
         (when (> (string-length desc) 0)
           (call-with-semaphore gui-state-lock
                                (lambda ()
                                  (define old (unbox state-box))
                                  (set-box! state-box (struct-copy gui-state old [active-goal desc]))
                                  (notify!))))]

        [(equal? ev "goal.achieved")
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (set-box! state-box (struct-copy gui-state old [active-goal #f]))
                                (notify!)))]

        [(equal? ev "goal.failed")
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (set-box! state-box (struct-copy gui-state old [active-goal #f]))
                                (notify!)))]

        ;; ─── Tool blocked ───
        [(equal? ev "tool.call.blocked")
         (define name (hash-ref payload 'name "unknown"))
         (define reason (hash-ref payload 'reason "blocked"))
         (add-system-msg! (format "[tool blocked: ~a — ~a]" name reason))]

        ;; Error events
        [(and (string? ev) (regexp-match? #rx"(?i:error)" ev))
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (when (event-for-active-turn? old evt)
                                  (set-box! state-box (gui-state-set-status old 'error))
                                  (notify!))))]

        [else (void)])
      (deliver-notification!))))

;; --------------------------------------------------
;; Factory: create the notify-gui! callback closure
;;
;; All parameters that were previously closed over in
;; launch-gui-window are now explicit arguments.
;; --------------------------------------------------
(define (make-notify-gui-callback state-box
                                  messages-obs
                                  status-obs
                                  transcript-text
                                  theme
                                  peek-obs
                                  set-obs!
                                  queue-callback)
  (define (sync-observables! state)
    (define msgs (map gui-message->hash (gui-state-messages state)))
    (unless (equal? msgs (peek-obs messages-obs))
      (set-obs! messages-obs msgs))
    (define st (gui-state-status state))
    (define ctx-info (gui-state-context-info state))
    (define goal (gui-state-active-goal state))
    (define ctx-pct (and (hash? ctx-info) (hash-ref ctx-info 'percent #f)))
    (define status-str
      (cond
        [(eq? st 'error) "Error"]
        [else
         (string-join (filter string?
                              (list (or (gui-state-model state) "q")
                                    (cond
                                      [(eq? st 'processing) "Processing..."]
                                      [else "Ready"])
                                    (and ctx-pct (format "ctx:~a%" ctx-pct))
                                    (and goal
                                         (if (> (string-length goal) 30)
                                             (format "Goal: ~a..." (substring goal 0 27))
                                             (format "Goal: ~a" goal)))))
                      " | ")]))
    (unless (equal? status-str (peek-obs status-obs))
      (set-obs! status-obs status-str)))

  (define previous-msgs-box (box '()))
  (define last-length-box (box 0))

  (define (apply-disclosure-target! target-id)
    (define changed?
      (call-with-semaphore
       gui-state-lock
       (lambda ()
         (define state (unbox state-box))
         (and (gui-state? state)
              (begin
                (set-box! state-box
                          (gui-state-apply-intent state (make-toggle-detail-intent target-id)))
                #t)))))
    (when changed?
      (notify-gui!)))

  (define (update-text%-content! state)
    (define new-msgs (map gui-message->hash (gui-state-messages state)))
    (define old-msgs (unbox previous-msgs-box))
    (when transcript-text
      (apply-diff-to-text! transcript-text
                           old-msgs
                           new-msgs
                           theme
                           last-length-box
                           apply-disclosure-target!)
      (set-box! previous-msgs-box new-msgs)))

  (define (notify-gui!)
    (queue-callback (lambda ()
                      (define state (unbox state-box))
                      (when (gui-state? state)
                        (sync-observables! state)
                        (update-text%-content! state)))))

  notify-gui!)
