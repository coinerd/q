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
(define (arg-summary-text args)
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

(define (make-gui-event-subscriber state-box [notify-callback-box #f])
  (define current-response-text (box ""))
  (define current-thinking-text (box ""))
  ;; notify-callback-box: (boxof (or/c (-> void?) #f)) — set by launch-gui-window
  (define (notify!)
    (define cb (and notify-callback-box (unbox notify-callback-box)))
    (when cb
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

      ;; Stream delta → accumulate text
      [(equal? ev "model.stream.delta")
       (define delta (hash-ref payload 'delta ""))
       (when (> (string-length delta) 0)
         (set-box! current-response-text (string-append (unbox current-response-text) delta))
         (call-with-semaphore
          gui-state-lock
          (lambda ()
            (define old (unbox state-box))
            (define msgs (gui-state-messages old))
            (define last-msg (and (pair? msgs) (last msgs)))
            (cond
              [(and last-msg (equal? (gui-message-role last-msg) "assistant"))
               (define updated-last (make-gui-message "assistant" (unbox current-response-text)))
               (define all-but-last (drop-right msgs 1))
               (set-box!
                state-box
                (struct-copy gui-state old [messages (append all-but-last (list updated-last))]))]
              [else
               (set-box! state-box
                         (gui-state-add-message old
                                                (make-gui-message "assistant"
                                                                  (unbox current-response-text))))])
            (notify!))))]

      ;; Thinking delta → accumulate thinking text
      [(equal? ev "model.stream.thinking")
       (define delta (hash-ref payload 'delta ""))
       (when (> (string-length delta) 0)
         (set-box! current-thinking-text (string-append (unbox current-thinking-text) delta))
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (set-box! state-box (gui-state-set-status old 'processing))
                                (notify!))))]

      ;; Stream completed → finalize message, flush thinking if any
      [(equal? ev "model.stream.completed")
       ;; Flush accumulated thinking as a kind='thinking entry
       (define thinking-content (unbox current-thinking-text))
       (when (> (string-length thinking-content) 0)
         (set-box! current-thinking-text "")
         (define summary
           (if (> (string-length thinking-content) 200)
               (string-append (substring thinking-content 0 197) "...")
               thinking-content))
         (call-with-semaphore
          gui-state-lock
          (lambda ()
            (define old (unbox state-box))
            (set-box! state-box
                      (gui-state-add-message
                       old
                       (make-gui-message "assistant" summary (hasheq) #:kind 'thinking)))
            (notify!))))
       (set-box! current-response-text "")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (gui-state-set-status old 'idle))
                              (notify!)))]

      ;; Turn started → set processing
      [(equal? ev "turn.started")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (gui-state-set-status old 'processing))
                              (notify!)))]

      ;; Turn completed → set idle
      [(equal? ev "turn.completed")
       (set-box! current-response-text "")
       (set-box! current-thinking-text "")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (gui-state-set-status old 'idle))
                              (notify!)))]

      ;; Tool call started → show in transcript with args as tool-start
      [(equal? ev "tool.call.started")
       (define name (hash-ref payload 'name "unknown"))
       (define args (hash-ref payload 'arguments #f))
       (define arg-summary (arg-summary-text args))
       (call-with-semaphore
        gui-state-lock
        (lambda ()
          (define old (unbox state-box))
          (set-box! state-box
                    (gui-state-add-message old
                                           (make-gui-message "tool"
                                                             (format "[~a]~a" name arg-summary)
                                                             (hasheq 'name name 'arguments args)
                                                             #:kind 'tool-start)))
          (notify!)))]

      ;; Tool execution completed → add new tool-end/tool-fail entry with result
      [(equal? ev "tool.execution.completed")
       (define name (hash-ref payload 'toolName "unknown"))
       (define result-summary (hash-ref payload 'resultSummary 'completed))
       (define is-error (eq? result-summary 'error))
       (define result-text
         (let ([raw (if (string? result-summary)
                        result-summary
                        (format "~a" result-summary))])
           (if (> (string-length raw) 80)
               (string-append (substring raw 0 77) "...")
               raw)))
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box
                                        (gui-state-add-message
                                         old
                                         (make-gui-message "tool"
                                                           (format "[~a] → ~a" name result-text)
                                                           (hasheq 'name name 'result result-summary)
                                                           #:kind
                                                           (if is-error 'tool-fail 'tool-end))))
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
                              (set-box! state-box (gui-state-set-status old 'error))
                              (notify!)))]

      [else (void)])))

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

  (define (update-text%-content! state)
    (define new-msgs (map gui-message->hash (gui-state-messages state)))
    (define old-msgs (unbox previous-msgs-box))
    (when transcript-text
      (apply-diff-to-text! transcript-text old-msgs new-msgs theme last-length-box)
      (set-box! previous-msgs-box new-msgs)))

  (define (notify-gui!)
    (queue-callback (lambda ()
                      (define state (unbox state-box))
                      (when (gui-state? state)
                        (sync-observables! state)
                        (update-text%-content! state)))))

  notify-gui!)
