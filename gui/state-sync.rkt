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
         "../util/event.rkt"
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
  ;; notify-callback-box: (boxof (or/c (-> void?) #f)) — set by launch-gui-window
  (define (notify!)
    (define cb (and notify-callback-box (unbox notify-callback-box)))
    (when cb
      (cb)))
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
               (define updated-last (gui-message "assistant" (unbox current-response-text) (hasheq)))
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

      ;; Thinking delta → show in transcript (dimmed)
      [(equal? ev "model.stream.thinking")
       (define delta (hash-ref payload 'delta ""))
       (when (> (string-length delta) 0)
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (define msgs (gui-state-messages old))
                                (when (not (equal? (and (pair? msgs) (gui-message-role (last msgs)))
                                                   "assistant"))
                                  (set-box! state-box (gui-state-set-status old 'processing)))
                                (notify!))))]

      ;; Stream completed → finalize message
      [(equal? ev "model.stream.completed")
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
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (gui-state-set-status old 'idle))
                              (notify!)))]

      ;; Tool call started → show in transcript with args
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
                                                             (hasheq 'name name 'arguments args))))
          (notify!)))]

      ;; Tool execution completed → show result
      [(equal? ev "tool.execution.completed")
       (define name (hash-ref payload 'name "unknown"))
       (define result-raw (hash-ref payload 'result #f))
       (define result-text
         (let ([s (cond
                    [(string? result-raw) result-raw]
                    [(hash? result-raw) (hash-ref result-raw 'text "")]
                    [else (format "~a" result-raw)])])
           (if (> (string-length s) 120)
               (string-append (substring s 0 117) "...")
               s)))
       (call-with-semaphore
        gui-state-lock
        (lambda ()
          (define old (unbox state-box))
          (set-box! state-box
                    (gui-state-add-message old
                                           (make-gui-message "tool-result"
                                                             (format "\u2192 ~a" result-text)
                                                             (hasheq 'name name 'result result-raw))))
          (notify!)))]
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
    (define status-str
      (cond
        [(eq? st 'processing) "Processing..."]
        [(eq? st 'error) "Error"]
        [else "Ready"]))
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
