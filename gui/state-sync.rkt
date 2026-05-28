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
         "../util/event.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../gui/components/streaming-cursor.rkt"
         "../ui-core/theme-protocol.rkt")

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

;; Helper: drop-right for lists
(define (drop-right lst n)
  (reverse (list-tail (reverse lst) n)))

;; --------------------------------------------------
;; GUI event subscriber
;;
;; Listens to event-bus events and accumulates messages
;; into the state hash that the GUI polls.
;; --------------------------------------------------
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
       (call-with-semaphore
        gui-state-lock
        (lambda ()
          (define old (unbox state-box))
          (define msgs (hash-ref old 'messages '()))
          (set-box! state-box
                    (hash-set old 'messages (append msgs (list (hash 'role "user" 'text text)))))
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
            (define msgs (hash-ref old 'messages '()))
            (define last-msg (and (pair? msgs) (car (reverse msgs))))
            (cond
              [(and last-msg (equal? (hash-ref last-msg 'role #f) "assistant"))
               (define updated-last (hash-set last-msg 'text (unbox current-response-text)))
               (define all-but-last (drop-right msgs 1))
               (set-box! state-box
                         (hash-set old 'messages (append all-but-last (list updated-last))))]
              [else
               (set-box! state-box
                         (hash-set old
                                   'messages
                                   (append msgs (list (hash 'role "assistant"
                                                            'text (unbox current-response-text))))))])
            (notify!))))]

      ;; Thinking delta → show in transcript (dimmed)
      [(equal? ev "model.stream.thinking")
       (define delta (hash-ref payload 'delta ""))
       (when (> (string-length delta) 0)
         (call-with-semaphore gui-state-lock
                              (lambda ()
                                (define old (unbox state-box))
                                (define msgs (hash-ref old 'messages '()))
                                (when (not (equal? (and (pair? msgs)
                                                        (hash-ref (car (reverse msgs)) 'role #f))
                                                   "assistant"))
                                  (set-box! state-box (hash-set old 'status 'processing)))
                                (notify!))))]

      ;; Stream completed → finalize message
      [(equal? ev "model.stream.completed")
       (set-box! current-response-text "")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))
                              (notify!)))]

      ;; Turn started → set processing
      [(equal? ev "turn.started")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'processing))
                              (notify!)))]

      ;; Turn completed → set idle
      [(equal? ev "turn.completed")
       (set-box! current-response-text "")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))
                              (notify!)))]

      ;; Tool call started → show in transcript
      [(equal? ev "tool.call.started")
       (define name (hash-ref payload 'name "unknown"))
       (call-with-semaphore
        gui-state-lock
        (lambda ()
          (define old (unbox state-box))
          (define msgs (hash-ref old 'messages '()))
          (set-box! state-box
                    (hash-set old
                              'messages
                              (append msgs (list (hash 'role "tool" 'text (format "[~a]" name))))))
          (notify!)))]

      ;; Error events
      [(and (string? ev) (regexp-match? #rx"(?i:error)" ev))
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'error))
                              (notify!)))]

      [else (void)])))

;; --------------------------------------------------
;; Factory: create the notify-gui! callback closure
;;
;; All parameters that were previously closed over in
;; launch-gui-window are now explicit arguments.
;; --------------------------------------------------
(define (make-notify-gui-callback state-box
                                  messages-obs status-obs
                                  transcript-text cursor-state
                                  theme
                                  peek-obs set-obs! queue-callback)
  (define (sync-observables! state)
    (define msgs (hash-ref state 'messages '()))
    (unless (equal? msgs (peek-obs messages-obs))
      (set-obs! messages-obs msgs))
    (define st (hash-ref state 'status 'idle))
    (define status-str
      (cond
        [(eq? st 'processing) "Processing..."]
        [(eq? st 'error) "Error"]
        [else "Ready"]))
    (unless (equal? status-str (peek-obs status-obs))
      (set-obs! status-obs status-str)))

  (define (update-text%-content! state)
    (define msgs (hash-ref state 'messages '()))
    (define st (hash-ref state 'status 'idle))
    (when transcript-text
      (send transcript-text lock #f)
      (send transcript-text delete 0 (send transcript-text last-position))
      (for ([msg (in-list msgs)])
        (insert-message-into-text! transcript-text msg theme))
      (when (eq? st 'processing)
        (send transcript-text insert (streaming-cursor-string cursor-state)))
      (send transcript-text lock #t)))

  (define (manage-streaming-cursor! state)
    (define st (hash-ref state 'status 'idle))
    (cond
      [(eq? st 'processing)
       (unless (streaming-cursor-active? cursor-state)
         (streaming-cursor-start! cursor-state notify-gui!))]
      [else
       (when (streaming-cursor-active? cursor-state)
         (streaming-cursor-stop! cursor-state))]))

  (define (notify-gui!)
    (queue-callback (lambda ()
      (define state (unbox state-box))
      (when (hash? state)
        (sync-observables! state)
        (update-text%-content! state)
        (manage-streaming-cursor! state)))))

  notify-gui!)
