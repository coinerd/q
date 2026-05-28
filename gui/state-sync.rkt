#lang racket/base

;; q/gui/state-sync.rkt — GUI state synchronization

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

(define gui-state-lock (make-semaphore 1))

(define (box-cell-semaphore b)
  gui-state-lock)

(define (drop-right lst n)
  (reverse (list-tail (reverse lst) n)))

(define (make-gui-event-subscriber state-box [notify-callback-box #f])
  (define current-response-text (box ""))
  (define (notify!)
    (define cb (and notify-callback-box (unbox notify-callback-box)))
    (when cb
      (cb)))
  (lambda (evt)
    (define ev (event-ev evt))
    (define payload (event-payload evt))
    (cond
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

      [(equal? ev "model.stream.completed")
       (set-box! current-response-text "")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))
                              (notify!)))]

      [(equal? ev "turn.started")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'processing))
                              (notify!)))]

      [(equal? ev "turn.completed")
       (set-box! current-response-text "")
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))
                              (notify!)))]

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

      [(and (string? ev) (regexp-match? #rx"(?i:error)" ev))
       (call-with-semaphore gui-state-lock
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'error))
                              (notify!)))]

      [else (void)])))

(define (make-notify-gui-callback state-box
                                  messages-obs status-obs
                                  transcript-text cursor-state
                                  theme
                                  peek-obs set-obs! queue-callback)
  (define previous-msgs-box (box '()))

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
    (define new-msgs (hash-ref state 'messages '()))
    (define old-msgs (unbox previous-msgs-box))
    (define st (hash-ref state 'status 'idle))
    (when transcript-text
      (apply-diff-to-text! transcript-text old-msgs new-msgs theme)
      (when (eq? st 'processing)
        (send transcript-text lock #f)
        (send transcript-text insert (streaming-cursor-string cursor-state))
        (send transcript-text lock #t))
      (set-box! previous-msgs-box new-msgs)))

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
