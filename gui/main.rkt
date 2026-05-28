#lang racket/base

;; q/gui/main.rkt — GUI entry point
;;
;; Entry point for the native GUI mode. Wires the agent session,
;; event bus, and observable bridge to a gui-easy window.
;;
;; Architecture:
;;   user types → on-input callback → run-prompt! (bg thread)
;;   LLM streams → event-bus → subscriber → accumulate messages → obs update → GUI refresh
;;
;; gui-easy-lib installs under the racket collection as
;; racket/gui/easy/... (collection "racket" in info.rkt).
;; racket/gui classes (color%, font%) are dynamically loaded
;; inside launch-gui-window to avoid requiring a display at
;; module load time.
;;
;; gui-easy observable API:
;;   obs, obs?, obs-set!, obs-peek, obs-observe!, obs-update!

(require racket/contract
         racket/dict
         racket/format
         racket/class
         racket/string
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../ui-core/observable-bridge.rkt"
         "../ui-core/dispatch.rkt"
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt"
         "../util/event.rkt"
         "../util/version.rkt"
         "../extensions/hooks.rkt"
         "../tui/command-parse.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../gui/components/streaming-cursor.rkt"
         "../gui/slash-commands.rkt")

(provide (contract-out [run-gui-with-runtime (-> any/c any/c void?)]
                       [run-gui (-> void?)]
                       [gui-available? (-> boolean?)]))

;; --------------------------------------------------
;; Check if GUI is available
;; --------------------------------------------------
(define (gui-available?)
  (and (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'racket/gui/easy/observable #f)
         (dynamic-require 'racket/gui/easy/view #f)
         (dynamic-require 'racket/gui/easy/renderer #f)
         #t)))

;; --------------------------------------------------
;; Internal: GUI event subscriber
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
        (box-cell-semaphore state-box)
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
          (box-cell-semaphore state-box)
          (lambda ()
            (define old (unbox state-box))
            (define msgs (hash-ref old 'messages '()))
            ;; Update or append the assistant message
            (define last-msg (and (pair? msgs) (car (reverse msgs))))
            (cond
              [(and last-msg (equal? (hash-ref last-msg 'role #f) "assistant"))
               ;; Update existing assistant message text
               (define updated-last (hash-set last-msg 'text (unbox current-response-text)))
               (define all-but-last (drop-right msgs 1))
               (set-box! state-box
                         (hash-set old 'messages (append all-but-last (list updated-last))))]
              [else
               ;; First delta — add new assistant message
               (set-box!
                state-box
                (hash-set
                 old
                 'messages
                 (append msgs (list (hash 'role "assistant" 'text (unbox current-response-text))))))])
            (notify!))))]

      ;; Thinking delta → show in transcript (dimmed)
      [(equal? ev "model.stream.thinking")
       (define delta (hash-ref payload 'delta ""))
       (when (> (string-length delta) 0)
         (call-with-semaphore (box-cell-semaphore state-box)
                              (lambda ()
                                (define old (unbox state-box))
                                (define msgs (hash-ref old 'messages '()))
                                ;; Only add thinking message if we haven't started response yet
                                (when (not (equal? (and (pair? msgs)
                                                        (hash-ref (car (reverse msgs)) 'role #f))
                                                   "assistant"))
                                  (set-box! state-box (hash-set old 'status 'processing)))
                                (notify!))))]

      ;; Stream completed → finalize message
      [(equal? ev "model.stream.completed")
       (set-box! current-response-text "")
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))
                              (notify!)))]

      ;; Turn started → set processing
      [(equal? ev "turn.started")
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'processing))
                              (notify!)))]

      ;; Turn completed → set idle
      [(or (equal? ev "turn.completed") (equal? ev "model.stream.completed"))
       (set-box! current-response-text "")
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))
                              (notify!)))]

      ;; Tool call started → show in transcript
      [(equal? ev "tool.call.started")
       (define name (hash-ref payload 'name "unknown"))
       (call-with-semaphore
        (box-cell-semaphore state-box)
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
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'error))
                              (notify!)))]

      [else (void)])))
(define (box-cell-semaphore b)
  ;; Reuse a per-box semaphore — we store it on the box's props
  ;; Simple approach: just use a global lock since updates are fast
  gui-state-lock)

(define gui-state-lock (make-semaphore 1))

;; Helper: drop-right for lists
(define (drop-right lst n)
  (reverse (list-tail (reverse lst) n)))

;; --------------------------------------------------
;; Internal: launch gui-easy window (blocks until closed)
;; --------------------------------------------------
(define (launch-gui-window state-box sess event-bus theme model-name notify-callback-box)
  ;; Dynamically load gui-easy to keep it optional at compile time.
  (define make-obs (dynamic-require 'racket/gui/easy/observable 'obs))
  (define peek-obs (dynamic-require 'racket/gui/easy/observable 'obs-peek))
  (define set-obs! (dynamic-require 'racket/gui/easy/observable 'obs-set!))
  (define render (dynamic-require 'racket/gui/easy/renderer 'render))
  (define window (dynamic-require 'racket/gui/easy/view 'window))
  (define vpanel (dynamic-require 'racket/gui/easy/view 'vpanel))
  (define hpanel (dynamic-require 'racket/gui/easy/view 'hpanel))
  (define text-view (dynamic-require 'racket/gui/easy/view 'text))
  (define input-view (dynamic-require 'racket/gui/easy/view 'input))
  (define editor-canvas-view (dynamic-require 'racket/gui/easy/view 'editor-canvas))

  ;; Load racket/gui classes for color/font objects
  (define color% (dynamic-require 'racket/gui 'color%))
  (define font% (dynamic-require 'racket/gui 'font%))
  (define text% (dynamic-require 'racket/gui 'text%))
  (define editor-canvas% (dynamic-require 'racket/gui 'editor-canvas%))
  (define style-delta% (dynamic-require 'racket/gui 'style-delta%))
  (define queue-callback (dynamic-require 'racket/gui 'queue-callback))

  ;; Helper: hex color string -> color% object
  (define (hex->color hex)
    (make-object color%
                 (string->number (substring hex 1 3) 16)
                 (string->number (substring hex 3 5) 16)
                 (string->number (substring hex 5 7) 16)))

  ;; Theme colors
  (define bg-c (hex->color (or (theme-ref theme 'background) "#1e1e2e")))
  (define fg-c (hex->color (or (theme-ref theme 'foreground) "#cdd6f4")))
  (define dim-c (hex->color "#6c7086"))
  (define user-c (hex->color "#89b4fa"))
  (define tool-c (hex->color "#a6e3a1"))
  (define mono-font (make-object font% 12 'modern 'normal 'normal #f))

  ;; Observable GUI state
  (define messages-obs (make-obs '()))
  (define status-obs (make-obs "Ready"))
  (define input-obs (make-obs ""))

  ;; Direct observable update via queue-callback (replaces poll thread)
  ;; Called from event subscriber threads, schedules GUI thread update
  (define (notify-gui!)
    (queue-callback (lambda ()
                      (define state (unbox state-box))
                      (when (hash? state)
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
                          (set-obs! status-obs status-str))
                        ;; Update text% transcript (editor-canvas% display requires display server)
                        (when transcript-text
                          (send transcript-text lock #f)
                          (send transcript-text delete 0 (send transcript-text last-position))
                          (for ([msg (in-list msgs)])
                            (insert-message-into-text! transcript-text msg theme))
                          ;; Append streaming cursor when processing
                          (when (eq? st 'processing)
                            (send transcript-text insert (streaming-cursor-string cursor-state)))
                          (send transcript-text lock #t))
                        ;; Manage streaming cursor based on status
                        (cond
                          [(eq? st 'processing)
                           (unless (streaming-cursor-active? cursor-state)
                             (streaming-cursor-start! cursor-state notify-gui!))]
                          [else
                           (when (streaming-cursor-active? cursor-state)
                             (streaming-cursor-stop! cursor-state))]))))))

  ;; Store notify callback in box so subscriber can use it
  (set-box! notify-callback-box notify-gui!)

  ;; Slash command handler (extracted to slash-commands.rkt)
  (define handle-slash-command (make-slash-command-handler sess state-box gui-state-lock))

    ;; Input callback: submit on Enter → slash command or run-prompt!
  (define (on-input action val)
    (when (eq? action 'return)
      (when (and val (> (string-length val) 0))
        (define trimmed (string-trim val))
        (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\/))
            ;; Slash command
            (begin
              (handle-slash-command val)
              (set-obs! input-obs ""))
            ;; Regular message → LLM
            (begin
              ;; Publish user.input event for the subscriber to pick up
              (publish!
               event-bus
               (make-event "user.input" (current-inexact-milliseconds) #f #f (hash 'text val)))
              ;; Run the prompt in a background thread
              (thread (lambda ()
                        (with-handlers
                            ([exn:fail?
                              (lambda (e)
                                (call-with-semaphore
                                 gui-state-lock
                                 (lambda ()
                                   (define old (unbox state-box))
                                   (define msgs (hash-ref old 'messages '()))
                                   (set-box!
                                    state-box
                                    (hash-set
                                     (hash-set
                                      old
                                      'messages
                                      (append msgs (list (hash 'role "error" 'text (exn-message e)))))
                                     'status
                                     'error)))))])
                          (run-prompt! sess val))))
              (set-obs! input-obs ""))))))

  ;; Create a text% object for the rich transcript
  (define transcript-text
    (make-rich-transcript-gui-view text%
                                   editor-canvas%
                                   color%
                                   font%
                                   style-delta%
                                   theme
                                   queue-callback))

  ;; Observable wrapping the text% editor for editor-canvas view
  (define transcript-obs (make-obs transcript-text))

  ;; Streaming cursor state (blinks during LLM response)
  (define cursor-state (make-streaming-cursor-state))

  ;; Build and render the window (blocks until closed)
  (render
   #:wait? #t
   (window
    #:title (format "q v~a - ~a" q-version (or model-name "q"))
    #:size '(860 640)
    (vpanel
     #:stretch '(#t #t)
     (hpanel #:stretch '(#t #f) #:style '(border) (text-view status-obs))
     (editor-canvas-view transcript-obs #:min-size '(#f 200) #:stretch '(#t #t))
     (input-view input-obs on-input #:style '(multiple) #:stretch '(#t #f) #:min-size '(#f 60)))))

  ;; Cleanup after window closes
  (void))

;; --------------------------------------------------
;; run-gui -- standalone GUI (no runtime)
;; --------------------------------------------------
(define (run-gui)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (eprintf "Install gui-easy-lib: raco pkg install gui-easy-lib\n")
    (exit 1))
  (eprintf "q GUI v~a -- standalone mode requires a prompt, use --gui with arguments\n" q-version)
  (exit 0))

;; --------------------------------------------------
;; run-gui-with-runtime -- full GUI with agent runtime
;; --------------------------------------------------
(define (run-gui-with-runtime rt-config cfg)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (eprintf "Install gui-easy-lib: raco pkg install gui-easy-lib\n")
    (exit 1))

  ;; Create agent session (wires provider, tools, extensions, event bus)
  (define sess (make-agent-session rt-config))
  (define bus (dict-ref rt-config 'event-bus #f))
  (define theme (default-theme))
  (define model-name (dict-ref rt-config 'model-name #f))

  ;; GUI state: accumulated messages + status
  (define state-box (box (hash 'messages '() 'status 'idle 'model model-name)))

  ;; Notify callback box — set by launch-gui-window after GUI thread starts
  (define notify-callback-box (box #f))

  ;; Subscribe our event handler to the bus (with notify-callback-box)
  (when bus
    (subscribe! bus (make-gui-event-subscriber state-box notify-callback-box)))

  ;; If there's an initial prompt from CLI, run it after GUI starts
  (define initial-prompt
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (dict-ref rt-config 'prompt #f)))

  ;; Launch the GUI window (blocks until closed)
  (launch-gui-window state-box sess bus theme model-name notify-callback-box)

  ;; Cleanup
  (close-session! sess))
