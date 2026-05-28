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
         "../util/version.rkt")

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
(define (make-gui-event-subscriber state-box)
  (define current-response-text (box ""))
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
                    (hash-set old 'messages (append msgs (list (hash 'role "user" 'text text)))))))]

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
                 (append msgs
                         (list (hash 'role "assistant" 'text (unbox current-response-text))))))]))))]

      ;; Thinking delta → show in transcript (dimmed)
      [(equal? ev "model.stream.thinking")
       (define delta (hash-ref payload 'delta ""))
       (when (> (string-length delta) 0)
         (call-with-semaphore (box-cell-semaphore state-box)
                              (lambda ()
                                (define old (unbox state-box))
                                (define msgs (hash-ref old 'messages '()))
                                (define last-msg (and (pair? msgs) (hash-ref last-msg 'role #f)))
                                ;; Only add thinking message if we haven't started response yet
                                (when (not (equal? (and (pair? msgs)
                                                        (hash-ref (car (reverse msgs)) 'role #f))
                                                   "assistant"))
                                  (set-box! state-box (hash-set old 'status 'processing))))))]

      ;; Stream completed → finalize message
      [(equal? ev "model.stream.completed")
       (set-box! current-response-text "")
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))))]

      ;; Turn started → set processing
      [(equal? ev "turn.started")
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'processing))))]

      ;; Turn completed → set idle
      [(or (equal? ev "turn.completed") (equal? ev "model.stream.completed"))
       (set-box! current-response-text "")
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'idle))))]

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
                              (append msgs (list (hash 'role "tool" 'text (format "[~a]" name))))))))]

      ;; Error events
      [(and (string? ev) (regexp-match? #rx"(?i:error)" ev))
       (call-with-semaphore (box-cell-semaphore state-box)
                            (lambda ()
                              (define old (unbox state-box))
                              (set-box! state-box (hash-set old 'status 'error))))]

      [else (void)])))

;; Helper: semaphore for box access (thread safety)
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
(define (launch-gui-window state-box sess event-bus theme model-name)
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
  (define canvas-view (dynamic-require 'racket/gui/easy/view 'canvas))

  ;; Load racket/gui classes for color/font objects
  (define color% (dynamic-require 'racket/gui 'color%))
  (define font% (dynamic-require 'racket/gui 'font%))

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

  ;; Background thread: poll state-box -> update observables
  (define poll-thread
    (thread (lambda ()
              (let loop ()
                (sleep 0.1)
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
                    (set-obs! status-obs status-str)))
                (loop)))))

  ;; Input callback: submit on Enter → run-prompt!
  (define (on-input action val)
    (when (eq? action 'return)
      (when (and val (> (string-length val) 0))
        ;; Publish user.input event for the subscriber to pick up
        (publish! event-bus
                  (make-event 'user.input (current-inexact-milliseconds) #f #f (hash 'text val)))
        ;; Run the prompt in a background thread
        (thread (lambda ()
                  (with-handlers ([exn:fail?
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
                                           (append msgs
                                                   (list (hash 'role "error" 'text (exn-message e)))))
                                          'status
                                          'error)))))])
                    (run-prompt! sess val)))))
      (set-obs! input-obs "")))

  ;; Canvas draw callback: render messages
  (define (on-draw dc msgs)
    (define-values (cw ch) (send dc get-size))
    (send dc set-background bg-c)
    (send dc clear)
    (send dc set-font mono-font)
    (let loop ([rest (if (list? msgs)
                         msgs
                         '())]
               [y 4])
      (cond
        [(null? rest) (void)]
        [(>= (+ y 16) ch) (void)]
        [else
         (define m (car rest))
         (define role (and (hash? m) (hash-ref m 'role "system")))
         (define txt
           (if (hash? m)
               (hash-ref m 'text (~a m))
               (~a m)))
         ;; Color by role
         (send dc set-text-foreground
               (case role
                 [("user") user-c]
                 [("tool") tool-c]
                 [("error") (hex->color "#f38ba8")]
                 [else fg-c]))
         (send dc draw-text txt 4 y)
         (loop (cdr rest) (+ y 16))])))

  ;; Build and render the window (blocks until closed)
  (render
   (window
    #:title (format "q v~a - ~a" q-version (or model-name "q"))
    #:size '(860 640)
    (vpanel
     #:stretch '(#t #t)
     (hpanel #:stretch '(#t #f) #:style '(border) (text-view status-obs))
     (canvas-view messages-obs on-draw #:style '(border vscroll) #:stretch '(#t #t))
     (input-view input-obs on-input #:style '(single) #:stretch '(#t #f) #:min-size '(#f 30)))))

  ;; Cleanup after window closes
  (kill-thread poll-thread))

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

  ;; Subscribe our event handler to the bus
  (when bus
    (subscribe! bus (make-gui-event-subscriber state-box)))

  ;; If there's an initial prompt from CLI, run it after GUI starts
  (define initial-prompt
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (dict-ref rt-config 'prompt #f)))

  ;; Launch the GUI window (blocks until closed)
  (launch-gui-window state-box sess bus theme model-name)

  ;; Cleanup
  (close-session! sess))
