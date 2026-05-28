#lang racket/base

;; q/gui/main.rkt — GUI entry point
;;
;; Thin facade: delegates state synchronization to gui/state-sync.rkt
;; and window construction to gui-easy views.

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
         "../gui/slash-commands.rkt"
         "../gui/state-sync.rkt")

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

  ;; Create a text% object for the rich transcript
  (define transcript-text
    (make-rich-transcript-gui-view text%
                                   editor-canvas%
                                   color%
                                   font%
                                   style-delta%
                                   theme
                                   queue-callback))

  ;; Streaming cursor removed — status bar shows Processing... instead

  ;; Build notify-gui! callback via state-sync factory
  (define notify-gui!
    (make-notify-gui-callback state-box
                              messages-obs
                              status-obs
                              transcript-text
                              theme
                              peek-obs
                              set-obs!
                              queue-callback))

  ;; Store notify callback in box so subscriber can use it
  (set-box! notify-callback-box notify-gui!)

  ;; Slash command handler (extracted to slash-commands.rkt)
  (define handle-slash-command (make-slash-command-handler sess state-box gui-state-lock))

  ;; Input callback: submit on Enter (single-line mode fires 'return on Enter)
  (define (on-input action val)
    (cond
      [(eq? action 'return)
       ;; Enter pressed -> submit message
       (when (and val (> (string-length val) 0))
         (define trimmed (string-trim val))
         (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\/))
             ;; Slash command
             (begin
               (handle-slash-command val)
               (set-obs! input-obs ""))
             ;; Regular message -> LLM
             (begin
               (publish!
                event-bus
                (make-event "user.input" (current-inexact-milliseconds) #f #f (hash 'text val)))
               (thread
                (lambda ()
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
                    (run-prompt! sess val))))
               (set-obs! input-obs ""))))]
      [else (void)]))

  ;; Observable wrapping the text% editor for editor-canvas view
  (define transcript-obs (make-obs transcript-text))

  ;; Mixin to set dark background on editor-canvas% — just call method after init
  (define ((editor-canvas-bg-mixin bg-color) base%)
    (class base%
      (super-new)
      (send this set-canvas-background bg-color)))

  ;; Build and render the window (blocks until closed)
  (render #:wait? #t
          (window #:title (format "q v~a - ~a" q-version (or model-name "q"))
                  #:size '(860 640)
                  (vpanel #:stretch '(#t #t)
                          (hpanel #:stretch '(#t #f) #:style '(border) (text-view status-obs))
                          (editor-canvas-view transcript-obs
                                              #:min-size '(#f 200)
                                              #:stretch '(#t #t)
                                              #:mixin (editor-canvas-bg-mixin bg-c))
                          (input-view input-obs on-input #:stretch '(#t #f)))))

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
