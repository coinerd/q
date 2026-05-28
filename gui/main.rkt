#lang racket/base

;; q/gui/main.rkt — GUI entry point
;;
;; Entry point for the native GUI mode. Wires the observable bridge
;; to the agent runtime event-bus and opens a gui-easy window.
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
         "../agent/event-bus.rkt"
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
;; Internal: create bridge and wire to runtime
;; --------------------------------------------------
(define (wire-bridge! rt-config)
  (define bus (dict-ref rt-config 'event-bus #f))
  (define state-box (box (hash 'messages '() 'status 'idle 'model #f)))
  (define bridge
    (make-gui-state-bridge state-box
                           (unbox state-box)
                           #:filter (lambda (evt)
                                      (define t (event-ev evt))
                                      (and t (symbol? t)))
                           #:transform (lambda (evt) evt)))
  (when bus
    (bridge-subscribe! bridge bus))
  bridge)

;; --------------------------------------------------
;; Internal: launch gui-easy window (blocks until closed)
;; --------------------------------------------------
(define (launch-gui-window bridge theme model-name event-bus)
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
  (define mono-font (make-object font% 12 'modern 'normal 'normal #f))

  ;; Observable GUI state
  (define messages-obs (make-obs '()))
  (define status-obs (make-obs "Ready"))
  (define input-obs (make-obs ""))

  ;; Background thread: poll bridge state -> update observables
  (define poll-thread
    (thread (lambda ()
              (let loop ()
                (sleep 0.15)
                (define state (bridge-state-ref bridge))
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

  ;; Input callback: submit on Enter
  (define (on-input action val)
    (when (eq? action 'return)
      (when event-bus
        (publish! event-bus
                  (make-event 'user.input (current-inexact-milliseconds) #f #f (hash 'text val))))
      (set-obs! input-obs "")))

  ;; Canvas draw callback
  (define (on-draw dc msgs)
    (define h
      (let-values ([(cw ch) (send dc get-size)])
        ch))
    (send dc set-background bg-c)
    (send dc clear)
    (send dc set-font mono-font)
    (send dc set-text-foreground fg-c)
    (let loop ([rest (if (list? msgs)
                         msgs
                         '())]
               [y 4])
      (cond
        [(null? rest) (void)]
        [(>= (+ y 16) h) (void)]
        [else
         (define m (car rest))
         (define txt
           (cond
             [(hash? m) (hash-ref m 'text (hash-ref m 'content (~a m)))]
             [(string? m) m]
             [else (~a m)]))
         (send dc draw-text txt 4 y)
         (loop (cdr rest) (+ y 16))])))

  ;; Build and render the window (blocks until closed)
  (render
   (window
    #:title (format "q v~a - ~a" q-version (or model-name "q"))
    #:size '(860 640)
    (vpanel
     #:stretch '(#t #t)
     ;; -- Status bar --
     (hpanel #:stretch '(#t #f) #:style '(border) (text-view status-obs))
     ;; -- Transcript canvas --
     (canvas-view messages-obs on-draw #:style '(border vscroll) #:stretch '(#t #t))
     ;; -- Input bar --
     (input-view input-obs on-input #:style '(single) #:stretch '(#t #f) #:min-size '(#f 30)))))

  ;; Cleanup after window closes
  (kill-thread poll-thread)
  (bridge-dispose! bridge))

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

  (define bus (dict-ref rt-config 'event-bus #f))
  (define bridge (wire-bridge! rt-config))
  (define theme (default-theme))
  (define model-name
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (dict-ref rt-config 'model-name #f)))

  ;; Start agent session in a background thread
  (define agent-thread
    (thread
     (lambda ()
       (define prompt
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (dict-ref rt-config 'prompt #f)))
       (when prompt
         (when bus
           (sleep 0.5)
           (publish!
            bus
            (make-event 'user.input (current-inexact-milliseconds) #f #f (hash 'text prompt))))))))

  ;; Launch the GUI window (blocks until window closes)
  (launch-gui-window bridge theme model-name bus)
  (when (thread-running? agent-thread)
    (kill-thread agent-thread)))
