#lang racket/base

;; q/gui/main.rkt — GUI entry point
;;
;; Thin facade: delegates state synchronization to gui/state-sync.rkt
;; and window construction to gui-easy views.

(require racket/contract
         racket/match
         racket/dict
         racket/format
         racket/class
         racket/runtime-path
         racket/string
         "../util/event/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../ui-core/observable-bridge.rkt"
         "../ui-core/dispatch.rkt"
         "../ui-core/theme-protocol.rkt"
         "../util/event/event.rkt"
         "../util/version.rkt"
         "../extensions/hooks.rkt"
         "../tui/command-parse.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../gui/components/input-helpers.rkt"
         "../gui/slash-commands.rkt"
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../ui-core/ui-intents.rkt"
         "../ui-core/preferences.rkt"
         (only-in "../ui-core/ui-actions.rkt"
                  current-ui-event-actions-enabled?
                  wire-ui-event-actions-from-config!)
         (only-in "../runtime/settings-query.rkt" setting-ref*)
         (only-in "lifecycle-hooks.rkt" dispatch-gui-hook! current-gui-event-runtime)
         (only-in "theme-manager.rkt" make-theme-manager theme-manager?)
         (only-in "../extensions/ui-surface.rkt" install-ui-callbacks!))
(require (only-in "../util/error/error-helpers.rkt" with-safe-fallback))

(provide (contract-out [run-gui-with-runtime (-> any/c any/c void?)]
                       [run-gui (-> void?)]
                       [gui-available? (-> boolean?)]
                       [load-gui-action-handler-factory (-> procedure?)])
         gui-resolve-session)

;; Stable module-path index for the GUI action adapter.  A plain string passed
;; to dynamic-require is resolved relative to the process current-directory;
;; this index resolves relative to this source module instead.
(define-runtime-module-path-index gui-action-adapter-module "ui-action-adapter.rkt")

(define (load-gui-action-handler-factory)
  (dynamic-require gui-action-adapter-module 'make-gui-action-handler))

;; --------------------------------------------------
;; Check if GUI is available
;; DESIGN FACT (W6, v0.99.38 adapter audit): This is the GUI adapter layer's
;; environment gate. DISPLAY or WAYLAND_DISPLAY must be set, and the gui/easy
;; libraries must be loadable. The dynamic-require with #f probe avoids hard
;; dependency on racket/gui at compile time (optional adapter pattern).
;; --------------------------------------------------
(define (gui-available?)
  (and (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))
       (with-safe-fallback #f
                           (dynamic-require 'racket/gui/easy/observable #f)
                           (dynamic-require 'racket/gui/easy/view #f)
                           (dynamic-require 'racket/gui/easy/renderer #f)
                           #t)))

;; GAP-TM (v0.98.8 W1): Theme manager parameter for cross-frontend synchronization.
;; Instantiated in run-gui-with-runtime; available to state-sync and action adapters.
;; DELTA-SET-THEME is wired via gui/ui-action-adapter.rkt since v0.98.11 W0.
(define current-gui-theme-manager (make-parameter #f))

(provide current-gui-theme-manager)

;; --------------------------------------------------
;; W4 (v0.99.96): shared composer input path.
;;
;; The GUI runtime input route now goes through the SAME semantic model as
;; the TUI: the shared composer state (q/ui-core/composer-model.rkt) holds
;; buffer/cursor/history-intent; named intents (q/ui-core/ui-intents.rkt)
;; decide submit/newline/history; the shared preference surface
;; (q/ui-core/preferences.rkt) decides WHICH key means what.  The GUI keeps
;; its native multiline control, selection and rich rendering — only the
;; SEMANTICS are shared.
;; --------------------------------------------------

;; Pure: fold one gui-easy input action into the composer state machine.
;; Returns (values intent draft) where intent is a ui-intent? (or #f) and
;; draft is the next draft state.  Headless-testable; identical semantics
;; to the TUI key path.
(define (gui-composer-event st
                            action
                            val
                            #:history [history '()]
                            #:history-index [idx 0]
                            #:prefs [prefs (default-preferences)])
  (define (with-st st*)
    (values #f st*))
  (case action
    ;; Whole-field change from the native control: update the shared draft.
    [(input change) (with-st (gui-draft-update st (or val "")))]
    [(return)
     (cond
       ;; Enter alone: submit intent with the prepared snapshot.
       [(input-key-should-submit? 'return #f #f)
        (define-values (text cleared) (gui-draft-submit st))
        (values (make-composer-submit-intent text) cleared)]
       [else (with-st (gui-draft-insert-newline st))])]
    [(newline) (with-st (gui-draft-insert-newline st))]
    [(history-up)
     (define-values (st* idx* text) (gui-draft-history st idx history 'up))
     (values (and text (make-composer-history-intent 'up))
             (if text
                 (gui-draft-update st* text)
                 st*))]
    [(history-down)
     (define-values (st* idx* text) (gui-draft-history st idx history 'down))
     (values (and text (make-composer-history-intent 'down))
             (if text
                 (gui-draft-update st* text)
                 st*))]
    [else (with-st st)]))

;; Pure: map a raw key event to a named intent using the SHARED keymap.
;; Used by the GUI control's on-char hook so shortcuts resolve to the same
;; intents the TUI resolves for the same physical key.
(define (gui-key-event->intent key-code
                               #:shift? [shift? #f]
                               #:control? [control? #f]
                               #:alt? [alt? #f]
                               #:at-start? [at-start? 'no]
                               #:at-end? [at-end? 'no]
                               #:target [target #f]
                               #:prefs [prefs (default-preferences)])
  (gui-key->intent key-code
                   #:shift? shift?
                   #:control? control?
                   #:alt? alt?
                   #:at-start? at-start?
                   #:at-end? at-end?
                   #:target target
                   #:prefs prefs))

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
  (call-with-semaphore gui-state-lock (lambda () (set-box! notify-callback-box notify-gui!)))

  ;; Both the visible disclosure affordance and Ctrl+O resolve to a targeted
  ;; toggle-detail intent, then use the same state transition path.
  (define (toggle-detail!)
    (define changed? #f)
    (call-with-semaphore gui-state-lock
                         (lambda ()
                           (define old (unbox state-box))
                           ;; Untargeted Ctrl+O delegates focused/active/latest
                           ;; selection to the same gui-state resolver used by
                           ;; visible targeted disclosure actions.
                           (define intent (gui-key-event->intent #\o #:control? #t #:target #f))
                           (when (toggle-detail-intent? intent)
                             (define next (gui-state-apply-intent old intent))
                             (unless (eq? next old)
                               (set-box! state-box next)
                               (set! changed? #t)))))
    (when changed?
      (notify-gui!)))

  ;; Slash command handler (extracted to slash-commands.rkt)
  (define handle-slash-command (make-slash-command-handler sess state-box gui-state-lock notify-gui!))

  ;; W4 (v0.99.96): shared composer state for the runtime input path.
  ;; The GUI keeps its native control (selection, rendering); the SEMANTICS
  ;; (draft text, submit/newline policy, history intent) go through the same
  ;; modules the TUI uses: input-helpers (contract) -> composer-model (state)
  ;; -> ui-intents (named intents) -> preferences (key mapping).
  (define composer-box (box (make-gui-draft)))
  (define composer-history '())
  (define composer-history-idx 0)
  (define keymap% (dynamic-require 'racket/gui 'keymap%))
  ;; W3 (v1.00.02): the GUI starts from the SAME loaded preference snapshot
  ;; as the TUI (user config file -> merge over defaults).  Installing it as
  ;; the ambient parameter means disclosure preview length (W2 rendering,
  ;; collapsed-preview-lines) and every leaf consumer read live config.
  (define gui-prefs (load-preferences))
  (current-preferences gui-prefs)
  ;; Multiline is on by default; off-ramp for rollout is the env var below.
  (define composer-multiline? (not (equal? (getenv "Q_GUI_MULTILINE") "0")))

  ;; Shared: submit the (already prepared) snapshot; mirrors the TUI path.
  (define (composer-submit! text)
    (when (> (string-length text) 0)
      (define trimmed (string-trim text))
      (cond
        [(and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\/))
         (handle-slash-command text)
         (set-obs! input-obs "")]
        [else
         (publish! event-bus
                   (make-event "user.input" (current-inexact-milliseconds) #f #f (hash 'text text)))
         (thread (lambda ()
                   (with-handlers ([exn:fail? (lambda (e)
                                                (call-with-semaphore
                                                 gui-state-lock
                                                 (lambda ()
                                                   (define old (unbox state-box))
                                                   (set-box! state-box
                                                             (gui-state-set-status
                                                              (gui-state-add-message
                                                               old
                                                               (make-gui-message "error"
                                                                                 (exn-message e)))
                                                              'error)))))])
                     (run-prompt! sess text))))
         (set-obs! input-obs "")])))

  ;; Shared: fold the draft through the semantic model, then act on intent.
  (define (apply-composer-action! action [val #f])
    (define-values (intent draft)
      (gui-composer-event (unbox composer-box)
                          action
                          val
                          #:history composer-history
                          #:history-index composer-history-idx
                          #:prefs gui-prefs))
    (set-box! composer-box draft)
    (define text (gui-draft-text draft))
    ;; Keep the native control and the shared draft in sync (single source of
    ;; truth for the buffer is the native editor; the draft mirrors it).
    (unless (or (eq? action 'input) (eq? action 'change) (not val))
      (set-obs! input-obs text))
    (match intent
      [(composer-submit-intent t) (composer-submit! t)]
      [(composer-history-intent dir)
       (set! composer-history-idx
             (if (eq? dir 'up)
                 (history-index-back composer-history-idx)
                 (history-index-forward composer-history-idx composer-history)))]
      [_ (void)]))

  ;; Input callback from the native control.
  ;; Single-line mode fires 'return on Enter (legacy path preserved).
  ;; Multiline mode routes every action through apply-composer-action!.
  (define (on-input action val)
    (cond
      [(eq? action 'return)
       (if composer-multiline?
           (apply-composer-action! 'return val)
           (let ()
             ;; Legacy single-line compatibility path (feature-flag off).
             (set-box! composer-box (gui-draft-update (unbox composer-box) (or val "")))
             (define-values (text _cleared) (gui-draft-submit (unbox composer-box)))
             (composer-submit! text)
             (set-box! composer-box (make-gui-draft))))]
      [(eq? action 'input)
       (set-box! composer-box (gui-draft-update (unbox composer-box) (or val "")))]
      [else (void)]))

  ;; Multiline keymap: Enter submits, Shift+Enter / Ctrl+Enter insert a
  ;; newline (the gui helper contract), Alt+Up/Down walk history.  Every
  ;; mapping goes through the shared preference surface, not ad-hoc code.
  (define composer-keymap
    (and
     composer-multiline?
     (let ([km (make-object keymap%)])
       (send km add-function
             "q-composer-submit"
             (lambda (editor _event)
               (queue-callback (lambda () (apply-composer-action! 'return (send editor get-text))))))
       (send km add-function "q-composer-newline" (lambda (editor _event) (send editor insert "\n")))
       (send km add-function
             "q-composer-history-up"
             (lambda (editor _event)
               (queue-callback (lambda ()
                                 (apply-composer-action! 'history-up (send editor get-text))))))
       (send km add-function
             "q-composer-history-down"
             (lambda (editor _event)
               (queue-callback (lambda ()
                                 (apply-composer-action! 'history-down (send editor get-text))))))
       (send km add-function
             "q-transcript-toggle-detail"
             (lambda (_editor _event) (queue-callback toggle-detail!)))
       (when (submit-key-policy gui-prefs)
         (send km map-function "return" "q-composer-submit"))
       (send km map-function "c:return" "q-composer-newline")
       (send km map-function "c:o" "q-transcript-toggle-detail")
       (send km map-function "a:up" "q-composer-history-up")
       (send km map-function "a:down" "q-composer-history-down")
       km)))

  ;; Observable wrapping the text% editor for editor-canvas view
  (define transcript-obs (make-obs transcript-text))

  ;; Mixin to set dark background on editor-canvas% — just call method after init
  (define ((editor-canvas-bg-mixin bg-color) base%)
    (class base%
      (super-new)
      (send this set-canvas-background bg-color)))

  ;; Mixin: enable clipboard shortcuts (Ctrl+C, Ctrl+A) on the editor-canvas%
  (define ((editor-canvas-clipboard-mixin) base%)
    (class base%
      (super-new)
      (define/override (on-char event)
        (cond
          [(and (send event get-control-down) (eq? (send event get-key-code) #\c))
           (define ed (send this get-editor))
           (when ed
             (send ed copy))]
          [(and (send event get-control-down) (eq? (send event get-key-code) #\a))
           (define ed (send this get-editor))
           (when ed
             (send ed set-position 0 (send ed last-position)))]
          [(and (send event get-control-down) (eq? (send event get-key-code) #\o)) (toggle-detail!)]
          [else (super on-char event)]))))

  ;; Compose multiple mixins into one (right-to-left application)
  (define (compose-mixins . mixins)
    (lambda (base%)
      (for/fold ([b base%]) ([m (in-list (reverse mixins))])
        (m b))))

  ;; Build and render the window (blocks until closed)
  ;; GAP-LH (v0.98.7 W1): Dispatch gui.window.opened lifecycle hook.
  ;; dispatch-gui-hook! has built-in error isolation (handlers wrapped in with-handlers).
  (dispatch-gui-hook! 'gui.window.opened (hasheq 'session-id (session-id sess) 'model model-name))
  (render #:wait? #t
          (window #:title (format "q v~a - ~a" q-version (or model-name "q"))
                  #:size '(860 640)
                  (vpanel #:stretch '(#t #t)
                          (hpanel #:stretch '(#t #f) #:style '(border) (text-view status-obs))
                          (editor-canvas-view transcript-obs
                                              #:min-size '(#f 200)
                                              #:stretch '(#t #t)
                                              #:mixin (compose-mixins (editor-canvas-clipboard-mixin)
                                                                      (editor-canvas-bg-mixin bg-c)))
                          (input-view input-obs
                                      on-input
                                      #:style (if composer-multiline?
                                                  '(multiple)
                                                  '())
                                      #:keymap composer-keymap
                                      #:min-size (list #f
                                                       (if composer-multiline?
                                                           (* 19 (max-composer-rows gui-prefs))
                                                           #f))
                                      #:stretch '(#t #f)))))
  ;; GAP-LH (v0.98.7 W1): Dispatch gui.window.closed lifecycle hook after window closes.
  (dispatch-gui-hook! 'gui.window.closed (hasheq 'session-id (session-id sess)))

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

;; F-11/A-09 (W4): GUI must resume the EXACT requested session instead of
;; unconditionally creating a new sibling. This pure seam delegates to the
;; canonical open-or-resume resolver and is unit-testable without a display.
(define (gui-resolve-session rt-config)
  (open-or-resume-session rt-config))

(define (run-gui-with-runtime rt-config cfg)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (eprintf "Install gui-easy-lib: raco pkg install gui-easy-lib\n")
    (exit 1))

  ;; GAP-EA (v0.98.7 W0): Wire UI event actions flag from config.json.
  ;; Reads "ui.event-actions.enabled" from settings; default #f = zero behavior change.
  (define settings (dict-ref rt-config 'settings #f))
  (wire-ui-event-actions-from-config! settings)

  ;; Create agent session — F-11/A-09: resume the requested session when an
  ;; id is present (no sibling), else create a new one.
  (define sess (gui-resolve-session rt-config))
  (define bus (dict-ref rt-config 'event-bus #f))
  (define theme (default-theme))
  (define model-name (dict-ref rt-config 'model-name #f))

  ;; GAP-LH (v0.98.7 W1): Set runtime parameter for lifecycle hook event emission.
  (current-gui-event-runtime rt-config)

  ;; GAP-TM (v0.98.8 W1): Instantiate theme manager for cross-frontend theme sync.
  ;; make-theme-manager is a pure constructor — no side effects.
  (define tm (make-theme-manager theme))
  (current-gui-theme-manager tm)

  ;; GUI state: accumulated messages + status
  (define state-box (box (make-gui-state #:model model-name)))

  ;; PIPE-02 (v0.98.13): Install UI extension callbacks for GUI.
  ;; Most callbacks are no-ops (GUI lacks footer/header/extension-widget primitives).
  ;; set-status-message is the critical callback — updates the status bar observable.
  (install-ui-callbacks! (hasheq 'set-footer
                                 (lambda (box lines) (void))
                                 'set-header
                                 (lambda (box lines) (void))
                                 'clear-footer
                                 (lambda (box) (void))
                                 'clear-header
                                 (lambda (box) (void))
                                 'make-styled-line
                                 (lambda (segments) segments)
                                 'make-styled-segment
                                 (lambda (text style) (cons text style))
                                 'set-status-message
                                 (lambda (box msg)
                                   (define safe-msg
                                     (cond
                                       [(string? msg) (string->symbol msg)]
                                       [(symbol? msg) msg]
                                       [else 'idle]))
                                   (set-box! box (gui-state-set-status (unbox box) safe-msg)))
                                 'set-extension-widget
                                 (lambda args (void))
                                 'remove-extension-widget
                                 (lambda args (void))
                                 'remove-all-extension-widgets
                                 (lambda args (void))))

  ;; Notify callback box — set by launch-gui-window after GUI thread starts
  (define notify-callback-box (box #f))

  ;; Subscribe our event handler to the bus (with notify-callback-box)
  (when bus
    (subscribe! bus (make-gui-event-subscriber state-box notify-callback-box))
    ;; PIPE-01 (v0.98.13): Subscribe action handler for ui.* events.
    ;; Converts event structs to hashes, then dispatches to gui-delta-handlers.
    ;; Events only emitted when ui.event-actions.enabled is #t in config.
    ;; Uses dynamic-require to avoid circular dependency (ui-action-adapter imports from this module).
    ;; F2 (v0.98.13 audit): Wrapped in with-handlers to prevent GUI crash on load failure.
    (with-handlers ([exn:fail? (lambda (e)
                                 (fprintf (current-error-port)
                                          "v0.98.13: GUI action handler load failed: ~a\n"
                                          (exn-message e)))])
      (define gui-action-handler ((load-gui-action-handler-factory) state-box notify-callback-box))
      (subscribe! bus
                  (lambda (evt)
                    (define ev (event-ev evt))
                    (define payload (event-payload evt))
                    (gui-action-handler (if (hash? payload)
                                            (hash-set payload 'type ev)
                                            (hasheq 'type ev 'payload payload)))))))

  ;; If there's an initial prompt from CLI, run it after GUI starts
  (define initial-prompt (with-safe-fallback #f (dict-ref rt-config 'prompt #f)))

  ;; Launch the GUI window (blocks until closed)
  (launch-gui-window state-box sess bus theme model-name notify-callback-box)

  ;; Cleanup
  (close-session! sess))
