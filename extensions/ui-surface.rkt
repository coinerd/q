#lang racket/base

;; extensions/ui-surface.rkt — UI abstraction layer for extensions
;;
;; Provides parameter-based callbacks that extensions can call without
;; importing TUI internals directly. The TUI layer installs concrete
;; implementations during initialization.
;;
;; This breaks the upward import from extensions/ → tui/ (ARCH-02).
;; M-08 (v0.38.4): Replaced 10 individual parameters with a single
;; ui-callback-registry struct.
;; v0.94.1 (W1.2): Added event-publishing dual-path behind feature flag.

(require racket/contract
         (only-in "../tui/state.rkt" ui-state ui-state? ui-state-extension-widgets)
         (only-in "../ui-core/ui-actions.rkt"
                  current-ui-event-actions-enabled?
                  emit-ui-action!
                  UI-ACTION-HEADER-SET
                  UI-ACTION-HEADER-CLEAR
                  UI-ACTION-FOOTER-SET
                  UI-ACTION-FOOTER-CLEAR
                  UI-ACTION-STATUS-SET
                  UI-ACTION-WIDGET-REGISTER
                  UI-ACTION-WIDGET-UNREGISTER
                  UI-ACTION-WIDGET-UNREGISTER-ALL))

;; ── Event runtime parameter ────────────────────────────────
;; Extensions don't have direct access to the runtime hash.
;; This parameter holds a hash with 'emit-event key (or #f).
(define current-ui-event-runtime (make-parameter #f))

;; Setter/Getter parameter callbacks
;; M-08: Registry struct and accessor (for advanced use)
(provide ui-callback-registry
         ui-callback-registry?

         ;; R-05: Parameterized registry for test isolation
         current-ui-registry

         ;; v0.94.1: Event runtime for dual-path
         current-ui-event-runtime

         ;; Setter/Getter callbacks
         (contract-out [ui-set-footer! (-> box? (or/c string? (listof any/c)) void?)]
                       [ui-set-header! (-> box? (or/c string? (listof any/c)) void?)]
                       [ui-clear-footer! (-> box? void?)]
                       [ui-clear-header! (-> box? void?)]
                       ;; Rendering callbacks
                       [ui-make-styled-line (-> (listof any/c) any/c)]
                       [ui-make-styled-segment (-> string? any/c any/c)]
                       ;; Status message callback (dialog-api)
                       [ui-set-status-message! (-> box? string? void?)]
                       ;; Widget callbacks (widget-api)
                       [ui-set-extension-widget! (-> ui-state? symbol? any/c any/c ui-state?)]
                       [ui-remove-extension-widget! (-> ui-state? symbol? any/c ui-state?)]
                       [ui-remove-all-extension-widgets! (-> ui-state? symbol? ui-state?)]
                       ;; Installation (called by TUI during init)
                       [install-ui-callbacks! (-> hash? void?)]
                       ;; Check if callbacks are installed
                       [ui-callbacks-installed? (-> boolean?)]))

;; ── Callback registry struct ───────────────────────────────

(struct ui-callback-registry
        (set-footer set-header
                    clear-footer
                    clear-header
                    make-styled-line
                    make-styled-segment
                    set-status-message
                    set-extension-widget
                    remove-extension-widget
                    remove-all-extension-widgets)
  #:transparent)

;; R-05: Parameterized registry for test isolation
(define current-ui-registry (make-parameter (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)))

;; ── Event emission helper ──────────────────────────────────

(define (maybe-emit-action! action-name . payload-args)
  (define runtime (current-ui-event-runtime))
  (when (and runtime (current-ui-event-actions-enabled?))
    (apply emit-ui-action! runtime action-name payload-args)))

;; ── Public API ─────────────────────────────────────────────

(define (ui-set-footer! ui-state-box lines)
  (maybe-emit-action! UI-ACTION-FOOTER-SET 'lines lines)
  ((ui-callback-registry-set-footer (current-ui-registry)) ui-state-box lines))

(define (ui-set-header! ui-state-box lines)
  (maybe-emit-action! UI-ACTION-HEADER-SET 'lines lines)
  ((ui-callback-registry-set-header (current-ui-registry)) ui-state-box lines))

(define (ui-clear-footer! ui-state-box)
  (maybe-emit-action! UI-ACTION-FOOTER-CLEAR)
  ((ui-callback-registry-clear-footer (current-ui-registry)) ui-state-box))

(define (ui-clear-header! ui-state-box)
  (maybe-emit-action! UI-ACTION-HEADER-CLEAR)
  ((ui-callback-registry-clear-header (current-ui-registry)) ui-state-box))

(define (ui-make-styled-line segments)
  ((ui-callback-registry-make-styled-line (current-ui-registry)) segments))

(define (ui-make-styled-segment text style)
  ((ui-callback-registry-make-styled-segment (current-ui-registry)) text style))

;; Set status message in the ui-state box (dialog-api notification integration)
(define (ui-set-status-message! ui-state-box message)
  (maybe-emit-action! UI-ACTION-STATUS-SET 'status message)
  (define cb (ui-callback-registry-set-status-message (current-ui-registry)))
  (when cb
    (cb ui-state-box message)))

;; Set a widget from an extension
;; Falls back to direct struct-copy when no TUI callback is installed
(define (ui-set-extension-widget! state ext-name key lines)
  (maybe-emit-action! UI-ACTION-WIDGET-REGISTER
                      'ext-name
                      ext-name
                      'key
                      key
                      'descriptor
                      (hash 'type 'text 'content lines))
  (define cb (ui-callback-registry-set-extension-widget (current-ui-registry)))
  (if cb
      (cb state ext-name key lines)
      ;; Fallback: directly update the extension-widgets hash
      (let* ([widgets (ui-state-extension-widgets state)]
             [new-widgets (hash-set widgets (cons ext-name key) lines)])
        (struct-copy ui-state state [extension-widgets new-widgets]))))

;; Remove a specific widget
(define (ui-remove-extension-widget! state ext-name key)
  (maybe-emit-action! UI-ACTION-WIDGET-UNREGISTER 'ext-name ext-name 'key key)
  (define cb (ui-callback-registry-remove-extension-widget (current-ui-registry)))
  (if cb
      (cb state ext-name key)
      (let* ([widgets (ui-state-extension-widgets state)]
             [new-widgets (hash-remove widgets (cons ext-name key))])
        (struct-copy ui-state state [extension-widgets new-widgets]))))

;; Remove all widgets for an extension
(define (ui-remove-all-extension-widgets! state ext-name)
  (maybe-emit-action! UI-ACTION-WIDGET-UNREGISTER-ALL 'ext-name ext-name)
  (define cb (ui-callback-registry-remove-all-extension-widgets (current-ui-registry)))
  (if cb
      (cb state ext-name)
      (let* ([widgets (ui-state-extension-widgets state)]
             [new-widgets (for/hash ([(k v) (in-hash widgets)]
                                     #:when (not (equal? (car k) ext-name)))
                            (values k v))])
        (struct-copy ui-state state [extension-widgets new-widgets]))))

(define (ui-callbacks-installed?)
  (define r (current-ui-registry))
  (and (ui-callback-registry-set-footer r)
       (ui-callback-registry-set-header r)
       (ui-callback-registry-clear-footer r)
       (ui-callback-registry-clear-header r)
       (ui-callback-registry-make-styled-line r)
       (ui-callback-registry-make-styled-segment r)))

;; install-ui-callbacks! : hash? -> void?
(define (install-ui-callbacks! callbacks)
  (current-ui-registry (ui-callback-registry (hash-ref callbacks 'set-footer #f)
                                             (hash-ref callbacks 'set-header #f)
                                             (hash-ref callbacks 'clear-footer #f)
                                             (hash-ref callbacks 'clear-header #f)
                                             (hash-ref callbacks 'make-styled-line #f)
                                             (hash-ref callbacks 'make-styled-segment #f)
                                             (hash-ref callbacks 'set-status-message #f)
                                             (hash-ref callbacks 'set-extension-widget #f)
                                             (hash-ref callbacks 'remove-extension-widget #f)
                                             (hash-ref callbacks 'remove-all-extension-widgets #f))))
