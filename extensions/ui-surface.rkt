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

(require racket/contract
         (only-in "../tui/state.rkt" ui-state ui-state-extension-widgets))

;; Setter/Getter parameter callbacks
(provide ui-set-footer!
         ui-set-header!
         ui-clear-footer!
         ui-clear-header!

         ;; Rendering callbacks
         ui-make-styled-line
         ui-make-styled-segment

         ;; Status message callback (dialog-api)
         ui-set-status-message!

         ;; Widget callbacks (widget-api)
         ui-set-extension-widget!
         ui-remove-extension-widget!
         ui-remove-all-extension-widgets!

         ;; Installation (called by TUI during init)
         install-ui-callbacks!

         ;; Check if callbacks are installed
         ui-callbacks-installed?

         ;; M-08: Registry struct and accessor (for advanced use)
         ui-callback-registry
         ui-callback-registry?)

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

;; Global mutable registry (replaces 10 individual parameters).
(define *ui-registry* (box (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)))

;; ── Public API ─────────────────────────────────────────────

(define (ui-set-footer! ui-state-box lines)
  ((ui-callback-registry-set-footer (unbox *ui-registry*)) ui-state-box lines))

(define (ui-set-header! ui-state-box lines)
  ((ui-callback-registry-set-header (unbox *ui-registry*)) ui-state-box lines))

(define (ui-clear-footer! ui-state-box)
  ((ui-callback-registry-clear-footer (unbox *ui-registry*)) ui-state-box))

(define (ui-clear-header! ui-state-box)
  ((ui-callback-registry-clear-header (unbox *ui-registry*)) ui-state-box))

(define (ui-make-styled-line segments)
  ((ui-callback-registry-make-styled-line (unbox *ui-registry*)) segments))

(define (ui-make-styled-segment text style)
  ((ui-callback-registry-make-styled-segment (unbox *ui-registry*)) text style))

;; Set status message in the ui-state box (dialog-api notification integration)
;; ui-state-box: box? containing ui-state
;; message: string? — the formatted status message
(define (ui-set-status-message! ui-state-box message)
  (define cb (ui-callback-registry-set-status-message (unbox *ui-registry*)))
  (when cb
    (cb ui-state-box message)))

;; Set a widget from an extension
;; Falls back to direct struct-copy when no TUI callback is installed
(define (ui-set-extension-widget! state ext-name key lines)
  (define cb (ui-callback-registry-set-extension-widget (unbox *ui-registry*)))
  (if cb
      (cb state ext-name key lines)
      ;; Fallback: directly update the extension-widgets hash
      (let* ([widgets (ui-state-extension-widgets state)]
             [new-widgets (hash-set widgets (cons ext-name key) lines)])
        (struct-copy ui-state state [extension-widgets new-widgets]))))

;; Remove a specific widget
(define (ui-remove-extension-widget! state ext-name key)
  (define cb (ui-callback-registry-remove-extension-widget (unbox *ui-registry*)))
  (if cb
      (cb state ext-name key)
      (let* ([widgets (ui-state-extension-widgets state)]
             [new-widgets (hash-remove widgets (cons ext-name key))])
        (struct-copy ui-state state [extension-widgets new-widgets]))))

;; Remove all widgets for an extension
(define (ui-remove-all-extension-widgets! state ext-name)
  (define cb (ui-callback-registry-remove-all-extension-widgets (unbox *ui-registry*)))
  (if cb
      (cb state ext-name)
      (let* ([widgets (ui-state-extension-widgets state)]
             [new-widgets (for/hash ([(k v) (in-hash widgets)]
                                     #:when (not (equal? (car k) ext-name)))
                            (values k v))])
        (struct-copy ui-state state [extension-widgets new-widgets]))))

(define (ui-callbacks-installed?)
  (define r (unbox *ui-registry*))
  (and (ui-callback-registry-set-footer r)
       (ui-callback-registry-set-header r)
       (ui-callback-registry-clear-footer r)
       (ui-callback-registry-clear-header r)
       (ui-callback-registry-make-styled-line r)
       (ui-callback-registry-make-styled-segment r)))

;; install-ui-callbacks! : hash? -> void?
;; Installs concrete UI implementations.
;; Expected keys: set-footer, set-header, clear-footer, clear-header,
;;                make-styled-line, make-styled-segment,
;;                set-status-message,
;;                set-extension-widget, remove-extension-widget,
;;                remove-all-extension-widgets
(define (install-ui-callbacks! callbacks)
  (set-box! *ui-registry*
            (ui-callback-registry (hash-ref callbacks 'set-footer #f)
                                  (hash-ref callbacks 'set-header #f)
                                  (hash-ref callbacks 'clear-footer #f)
                                  (hash-ref callbacks 'clear-header #f)
                                  (hash-ref callbacks 'make-styled-line #f)
                                  (hash-ref callbacks 'make-styled-segment #f)
                                  (hash-ref callbacks 'set-status-message #f)
                                  (hash-ref callbacks 'set-extension-widget #f)
                                  (hash-ref callbacks 'remove-extension-widget #f)
                                  (hash-ref callbacks 'remove-all-extension-widgets #f))))
