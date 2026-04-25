#lang racket/base

;; extensions/ui-surface.rkt — UI abstraction layer for extensions
;;
;; Provides parameter-based callbacks that extensions can call without
;; importing TUI internals directly. The TUI layer installs concrete
;; implementations during initialization.
;;
;; This breaks the upward import from extensions/ → tui/ (ARCH-02).

(require racket/contract)

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
         ui-callbacks-installed?)

;; ── Callback parameters ────────────────────────────────────

;; Each parameter holds a procedure (or #f if not yet installed).

(define ui-set-footer-param (make-parameter #f))
(define ui-set-header-param (make-parameter #f))
(define ui-clear-footer-param (make-parameter #f))
(define ui-clear-header-param (make-parameter #f))
(define ui-make-styled-line-param (make-parameter #f))
(define ui-make-styled-segment-param (make-parameter #f))
(define ui-set-status-message-param (make-parameter #f))
(define ui-set-extension-widget-param (make-parameter #f))
(define ui-remove-extension-widget-param (make-parameter #f))
(define ui-remove-all-extension-widgets-param (make-parameter #f))

;; ── Public API ─────────────────────────────────────────────

(define (ui-set-footer! ui-state-box lines)
  ((ui-set-footer-param) ui-state-box lines))

(define (ui-set-header! ui-state-box lines)
  ((ui-set-header-param) ui-state-box lines))

(define (ui-clear-footer! ui-state-box)
  ((ui-clear-footer-param) ui-state-box))

(define (ui-clear-header! ui-state-box)
  ((ui-clear-header-param) ui-state-box))

(define (ui-make-styled-line segments)
  ((ui-make-styled-line-param) segments))

(define (ui-make-styled-segment text style)
  ((ui-make-styled-segment-param) text style))

;; Set status message in the ui-state box (dialog-api notification integration)
;; ui-state-box: box? containing ui-state
;; message: string? — the formatted status message
(define (ui-set-status-message! ui-state-box message)
  (define cb (ui-set-status-message-param))
  (when cb
    (cb ui-state-box message)))

;; Set a widget from an extension
(define (ui-set-extension-widget! state ext-name key lines)
  (define cb (ui-set-extension-widget-param))
  (if cb
      (cb state ext-name key lines)
      state))

;; Remove a specific widget
(define (ui-remove-extension-widget! state ext-name key)
  (define cb (ui-remove-extension-widget-param))
  (if cb
      (cb state ext-name key)
      state))

;; Remove all widgets for an extension
(define (ui-remove-all-extension-widgets! state ext-name)
  (define cb (ui-remove-all-extension-widgets-param))
  (if cb
      (cb state ext-name)
      state))

(define (ui-callbacks-installed?)
  (and (ui-set-footer-param)
       (ui-set-header-param)
       (ui-clear-footer-param)
       (ui-clear-header-param)
       (ui-make-styled-line-param)
       (ui-make-styled-segment-param)))

;; install-ui-callbacks! : hash? -> void?
;; Installs concrete UI implementations.
;; Expected keys: set-footer, set-header, clear-footer, clear-header,
;;                make-styled-line, make-styled-segment,
;;                set-status-message,
;;                set-extension-widget, remove-extension-widget,
;;                remove-all-extension-widgets
(define (install-ui-callbacks! callbacks)
  (when (hash-ref callbacks 'set-footer #f)
    (ui-set-footer-param (hash-ref callbacks 'set-footer)))
  (when (hash-ref callbacks 'set-header #f)
    (ui-set-header-param (hash-ref callbacks 'set-header)))
  (when (hash-ref callbacks 'clear-footer #f)
    (ui-clear-footer-param (hash-ref callbacks 'clear-footer)))
  (when (hash-ref callbacks 'clear-header #f)
    (ui-clear-header-param (hash-ref callbacks 'clear-header)))
  (when (hash-ref callbacks 'make-styled-line #f)
    (ui-make-styled-line-param (hash-ref callbacks 'make-styled-line)))
  (when (hash-ref callbacks 'make-styled-segment #f)
    (ui-make-styled-segment-param (hash-ref callbacks 'make-styled-segment)))
  (when (hash-ref callbacks 'set-status-message #f)
    (ui-set-status-message-param (hash-ref callbacks 'set-status-message)))
  (when (hash-ref callbacks 'set-extension-widget #f)
    (ui-set-extension-widget-param (hash-ref callbacks 'set-extension-widget)))
  (when (hash-ref callbacks 'remove-extension-widget #f)
    (ui-remove-extension-widget-param (hash-ref callbacks 'remove-extension-widget)))
  (when (hash-ref callbacks 'remove-all-extension-widgets #f)
    (ui-remove-all-extension-widgets-param (hash-ref callbacks 'remove-all-extension-widgets))))
