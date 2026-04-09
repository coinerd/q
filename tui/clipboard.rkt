#lang racket/base

;; q/tui/clipboard.rkt — Clipboard support for the TUI
;;
;; Provides clipboard copy functionality with multiple backends:
;;   - OSC 52 escape sequence (primary, works over SSH)
;;   - System clipboard tools (wl-copy, xclip, xsel, pbcopy)
;;
;; Controlled via `current-clipboard-mode` parameter:
;;   'auto   — try system tool first, fall back to OSC 52
;;   'osc52  — only use OSC 52 escape sequence
;;   'system — only use system clipboard tools
;;   'off    — disable clipboard entirely

(require racket/port
         net/base64)

(provide
 ;; Configuration
 current-clipboard-mode

 ;; Public API
 copy-text!
 copy-selection!
 clipboard-backend-available?

 ;; Status symbols (documented, not exported as values)
 ;; 'ok-osc52   — OSC 52 sequence emitted successfully
 ;; 'ok-system  — system clipboard tool succeeded
 ;; 'disabled   — current-clipboard-mode is 'off
 ;; 'unavailable — no backend available for the selected mode
 ;; 'failed     — backend reported failure

 ;; Re-exported for backward compatibility / testing
 detect-clipboard-tool
 clipboard-copy-via-tool
 osc-52-copy)

;; ============================================================
;; Configuration
;; ============================================================

;; Controls which clipboard backend to use.
;; 'auto   — system tool first, then OSC 52
;; 'osc52  — only OSC 52
;; 'system — only system tools
;; 'off    — no clipboard
(define current-clipboard-mode (make-parameter 'auto))

;; ============================================================
;; Backend detection
;; ============================================================

;; Detect an available platform clipboard tool.
;; Checks environment variables to match tools to the active display system:
;;   WAYLAND_DISPLAY set → try wl-copy first
;;   DISPLAY set → try xclip/xsel first
;;   macOS → pbcopy (always tried)
;; Returns: (list tool-path tool-name) or #f
(define (detect-clipboard-tool)
  (define wayland? (getenv "WAYLAND_DISPLAY"))
  (define x11? (getenv "DISPLAY"))
  (cond
    ;; macOS always first
    [(find-executable-path "pbcopy") => (lambda (p) (list p 'pbcopy))]
    ;; Wayland session
    [(and wayland? (find-executable-path "wl-copy"))
     => (lambda (p) (list p 'wl-copy))]
    ;; X11 session
    [(and x11? (find-executable-path "xclip"))
     => (lambda (p) (list p 'xclip))]
    [(and x11? (find-executable-path "xsel"))
     => (lambda (p) (list p 'xsel))]
    ;; Fallback: try all tools regardless of env (covers unusual setups)
    [(find-executable-path "wl-copy") => (lambda (p) (list p 'wl-copy))]
    [(find-executable-path "xclip")   => (lambda (p) (list p 'xclip))]
    [(find-executable-path "xsel")    => (lambda (p) (list p 'xsel))]
    [else #f]))

;; Returns #t if at least one clipboard backend is available.
;; OSC 52 is always considered available (can't verify terminal support).
(define (clipboard-backend-available?)
  #t)

;; ============================================================
;; OSC 52 backend
;; ============================================================

;; Emit OSC 52 escape sequence to copy text.
;; Writes to the given output port (defaults to current-output-port).
;; ESC ] 52 ; c ; <base64> ST
;; Uses ESC \ as ST (String Terminator) for broad compatibility.
(define (osc-52-copy text [out (current-output-port)])
  (define b64 (base64-encode (string->bytes/utf-8 text) #""))
  (display (string-append "\x1b]52;c;" (bytes->string/latin-1 b64) "\x1b\\") out)
  (flush-output out))

;; ============================================================
;; System clipboard backend
;; ============================================================

;; Copy text to clipboard via a platform tool (subprocess).
;; Returns #t on success, #f on failure.
(define (clipboard-copy-via-tool tool-path tool-name text)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define args
      (case tool-name
        [(pbcopy) '()]
        [(wl-copy) '()]
        [(xclip)  '("-selection" "clipboard")]
        [(xsel)   '("--clipboard" "--input")]
        [else '()]))
    (define-values (sp out in err)
      (apply subprocess #f #f #f tool-path args))
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (display text in)
        (close-output-port in)
        ;; Wait up to 2 seconds for clipboard tool to finish
        (sync/timeout 2.0 sp))
      (lambda ()
        (close-input-port out)
        (close-input-port err)))
    (= (subprocess-status sp) 0)))

;; ============================================================
;; Main API
;; ============================================================

;; Copy text to clipboard using the configured backend.
;; Returns a status symbol:
;;   'ok-osc52    — OSC 52 sequence emitted
;;   'ok-system   — system clipboard tool succeeded
;;   'disabled    — mode is 'off
;;   'unavailable — no backend available for selected mode
;;   'failed      — backend reported failure
(define (copy-text! text [out (current-output-port)])
  (define mode (current-clipboard-mode))
  (cond
    [(eq? mode 'off) 'disabled]
    [(eq? mode 'osc52)
     (with-handlers ([exn:fail? (lambda (e) 'failed)])
       (osc-52-copy text out)
       'ok-osc52)]
    [(eq? mode 'system)
     (define tool (detect-clipboard-tool))
     (cond
       [(not tool) 'unavailable]
       [(clipboard-copy-via-tool (car tool) (cadr tool) text) 'ok-system]
       [else 'failed])]
    ;; 'auto — try system tool first, then OSC 52
    [else
     (define tool (detect-clipboard-tool))
     (cond
       [(and tool (clipboard-copy-via-tool (car tool) (cadr tool) text))
        ;; System tool succeeded — also emit OSC 52 for SSH sessions
        (with-handlers ([exn:fail? (lambda (e) (void))])
          (osc-52-copy text out))
        'ok-system]
       [else
        ;; No system tool or it failed — OSC 52 fallback
        (with-handlers ([exn:fail? (lambda (e) 'failed)])
          (osc-52-copy text out)
          'ok-osc52)])]))

;; Copy the current selection from the TUI context to clipboard.
;; ctx must have selection-text and has-selection? available.
;; Returns a status symbol (same as copy-text!).
(define (copy-selection! ctx get-selection-text has-selection?)
  (if (not (has-selection?))
      'disabled
      (let ([text (get-selection-text ctx)])
        (if (or (not text) (string=? text ""))
            'disabled
            (copy-text! text)))))
