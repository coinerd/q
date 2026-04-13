#lang racket/base

;; q/cli/render.rkt — Terminal rendering helpers
;;
;; Extracted from interfaces/cli.rkt for modularity (Issue #193).
;;
;; Provides:
;;   format-event-for-terminal — event → terminal string
;;   render-markdown            — markdown → ANSI-styled text
;;   render-tokens, render-token — token-level rendering
;;   make-stream-markdown-writer — line-buffered streaming renderer
;;   truncate-string            — string truncation helper
;;   tool-result-content->string — tool result content extraction
;;   MAX-TOOL-DISPLAY-LEN       — constant
;;   format-classified-error    — error formatting

(require "../util/protocol-types.rkt"
         "../util/ansi.rkt"
         "../util/markdown.rkt"
         "../util/error-classify.rkt"
         (only-in "../util/content-helpers.rkt" tool-result-content->string)
         json
         racket/string
         racket/format
         racket/list)

(provide format-event-for-terminal
         render-markdown
         render-tokens
         render-token
         make-stream-markdown-writer
         truncate-string
         tool-result-content->string
         MAX-TOOL-DISPLAY-LEN
         format-classified-error)

;; ============================================================
;; Convert tool-result content to display string
;; ============================================================

;; tool-result-content->string imported from util/content-helpers.rkt

;; ============================================================
;; Markdown → Terminal renderer
;; ============================================================

;; Renders basic Markdown to ANSI-styled terminal text.
;; Delegates to the shared token-based parser in q/util/markdown.rkt,
;; then converts each token to an ANSI string using constants from q/util/ansi.rkt.

(define (render-markdown text)
  (if (not (color-enabled?))
      text
      (render-tokens (parse-markdown text))))

(define (render-tokens tokens)
  (string-append* (map render-token tokens)))

(define (render-token tok)
  (case (md-token-type tok)
    [(text) (md-token-content tok)]
    [(bold) (string-append ANSI-BOLD (md-token-content tok) ANSI-RESET)]
    [(italic) (string-append ANSI-ITALIC (md-token-content tok) ANSI-RESET)]
    [(code) (string-append ANSI-CYAN (md-token-content tok) ANSI-RESET)]
    [(header)
     (define content (md-token-content tok))
     (string-append ANSI-BOLD ANSI-YELLOW (cdr content) ANSI-RESET)]
    [(code-block)
     (define content (md-token-content tok))
     (define code (cdr content))
     (string-append ANSI-GREEN
                    (string-join (for/list ([line (string-split code "\n" #:trim? #f)])
                                   (string-append "  " line))
                                 (string-append "\n" ANSI-GREEN))
                    ANSI-RESET)]
    [(link)
     (define content (md-token-content tok))
     (string-append ANSI-BLUE ANSI-UNDERLINE (cdr content) ANSI-RESET)]
    [(newline) "\n"]
    [else (format "~a" (md-token-content tok))]))

(define MAX-TOOL-DISPLAY-LEN 300)

;; ============================================================
;; Stream Markdown Writer — line-buffered Markdown rendering
;; ============================================================

;; Creates a stateful stream writer that buffers incoming text
;; fragments and renders complete lines through render-markdown.
;;
;; Strategy:
;;   1. Buffer incoming delta text
;;   2. When buffer contains complete lines (ending in \n),
;;      extract and render them through render-markdown
;;   3. Partial lines (no trailing \n) are printed raw for
;;      immediate feedback, tracked for later replacement
;;   4. When a partial line later completes, erase the raw
;;      output with \033[2K\r and re-render the full line
;;   5. On flush, render any remaining partial line
;;
;; Returns a procedure: (writer text [port])
;; The writer writes rendered output to the given port.
;; Also returns a second value: the flush procedure.

(define (make-stream-markdown-writer)
  (define line-buffer (box "")) ;; accumulated text for current line

  ;; The writer procedure — buffers deltas, renders complete lines
  (define (writer text [port (current-output-port)])
    (when (and (string? text) (> (string-length text) 0))
      ;; Append to line buffer
      (set-box! line-buffer (string-append (unbox line-buffer) text))
      ;; Process any complete lines
      (define buf (unbox line-buffer))
      (when (string-contains? buf "\n")
        (define lines (string-split buf "\n" #:trim? #f))
        (define complete-lines (drop-right lines 1))
        (define remaining (last lines))
        ;; Render and print each complete line
        (for ([line (in-list complete-lines)])
          (display (render-markdown line) port)
          (newline port))
        (set-box! line-buffer remaining)
        (flush-output port))))

  ;; Flush procedure — call at stream end
  (define (flush! [port (current-output-port)])
    (define remaining (unbox line-buffer))
    (when (> (string-length remaining) 0)
      ;; Render and print the final partial line
      (display (render-markdown remaining) port)
      (newline port)
      (set-box! line-buffer "")
      (flush-output port)))

  (values writer flush!))

(define (truncate-string s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len) "...")
      s))

;; ============================================================
;; format-event-for-terminal
;; ============================================================

;; Convert an event struct to a human-readable string for terminal output.
;; Different formatting for different event types.
;; Returns "" for events that don't need terminal display.

(define (format-event-for-terminal evt)
  (define ev (event-ev evt))
  (define payload (event-payload evt))
  (case ev
    [("model.stream.delta") (hash-ref payload 'delta "")]
    ;; Emit trailing newline after streaming — puts cursor on fresh line
    [("model.stream.completed") "\n"]
    ;; Suppress — content already displayed via model.stream.delta events.
    ;; Return empty string so the subscriber prints nothing.
    [("assistant.message.completed") ""]
    [("tool.call.started")
     (define name (hash-ref payload 'name "?"))
     (define args-raw (hash-ref payload 'arguments #f))
     (define args
       (cond
         [(hash? args-raw) args-raw]
         [(string? args-raw)
          (with-handlers ([exn:fail? (lambda (_) #f)])
            (string->jsexpr args-raw))]
         [else #f]))
     (define detail
       (cond
         [(and args (hash? args))
          (define cmd
            (or (hash-ref args 'command #f) (hash-ref args 'path #f) (hash-ref args 'pattern #f) #f))
          (if cmd
              (truncate-string (format "~a" cmd) 100)
              #f)]
         [else #f]))
     (if detail
         (styled (format "[tool: ~a: ~a]" name detail) '(bold yellow))
         (styled (format "[tool: ~a]" name) '(bold yellow)))]
    [("tool.call.completed")
     (define name (hash-ref payload 'name "?"))
     (define result (hash-ref payload 'result #f))
     (define content-str
       (if result
           (truncate-string (tool-result-content->string result) MAX-TOOL-DISPLAY-LEN)
           name))
     (styled (format "[tool result: ~a]" content-str) '(dim))]
    [("tool.call.failed")
     (styled (format "[tool failed: ~a \u2014 ~a]"
                     (hash-ref payload 'name "?")
                     (hash-ref payload 'error "unknown"))
             '(red))]
    [("turn.started") ""]
    [("turn.completed") ""]
    [("runtime.error") (styled (format "Error: ~a" (hash-ref payload 'error "unknown error")) '(red))]
    [("session.started")
     (styled (format "[session started: ~a]" (hash-ref payload 'sessionId "")) '(dim))]
    [("session.resumed")
     (styled (format "[session resumed: ~a]" (hash-ref payload 'sessionId "")) '(dim))]
    [("compaction.warning")
     (styled (format "[compaction warning: ~a tokens]" (hash-ref payload 'tokenCount "?")) '(yellow))]
    [("session.forked")
     (styled (format "[session forked: ~a]" (hash-ref payload 'newSessionId "")) '(dim))]
    [else ""]))

;; ============================================================
;; Error formatting helpers
;; ============================================================

;; Format a classified error with user-friendly message and suggestions.
;; When verbose? is true, also shows the stack trace.
;; Returns a string suitable for display.
(define (format-classified-error e [verbose? #f])
  (define msg (exn-message e))
  (define classified (classify-error e))
  (define parts
    (append (list (format "Error: ~a" msg))
            (if classified
                (cons (format "  ~a" (car classified))
                      (for/list ([s (cdr classified)])
                        (format "  → ~a" s)))
                '())
            (if verbose?
                (list (format "  Stack trace: ~a" (exn-continuation-marks e)))
                '())))
  (string-join parts "\n"))
