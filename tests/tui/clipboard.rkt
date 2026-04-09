#lang racket

;; q/tests/tui/clipboard.rkt — Unit tests for q/tui/clipboard module

(require rackunit
         net/base64
         "../../../q/tui/clipboard.rkt")

;; Helper: extract base64 payload from OSC 52 sequence
(define (extract-osc52-b64 str)
  (define m (regexp-match #rx"]52;c;([A-Za-z0-9+/=]+)" str))
  (and m (cadr m)))

;; ============================================================
;; OSC 52 encoding
;; ============================================================

(let ()
  ;; osc-52-copy emits a well-formed OSC 52 sequence
  (define output (with-output-to-bytes (lambda () (osc-52-copy "hello"))))
  (define str (bytes->string/latin-1 output))
  (check-true (string-contains? str "]52;c;")
              "osc-52: has OSC 52 prefix")
  (check-equal? (extract-osc52-b64 str) "aGVsbG8="
                "osc-52: base64 encoded")
  (check-true (string-suffix? str (string (integer->char 27) #\\))
              "osc-52: ends with ST"))

(let ()
  ;; osc-52-copy handles empty string
  (define output (with-output-to-bytes (lambda () (osc-52-copy ""))))
  (define str (bytes->string/latin-1 output))
  (check-true (string-contains? str "]52;c;")
              "osc-52: empty string still emits OSC 52"))

(let ()
  ;; osc-52-copy handles spaces and special chars
  (define output (with-output-to-bytes (lambda () (osc-52-copy "hello world!"))))
  (define str (bytes->string/latin-1 output))
  (check-equal? (extract-osc52-b64 str) "aGVsbG8gd29ybGQh"
                "osc-52: spaces encoded correctly"))

(let ()
  ;; osc-52-copy encodes and round-trips text correctly
  (define output (with-output-to-bytes (lambda () (osc-52-copy "hello unicode"))))
  (define str (bytes->string/latin-1 output))
  (define b64 (extract-osc52-b64 str))
  (check-not-false b64 "osc-52: base64 payload found")
  (define decoded (bytes->string/utf-8 (base64-decode (string->bytes/latin-1 b64))))
  (check-equal? decoded "hello unicode"
                "osc-52: text round-trips correctly"))

(let ()
  ;; osc-52-copy handles multi-byte UTF-8 (\u00e4\u00f6\u00fc\u00df \u2713)
  (define output (with-output-to-bytes (lambda () (osc-52-copy "\u00e4\u00f6\u00fc\u00df \u2713"))))
  (define str (bytes->string/latin-1 output))
  (define b64 (extract-osc52-b64 str))
  (check-not-false b64 "osc-52 UTF-8: base64 payload found")
  (define decoded (bytes->string/utf-8 (base64-decode (string->bytes/latin-1 b64))))
  (check-equal? decoded "\u00e4\u00f6\u00fc\u00df \u2713"
                "osc-52 UTF-8: round-trips correctly"))

(let ()
  ;; osc-52-copy flushes the output port (sequence appears immediately)
  (define output (open-output-bytes))
  (osc-52-copy "flush-test" output)
  (define result (get-output-bytes output))
  (check-true (> (bytes-length result) 0)
              "osc-52: output port has data after call (flushed)"))

(let ()
  ;; osc-52-copy writes to custom output port
  (define output (open-output-bytes))
  (osc-52-copy "port-test" output)
  (define str (bytes->string/latin-1 (get-output-bytes output)))
  (check-true (string-contains? str "]52;c;")
              "osc-52: writes to custom output port"))

;; ============================================================
;; copy-text! with mode 'off
;; ============================================================

(let ()
  ;; mode 'off returns 'disabled
  (parameterize ([current-clipboard-mode 'off])
    (define result (copy-text! "test"))
    (check-equal? result 'disabled "copy-text! mode=off returns 'disabled")))

(let ()
  ;; mode 'off does not emit anything to output
  (define output (open-output-bytes))
  (parameterize ([current-clipboard-mode 'off])
    (copy-text! "test" output))
  (check-equal? (get-output-bytes output) #""
                "copy-text! mode=off emits nothing"))

;; ============================================================
;; copy-text! with mode 'osc52
;; ============================================================

(let ()
  ;; mode 'osc52 emits OSC 52 sequence
  (define output (open-output-bytes))
  (parameterize ([current-clipboard-mode 'osc52])
    (define result (copy-text! "hello osc" output))
    (check-equal? result 'ok-osc52 "copy-text! mode=osc52 returns 'ok-osc52")
    (define str (bytes->string/latin-1 (get-output-bytes output)))
    (check-true (string-contains? str "]52;c;")
                "copy-text! mode=osc52 emits OSC 52 sequence")))

(let ()
  ;; mode 'osc52 with multi-byte UTF-8 text
  (define output (open-output-bytes))
  (parameterize ([current-clipboard-mode 'osc52])
    (define result (copy-text! "\u00e4\u00f6\u00fc\u00df \u2713" output))
    (check-equal? result 'ok-osc52 "copy-text! mode=osc52 UTF-8 returns 'ok-osc52")
    (define str (bytes->string/latin-1 (get-output-bytes output)))
    (define b64 (extract-osc52-b64 str))
    (check-not-false b64 "copy-text! osc52 UTF-8: base64 payload found")
    (define decoded (bytes->string/utf-8 (base64-decode (string->bytes/latin-1 b64))))
    (check-equal? decoded "\u00e4\u00f6\u00fc\u00df \u2713"
                  "copy-text! osc52 UTF-8: round-trips correctly")))

;; ============================================================
;; copy-text! with mode 'system
;; ============================================================

(let ()
  ;; mode 'system returns valid status
  (parameterize ([current-clipboard-mode 'system])
    (define result (copy-text! "test"))
    (check-not-false (member result '(ok-system unavailable failed))
                "copy-text! mode=system returns valid status symbol")))

;; ============================================================
;; copy-text! with mode 'auto
;; ============================================================

(let ()
  ;; mode 'auto always succeeds (at minimum via OSC 52)
  (define output (open-output-bytes))
  (parameterize ([current-clipboard-mode 'auto])
    (define result (copy-text! "auto test" output))
    (check-not-false (member result '(ok-osc52 ok-system))
                "copy-text! mode=auto returns ok-* status")
    (define str (bytes->string/latin-1 (get-output-bytes output)))
    (check-true (string-contains? str "]52;c;")
                "copy-text! mode=auto emits OSC 52")))

;; ============================================================
;; Backend detection
;; ============================================================

(let ()
  ;; detect-clipboard-tool returns #f or (list path symbol)
  (define tool (detect-clipboard-tool))
  (check-true (or (not tool)
                  (and (list? tool)
                       (= (length tool) 2)
                       (path? (car tool))
                       (symbol? (cadr tool))))
              "detect-clipboard-tool returns #f or (list path symbol)"))

(let ()
  ;; clipboard-backend-available? returns #t
  (check-true (clipboard-backend-available?)
              "clipboard-backend-available? returns #t (OSC 52 always available)"))

;; ============================================================
;; clipboard-copy-via-tool
;; ============================================================

(let ()
  ;; clipboard-copy-via-tool returns #f for invalid tool
  (define result (clipboard-copy-via-tool "/nonexistent/tool/path" 'pbcopy "test"))
  (check-false result "clipboard-copy-via-tool returns #f for missing tool"))

(let ()
  ;; clipboard-copy-via-tool is a procedure
  (check-true (procedure? clipboard-copy-via-tool)
              "clipboard-copy-via-tool is a procedure"))

;; ============================================================
;; copy-selection!
;; ============================================================

(let ()
  ;; copy-selection! returns 'disabled when no selection
  (define (no-sel?) #f)
  (define (get-text ctx) "should not be called")
  (parameterize ([current-clipboard-mode 'osc52])
    (define result (copy-selection! 'fake-ctx get-text no-sel?))
    (check-equal? result 'disabled
                  "copy-selection! returns 'disabled when no selection")))

(let ()
  ;; copy-selection! returns 'disabled when selection is empty string
  (define (has-sel?) #t)
  (define (get-text ctx) "")
  (parameterize ([current-clipboard-mode 'osc52])
    (define result (copy-selection! 'fake-ctx get-text has-sel?))
    (check-equal? result 'disabled
                  "copy-selection! returns 'disabled for empty selection")))

(let ()
  ;; copy-selection! copies when selection exists
  (define output (open-output-bytes))
  (define (has-sel?) #t)
  (define (get-text ctx) "selected text")
  (parameterize ([current-clipboard-mode 'osc52]
                  [current-output-port output])
    (define result (copy-selection! 'fake-ctx get-text has-sel?))
    (check-equal? result 'ok-osc52
                  "copy-selection! returns 'ok-osc52 for valid selection")))

(let ()
  ;; copy-selection! returns 'disabled when selection is #f
  (define (has-sel?) #t)
  (define (get-text ctx) #f)
  (parameterize ([current-clipboard-mode 'osc52])
    (define result (copy-selection! 'fake-ctx get-text has-sel?))
    (check-equal? result 'disabled
                  "copy-selection! returns 'disabled when text is #f")))

;; ============================================================
;; current-clipboard-mode parameter
;; ============================================================

(let ()
  ;; Default mode is 'auto
  (check-equal? (current-clipboard-mode) 'auto
                "default clipboard mode is 'auto"))

(let ()
  ;; Mode can be set and restored
  (parameterize ([current-clipboard-mode 'off])
    (check-equal? (current-clipboard-mode) 'off "mode set to 'off"))
  (check-equal? (current-clipboard-mode) 'auto "mode restored to 'auto"))

;; ============================================================
;; Environment-aware tool detection
;; ============================================================

(let ()
  ;; detect-clipboard-tool prefers X11 tools when DISPLAY is set
  (define tool (parameterize ([current-clipboard-mode 'auto])
                 (detect-clipboard-tool)))
  ;; In this environment, DISPLAY is set, so xclip/xsel should be preferred
  ;; over wl-copy (unless Wayland env is also set)
  (when (and (getenv "DISPLAY") (not (getenv "WAYLAND_DISPLAY")))
    (when tool
      (check-not-false (member (cadr tool) '(xclip xsel pbcopy))
                  (format "X11 env: tool should be X11 or macOS, got ~a" tool)))))

(let ()
  ;; detect-clipboard-tool returns #f or valid tool even without env vars
  ;; (fallback path still tries all tools)
  (define tool (detect-clipboard-tool))
  (check-true (or (not tool)
                  (and (list? tool) (= (length tool) 2)))
              "detect-clipboard-tool: always returns #f or (list path sym)"))

(let ()
  ;; When DISPLAY is set and xclip exists, it should be found
  (when (and (getenv "DISPLAY") (find-executable-path "xclip"))
    (define tool (detect-clipboard-tool))
    (check-not-false (and tool (member (cadr tool) '(xclip xsel pbcopy)))
                "DISPLAY set + xclip installed: found X11 tool")))

(let ()
  ;; copy-text! auto mode: with a working tool, returns ok-system
  ;; (only if xclip actually works, which requires X11 running)
  (parameterize ([current-clipboard-mode 'system])
    (define result (copy-text! "test-auto-copy"))
    ;; Result depends on whether X11 is actually accessible
    (check-not-false (member result '(ok-system unavailable failed))
                (format "copy-text! system mode returns valid status: ~a" result))))

;; ============================================================
;; Environment-aware tool detection (with mocked env vars)
;; ============================================================

(let ()
  ;; When DISPLAY is set and WAYLAND_DISPLAY is unset, X11 tools preferred
  ;; Note: We can only test this properly if WAYLAND_DISPLAY is actually unset.
  ;; In environments where it's set, this test is a no-op.
  (define wayland-env (getenv "WAYLAND_DISPLAY"))
  (define display-env (getenv "DISPLAY"))
  (when (and display-env (not wayland-env))
    ;; Real X11-only environment
    (define tool (detect-clipboard-tool))
    (when tool
      (check-not-false (member (cadr tool) '(xclip xsel pbcopy))
                       (format "X11-only env: prefer X11 tools, got ~a" tool)))))

(let ()
  ;; When WAYLAND_DISPLAY is set, wl-copy preferred over X11 tools
  ;; Note: Can only test if wl-copy is installed and WAYLAND_DISPLAY is set.
  (define wayland-env (getenv "WAYLAND_DISPLAY"))
  (when (and wayland-env (find-executable-path "wl-copy"))
    (define tool (detect-clipboard-tool))
    (when tool
      ;; Should prefer wl-copy over xclip in Wayland env
      (check-not-false (member (cadr tool) '(wl-copy pbcopy))
                       (format "Wayland env: prefer wl-copy, got ~a" tool)))))
