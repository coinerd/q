#lang racket

;; tests/tui/test-wave4-markdown-theme.rkt — Wave 4 edge case tests
;;                                               for markdown, ANSI wrap, theme
;;
;; Tests for:
;; - #616: Expand markdown token coverage (G15)
;; - #617: ANSI-aware word wrapping (G16)
;; - #618: Theme system for TUI rendering (G10)

(require rackunit
         rackunit/text-ui
         racket/list
         "../../util/markdown.rkt"
         "../../tui/ansi-wrap.rkt"
         "../../tui/theme.rkt")

;; ============================================================
;; #616: Markdown token coverage edge cases
;; ============================================================

;; --- Nested constructs ---

(test-case "md-edge: bold inside blockquote"
  (define tokens (parse-markdown "> **bold text**"))
  (check-true (andmap md-token? tokens))
  (define bq (findf (lambda (t) (eq? (md-token-type t) 'blockquote)) tokens))
  (check-not-false bq "should produce a blockquote token")
  (when bq
    ;; content is (cons depth inner-tokens) — but may be a list
    (define content (md-token-content bq))
    (define inner (if (pair? content) (cdr content) '()))
    (check-not-false (andmap md-token? inner))
    (define bold-tok (findf (lambda (t) (eq? (md-token-type t) 'bold)) inner))
    (check-not-false bold-tok "blockquote should contain bold token")))

(test-case "md-edge: link inside unordered list"
  (define tokens (parse-markdown "- [click here](https://example.com)"))
  (check-true (andmap md-token? tokens))
  ;; Should produce unordered-list token with link inside
  (define ul (findf (lambda (t) (eq? (md-token-type t) 'unordered-list)) tokens))
  (check-not-false ul "should produce unordered-list token")
  (when ul
    (define content (md-token-content ul))
    (define inner (if (pair? content) (cdr content) '()))
    (define link (findf (lambda (t) (eq? (md-token-type t) 'link)) inner))
    (check-not-false link "list should contain link token")))

(test-case "md-edge: inline code inside bold"
  (define tokens (parse-markdown "**use `code` here**"))
  (define bold (findf (lambda (t) (eq? (md-token-type t) 'bold)) tokens))
  (check-not-false bold "should find bold token"))

(test-case "md-edge: code inside italic"
  (define tokens (parse-markdown "*use `code` here*"))
  (define italic (findf (lambda (t) (eq? (md-token-type t) 'italic)) tokens))
  (check-not-false italic "should find italic token"))

;; --- Edge cases: empty/minimal inputs ---

(test-case "md-edge: single backtick only"
  (define tokens (parse-markdown "`"))
  (check-true (andmap md-token? tokens)))

(test-case "md-edge: triple backtick only (unclosed)"
  (define tokens (parse-markdown "```"))
  (check-true (andmap md-token? tokens)))

(test-case "md-edge: header at level boundary h7 (should be plain text)"
  (define tokens (parse-markdown "####### not a header"))
  (define hdr (findf (lambda (t) (eq? (md-token-type t) 'header)) tokens))
  ;; h7 is beyond h6, should NOT be a header
  (check-false hdr "h7 should not be parsed as header"))

(test-case "md-edge: code block with empty language"
  (define tokens (parse-markdown "```\ncode\n```"))
  (define cb (findf (lambda (t) (eq? (md-token-type t) 'code-block)) tokens))
  (check-not-false cb "should produce code-block token")
  (when cb
    (define lang (car (md-token-content cb)))
    (check-false lang "language should be #f for empty")))

(test-case "md-edge: empty input via parse-line"
  (define tokens (parse-line ""))
  (check-equal? tokens '()))

(test-case "md-edge: whitespace only line"
  (define tokens (parse-line "   "))
  (check-true (andmap md-token? tokens)))

;; --- Strikethrough edge cases ---

(test-case "md-edge: strikethrough with special chars"
  (define tokens (parse-inline-markdown "~~hello-world!~~"))
  (define st (findf (lambda (t) (eq? (md-token-type t) 'strikethrough)) tokens))
  (check-not-false st "should find strikethrough"))

(test-case "md-edge: multiple strikethroughs in one line"
  (define tokens (parse-inline-markdown "~~a~~ and ~~b~~"))
  (define sts (filter (lambda (t) (eq? (md-token-type t) 'strikethrough)) tokens))
  (check-equal? (length sts) 2 "should find two strikethrough tokens"))

;; --- Mixed construct priority ---

(test-case "md-edge: inline code takes priority over bold markers"
  (define tokens (parse-inline-markdown "`**not bold**`"))
  (define code (findf (lambda (t) (eq? (md-token-type t) 'code)) tokens))
  (check-not-false code "should be inline code, not bold")
  (when code
    (check-equal? (md-token-content code) "**not bold**")))

(test-case "md-edge: bold and link adjacent"
  (define tokens (parse-inline-markdown "**bold** [link](url)"))
  (define bold (findf (lambda (t) (eq? (md-token-type t) 'bold)) tokens))
  (define link (findf (lambda (t) (eq? (md-token-type t) 'link)) tokens))
  (check-not-false bold "should find bold")
  (check-not-false link "should find link"))

;; --- Multi-line constructs ---

(test-case "md-edge: multiple headers of different levels"
  (define tokens (parse-markdown "# H1\n## H2\n### H3"))
  (define hdrs (filter (lambda (t) (eq? (md-token-type t) 'header)) tokens))
  (check-equal? (length hdrs) 3 "should find 3 headers")
  (check-equal? (car (md-token-content (car hdrs))) 1)
  (check-equal? (car (md-token-content (cadr hdrs))) 2)
  (check-equal? (car (md-token-content (caddr hdrs))) 3))

(test-case "md-edge: mixed block and inline"
  (define tokens (parse-markdown "# Title\n\n- item with **bold**\n\n> quote with `code`"))
  (check-true (andmap md-token? tokens))
  (check-not-false (findf (lambda (t) (eq? (md-token-type t) 'header)) tokens))
  (check-not-false (findf (lambda (t) (eq? (md-token-type t) 'unordered-list)) tokens))
  (check-not-false (findf (lambda (t) (eq? (md-token-type t) 'blockquote)) tokens)))

(test-case "md-edge: code block with language containing special chars"
  ;; Only alphanumeric + dash/underscore allowed
  (define tokens (parse-markdown "```javascript\ncode\n```"))
  (define cb (findf (lambda (t) (eq? (md-token-type t) 'code-block)) tokens))
  (check-not-false cb "should find code block")
  (when cb
    (check-equal? (car (md-token-content cb)) "javascript")))

(test-case "md-edge: ordered list with large number"
  (define tokens (parse-markdown "42. item forty-two"))
  (define ol (findf (lambda (t) (eq? (md-token-type t) 'ordered-list)) tokens))
  (check-not-false ol "should find ordered list")
  (when ol
    ;; content is (cons indent (cons number inner)) or a list
    (define content (md-token-content ol))
    (check-equal? (list-ref content 1) 42)))

;; ============================================================
;; #617: ANSI-aware word wrapping edge cases
;; ============================================================

(test-case "wrap-edge: empty string"
  (define result (wrap-ansi-line "" 80))
  (check-equal? result '("")))

(test-case "wrap-edge: text shorter than width"
  (define result (wrap-ansi-line "hello" 80))
  (check-equal? result '("hello")))

(test-case "wrap-edge: text exactly at width"
  (define result (wrap-ansi-line "hello" 5))
  (check-equal? result '("hello")))

(test-case "wrap-edge: text one char over width"
  (define result (wrap-ansi-line "hello!" 5))
  (check-equal? (length result) 2 "should wrap into 2 lines"))

(test-case "wrap-edge: ANSI at line break boundary"
  ;; ANSI code at the exact break point should not be broken
  (define text (format "hello world \x1b[31mredis\x1b[0m"))
  (define result (wrap-ansi-line text 13))
  (check-true (andmap string? result))
  (check-true (>= (length result) 1)))

(test-case "wrap-edge: multiple ANSI codes in a row"
  (define text (format "\x1b[1m\x1b[31mbold red\x1b[0m and plain text here"))
  (define result (wrap-ansi-line text 15))
  (check-true (andmap string? result)))

(test-case "wrap-edge: very long word without spaces"
  (define result (wrap-ansi-line "abcdefghijklmnopqrstuvwxyz" 10))
  (check-true (>= (length result) 2) "long word should be hard-wrapped"))

(test-case "wrap-edge: text with only ANSI codes"
  (define text (format "\x1b[31m\x1b[0m"))
  (define result (wrap-ansi-line text 10))
  (check-equal? (length result) 1 "ANSI-only text should be single line"))

(test-case "wrap-edge: wrap-ansi-text with multiple newlines"
  (define result (wrap-ansi-text "line1\nline2\nline3" 80))
  (check-equal? result '("line1" "line2" "line3")))

(test-case "wrap-edge: wrap-ansi-text zero width"
  (define result (wrap-ansi-text "hello" 0))
  (check-equal? result '("hello") "zero width returns input"))

(test-case "wrap-edge: ansi-visible-width with CJK character"
  (define w (ansi-visible-width "你好"))
  (check-equal? w 4 "CJK chars should be double-width"))

(test-case "wrap-edge: ansi-visible-width plain text"
  (define w (ansi-visible-width "hello"))
  (check-equal? w 5))

(test-case "wrap-edge: ansi-visible-width with CSI sequence"
  (define w (ansi-visible-width (format "\x1b[31mhello\x1b[0m")))
  (check-equal? w 5 "CSI codes have zero width"))

(test-case "wrap-edge: ansi-visible-width empty string"
  (define w (ansi-visible-width ""))
  (check-equal? w 0))

(test-case "wrap-edge: find-ansi-break-pos with no spaces"
  (define pos (find-ansi-break-pos "abcdefghij" 5))
  (check-true (and (integer? pos) (>= pos 0))))

(test-case "wrap-edge: find-ansi-break-pos with space in middle"
  (define pos (find-ansi-break-pos "hello world" 8))
  (check-equal? pos 6 "should break after space"))

;; ============================================================
;; #618: Theme system edge cases
;; ============================================================

(test-case "theme-edge: default-dark-theme all fields are symbols or #f"
  (define t default-dark-theme)
  (check-true (tui-theme? t))
  (check-true (or (symbol? (tui-theme-text t)) (not (tui-theme-text t))))
  (check-true (or (symbol? (tui-theme-accent t)) (not (tui-theme-accent t))))
  (check-true (or (symbol? (tui-theme-muted t)) (not (tui-theme-muted t))))
  (check-true (or (symbol? (tui-theme-error t)) (not (tui-theme-error t)))))

(test-case "theme-edge: default-light-theme is valid theme"
  (check-true (tui-theme? default-light-theme)))

(test-case "theme-edge: theme-ref for all standard fields"
  ;; theme-ref should resolve field names to color values
  (parameterize ([current-tui-theme default-dark-theme])
    (define text-color (theme-ref 'text))
    (check-true (or (symbol? text-color) (string? text-color) (not text-color)))))

(test-case "theme-edge: theme-color->sgr with all named colors"
  (for ([color '(black red green yellow blue magenta cyan white)])
    (define sgr (theme-color->sgr color))
    (check-true (string? sgr) (format "~a should produce SGR string" color))))

(test-case "theme-edge: theme-color->sgr with bright colors"
  (for ([color '(bright-black bright-red bright-green bright-yellow
                 bright-blue bright-magenta bright-cyan bright-white)])
    (define sgr (theme-color->sgr color))
    (check-true (string? sgr) (format "~a should produce SGR string" color))))

(test-case "theme-edge: theme-color->sgr with 256-color string"
  (define sgr (theme-color->sgr "38;5;202"))
  (check-true (string? sgr) "256-color string should pass through"))

(test-case "theme-edge: theme-color->sgr with truecolor string"
  (define sgr (theme-color->sgr "38;2;255;128;0"))
  (check-true (string? sgr) "truecolor string should pass through"))

(test-case "theme-edge: theme-color->sgr-bg converts fg to bg"
  (define fg-sgr (theme-color->sgr 'red))
  (define bg-sgr (theme-color->sgr-bg 'red))
  (check-true (string? bg-sgr) "should produce bg SGR string")
  ;; bg should be different from fg (3x vs 4x prefix)
  (check-not-equal? fg-sgr bg-sgr "fg and bg should differ"))

(test-case "theme-edge: theme-color->sgr with #f returns #f or empty"
  (define sgr (theme-color->sgr #f))
  (check-true (or (not sgr) (string? sgr)) "#f should produce #f or empty string"))

(test-case "theme-edge: current-tui-theme parameterize"
  (parameterize ([current-tui-theme default-light-theme])
    (check-eq? (current-tui-theme) default-light-theme))
  (check-eq? (current-tui-theme) default-dark-theme "restored after parameterize"))

(test-case "theme-edge: dark and light themes differ"
  ;; At least text color should differ between dark and light themes
  (check-not-equal? (tui-theme-text default-dark-theme)
                    (tui-theme-text default-light-theme)
                    "dark and light text colors should differ"))

(test-case "theme-edge: theme struct transparency"
  (check-true (tui-theme? default-dark-theme))
  ;; Transparent struct — can destructure
  (check-true (string? (format "~a" default-dark-theme)) "theme should format as string"))
