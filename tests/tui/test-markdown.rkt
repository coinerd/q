#lang racket

;; tests/tui/test-markdown.rkt — FEAT-69: Markdown rendering in TUI transcript

(require rackunit
         rackunit/text-ui
         "../../tui/markdown.rkt"
         "../../tui/render.rkt"
         "../../util/markdown.rkt")

(define markdown-tests
  (test-suite "TUI Markdown Rendering"

    ;; ============================================================
    ;; Headers
    ;; ============================================================
    (test-case "### headers render bold with theme style"
      (define lines (markdown->styled-lines "## Hello World" 80))
      (check > (length lines) 0)
      (define segs (styled-line-segments (car lines)))
      (check > (length segs) 0)
      (check-equal? (styled-segment-text (car segs)) "Hello World")
      (check-not-false (member 'bold (styled-segment-style (car segs)))))

    (test-case "# h1 header renders"
      (define lines (markdown->styled-lines "# Title" 80))
      (check > (length lines) 0)
      (check-not-false (string-contains? (styled-line-plain-text (car lines)) "Title")))

    ;; ============================================================
    ;; Bold and italic
    ;; ============================================================
    (test-case "**bold** renders with bold style"
      (define lines (markdown->styled-lines "This is **important** text" 80))
      (check > (length lines) 0)
      (define all-segs (append-map styled-line-segments lines))
      (define bold-segs (filter (lambda (s) (member 'bold (styled-segment-style s))) all-segs))
      (check > (length bold-segs) 0)
      (check-equal? (styled-segment-text (car bold-segs)) "important"))

    (test-case "*italic* renders with italic style"
      (define lines (markdown->styled-lines "This is *emphasized* text" 80))
      (check > (length lines) 0)
      (define all-segs (append-map styled-line-segments lines))
      (define italic-segs (filter (lambda (s) (member 'italic (styled-segment-style s))) all-segs))
      (check > (length italic-segs) 0)
      (check-equal? (styled-segment-text (car italic-segs)) "emphasized"))

    ;; ============================================================
    ;; Inline code
    ;; ============================================================
    (test-case "`code` renders with code style"
      (define lines (markdown->styled-lines "Use `raco test` to run" 80))
      (check > (length lines) 0)
      (define all-segs (append-map styled-line-segments lines))
      (define code-segs
        (filter (lambda (s) (member 'bright-green (styled-segment-style s))) all-segs))
      (check > (length code-segs) 0)
      (check-equal? (styled-segment-text (car code-segs)) "raco test"))

    ;; ============================================================
    ;; Code blocks
    ;; ============================================================
    (test-case "code blocks render inset"
      (define lines (markdown->styled-lines "```racket\n(define x 42)\n```" 80))
      (check > (length lines) 0)
      ;; Each code line should have indentation prefix
      (define line-text (styled-line-plain-text (car lines)))
      (check-not-false (string-contains? line-text "(define x 42)")))

    ;; ============================================================
    ;; Lists
    ;; ============================================================
    (test-case "- unordered list renders with bullet"
      (define lines (markdown->styled-lines "- First item\n- Second item" 80))
      (check >= (length lines) 2)
      (check-not-false (string-contains? (styled-line-plain-text (car lines)) "First item"))
      (check-not-false (string-contains? (styled-line-plain-text (cadr lines)) "Second item")))

    (test-case "ordered list renders with number"
      (define lines (markdown->styled-lines "1. First\n2. Second" 80))
      (check >= (length lines) 2)
      (check-not-false (string-contains? (styled-line-plain-text (car lines)) "1. First")))

    ;; ============================================================
    ;; Links
    ;; ============================================================
    (test-case "[link](url) renders with underline"
      (define lines (markdown->styled-lines "Click [here](https://racket-lang.org) for info" 80))
      (check > (length lines) 0)
      (define all-segs (append-map styled-line-segments lines))
      (define link-segs (filter (lambda (s) (member 'underline (styled-segment-style s))) all-segs))
      (check > (length link-segs) 0)
      (check-equal? (styled-segment-text (car link-segs)) "here"))

    ;; ============================================================
    ;; Horizontal rules
    ;; ============================================================
    (test-case "--- renders as horizontal rule"
      (define lines (markdown->styled-lines "above\n---\nbelow" 80))
      (check >= (length lines) 3)
      (check-not-false (string-contains? (styled-line-plain-text (car lines)) "above"))
      (check-not-false (string-contains? (styled-line-plain-text (last lines)) "below")))

    ;; ============================================================
    ;; Token-based API
    ;; ============================================================
    (test-case "markdown-tokens->styled-lines works with pre-parsed tokens"
      (define tokens (parse-markdown "**bold** and `code`"))
      (define lines (markdown-tokens->styled-lines tokens 80))
      (check > (length lines) 0)
      (define all-segs (append-map styled-line-segments lines))
      (define bold-segs (filter (lambda (s) (member 'bold (styled-segment-style s))) all-segs))
      (check > (length bold-segs) 0))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================
    (test-case "empty input returns empty lines"
      (define lines (markdown->styled-lines "" 80))
      (check-equal? (length lines) 0))

    (test-case "plain text renders as-is"
      (define lines (markdown->styled-lines "Hello world" 80))
      (check > (length lines) 0)
      (check-equal? (styled-line-plain-text (car lines)) "Hello world"))

    (test-case "mixed bold, italic, code in one line"
      (define lines (markdown->styled-lines "**bold** *italic* `code`" 80))
      (check > (length lines) 0)
      (define all-segs (append-map styled-line-segments lines))
      (check >= (length all-segs) 3))))

(run-tests markdown-tests)
