#lang racket

;; @speed fast
;; @suite default

;; tests/test-cli-markdown.rkt — v0.70.8 W0
;; Extracted from test-cli.rkt: markdown rendering + stream writer tests

(require rackunit
         rackunit/text-ui
         "../util/markdown.rkt"
         "../interfaces/cli.rkt")

(define-test-suite test-cli-markdown
  (test-suite "Markdown rendering"

    (test-case "render-markdown returns text unchanged when color disabled"
      (check-equal? (render-markdown "hello **world**") "hello **world**"))

    (test-case "parse-markdown: bold token produced for **text**"
      (define tokens (parse-markdown "hello **world** end"))
      (define bold-toks (filter (lambda (t) (eq? (md-token-type t) 'bold)) tokens))
      (check-equal? (length bold-toks) 1)
      (check-equal? (md-token-content (car bold-toks)) "world"))

    (test-case "parse-markdown: italic token produced for *text*"
      (define tokens (parse-markdown "hello *world* end"))
      (define italic-toks (filter (lambda (t) (eq? (md-token-type t) 'italic)) tokens))
      (check-equal? (length italic-toks) 1)
      (check-equal? (md-token-content (car italic-toks)) "world"))

    (test-case "parse-markdown: inline code token for `code`"
      (define tokens (parse-markdown "use `code` here"))
      (define code-toks (filter (lambda (t) (eq? (md-token-type t) 'code)) tokens))
      (check-equal? (length code-toks) 1)
      (check-equal? (md-token-content (car code-toks)) "code"))

    (test-case "parse-markdown: link token for [text](url)"
      (define tokens (parse-markdown "click [here](https://example.com) now"))
      (define link-toks (filter (lambda (t) (eq? (md-token-type t) 'link)) tokens))
      (check-equal? (length link-toks) 1)
      (check-equal? (cdr (md-token-content (car link-toks))) "here"))

    (test-case "parse-markdown: header token for ### Header"
      (define tokens (parse-markdown "### My Header"))
      (define header-toks (filter (lambda (t) (eq? (md-token-type t) 'header)) tokens))
      (check-equal? (length header-toks) 1)
      (check-equal? (cdr (md-token-content (car header-toks))) "My Header")
      (check-equal? (car (md-token-content (car header-toks))) 3)))

  (test-suite "Stream Markdown Writer"

    (test-case "stream-writer: single complete line"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "hello world\n" out)
      (flush! out)
      (check-equal? (get-output-string out) "hello world\n"))

    (test-case "stream-writer: multiple fragments forming one line"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "hel" out)
      (writer "lo " out)
      (writer "world\n" out)
      (flush! out)
      (check-equal? (get-output-string out) "hello world\n"))

    (test-case "stream-writer: flush renders final partial line"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "hello" out)
      (writer " world" out)
      (flush! out)
      (check-equal? (get-output-string out) "hello world\n"))

    (test-case "stream-writer: two complete lines"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "line 1\nline 2\n" out)
      (flush! out)
      (check-equal? (get-output-string out) "line 1\nline 2\n"))

    (test-case "stream-writer: empty string is no-op"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "" out)
      (flush! out)
      (check-equal? (string-length (get-output-string out)) 0))

    (test-case "stream-writer: multiple lines in one delta"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "line1\nline2\nline3\n" out)
      (flush! out)
      (check-equal? (get-output-string out) "line1\nline2\nline3\n"))

    (test-case "stream-writer: complete lines then partial then complete"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "first\n" out)
      (writer "partial" out)
      (writer " line\n" out)
      (flush! out)
      (check-equal? (get-output-string out) "first\npartial line\n"))

    (test-case "stream-writer: double newline produces blank line"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (writer "\n\n" out)
      (flush! out)
      (check-equal? (get-output-string out) "\n\n"))

    (test-case "stream-writer: only flush, no prior writes"
      (define out (open-output-string))
      (define-values (writer flush!) (make-stream-markdown-writer))
      (flush! out)
      (check-equal? (string-length (get-output-string out)) 0))))

(run-tests test-cli-markdown)
