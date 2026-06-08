#lang racket/base

;; @speed fast
;; @suite default

;; q/tests/test-gui-markdown-parser.rkt — Tests for markdown code block parsing
;;
;; Tests for gui/components/markdown-parser.rkt

(require rackunit
         rackunit/text-ui
         racket/string
         "../gui/components/markdown-parser.rkt"
         "../ui-core/theme-protocol.rkt")

;; ── contains-code-blocks? tests ──

(define-test-suite test-contains-code-blocks

  (test-case "detects triple backticks"
    (check-true (contains-code-blocks? "hello ```racket\n(+ 1 2)\n``` world"))

  (test-case "returns #f for plain text"
    (check-false (contains-code-blocks? "just some text without code")))

  (test-case "returns #f for empty string"
    (check-false (contains-code-blocks? "")))

  (test-case "returns #f for non-string"
    (check-false (contains-code-blocks? 42))
    (check-false (contains-code-blocks? #f)))

  (test-case "detects multiple code blocks"
    (check-true (contains-code-blocks? "```a``` text ```b```")))))

(run-tests test-contains-code-blocks)

;; ── parse-code-blocks tests ──

(define-test-suite test-parse-code-blocks

  (test-case "splits on triple backticks"
    (define result (parse-code-blocks "hello ```(+ 1 2)``` world"))
    (check >= (length result) 2)
    (check equal? (hash-ref (car result) 'type) 'text)
    (check equal? (hash-ref (cadr result) 'type) 'code-block))

  (test-case "extracts language from first line"
    (define result (parse-code-blocks "```racket\n(+ 1 2)\n```"))
    (define code-seg (findf (lambda (s) (equal? (hash-ref s 'type #f) 'code-block)) result))
    (check-not-false code-seg)
    (check equal? (hash-ref code-seg 'lang) "racket"))

  (test-case "handles empty input"
    (define result (parse-code-blocks ""))
    (check = (length result) 1)
    (check equal? (hash-ref (car result) 'type) 'text))

  (test-case "handles non-string input"
    (define result (parse-code-blocks 42))
    (check = (length result) 1)
    (check equal? (hash-ref (car result) 'type) 'text))

  (test-case "handles code-only input"
    (define result (parse-code-blocks "```(+ 1 2)```"))
    (check >= (length result) 1)
    (check equal? (hash-ref (car result) 'type) 'code-block))

  (test-case "handles text-only input"
    (define result (parse-code-blocks "just text"))
    (check = (length result) 1)
    (check equal? (hash-ref (car result) 'type) 'text))

  (test-case "handles consecutive code blocks"
    (define result (parse-code-blocks "```a``````b```"))
    (define code-segs (filter (lambda (s) (equal? (hash-ref s 'type #f) 'code-block)) result))
    (check = (length code-segs) 2)))

(run-tests test-parse-code-blocks)

;; ── code-block-style tests ──

(define-test-suite test-code-block-style

  (test-case "returns hash with expected keys"
    (define theme (make-ui-theme))
    (define style (code-block-style theme))
    (check-not-false (hash-ref style 'background #f))
    (check-not-false (hash-ref style 'foreground #f))
    (check-not-false (hash-ref style 'font #f))
    (check-not-false (hash-ref style 'border-left #f)))

  (test-case "defaults to dark theme colors"
    (define theme (make-ui-theme))
    (define style (code-block-style theme))
    (check equal? (hash-ref style 'foreground) "#cdd6f4")
    (check equal? (hash-ref style 'font) "monospace")))

(run-tests test-code-block-style)

;; ── code-block-header-style tests ──

(define-test-suite test-code-block-header-style

  (test-case "returns hash with text and style"
    (define h (code-block-header-style "racket"))
    (check equal? (hash-ref h 'text) "racket")
    (check-not-false (hash-ref h 'style #f)))

  (test-case "handles #f language"
    (define h (code-block-header-style #f))
    (check equal? (hash-ref h 'text) "")))

(run-tests test-code-block-header-style)

;; ── render-message-with-code-blocks tests ──

(define-test-suite test-render-message-code-blocks

  (test-case "produces segments for code message"
    (define msg (hash 'text "```racket\n(+ 1 2)\n```" 'role "assistant"))
    (define result (render-message-with-code-blocks msg (make-ui-theme)))
    (check-not-false (hash-ref result 'segments #f))
    (check >= (length (hash-ref result 'segments)) 2))

  (test-case "plain text passthrough"
    (define msg (hash 'text "just a message" 'role "user"))
    (define result (render-message-with-code-blocks msg (make-ui-theme)))
    (check-not-false (hash-ref result 'segments #f))
    (check >= (length (hash-ref result 'segments)) 1)))

(run-tests test-render-message-code-blocks)
