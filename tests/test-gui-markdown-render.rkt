#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-markdown-render.rkt — W5: markdown rendering polish
;;
;; Tests that parse-markdown-elements, contains-markdown?, and
;; render-message-descriptor produce correct typed segments.

(require rackunit
         rackunit/text-ui
         "../gui/components/markdown-parser.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../ui-core/theme-protocol.rkt")

(define theme (default-theme))

(run-tests
 (test-suite
  "gui-markdown-render"

  ;; ─── contains-markdown? ───

  (test-case "detects headers as markdown"
    (check-true (contains-markdown? "# Hello")))

  (test-case "detects list items as markdown"
    (check-true (contains-markdown? "- item")))

  (test-case "detects inline code as markdown"
    (check-true (contains-markdown? "use `foo` here")))

  (test-case "plain text is not markdown"
    (check-false (contains-markdown? "Just plain text here.")))

  ;; ─── parse-markdown-elements ───

  (test-case "parses headers with level"
    (define result (parse-markdown-elements "# Title\n## Subtitle"))
    (define headers (filter (lambda (s) (eq? (hash-ref s 'type #f) 'header)) result))
    (check-equal? (length headers) 2)
    (check-equal? (hash-ref (car headers) 'level) 1)
    (check-equal? (hash-ref (cadr headers) 'level) 2))

  (test-case "parses list items"
    (define result (parse-markdown-elements "- first\n- second"))
    (define items (filter (lambda (s) (eq? (hash-ref s 'type #f) 'list-item)) result))
    (check-equal? (length items) 2)
    (check-equal? (hash-ref (car items) 'text) "first")
    (check-equal? (hash-ref (cadr items) 'text) "second"))

  (test-case "parses numbered list items"
    (define result (parse-markdown-elements "1. first\n2. second"))
    (define items (filter (lambda (s) (eq? (hash-ref s 'type #f) 'list-item)) result))
    (check-equal? (length items) 2))

  (test-case "plain text returns text segments"
    (define result (parse-markdown-elements "Just a line"))
    (check-equal? (length result) 1)
    (check-equal? (hash-ref (car result) 'type) 'text))

  (test-case "code blocks delegated to parse-code-blocks"
    (define result (parse-markdown-elements "before```code```after"))
    (check-true (ormap (lambda (s) (eq? (hash-ref s 'type #f) 'code-block)) result)))

  ;; ─── render-message-descriptor with markdown ───

  (test-case "assistant message with header gets header segment"
    (define msg (hash 'role "assistant" 'text "# Hello World" 'kind 'message 'meta (hasheq)))
    (define plan (render-message-descriptor msg theme))
    (define segs (hash-ref plan 'segments))
    (define hdr-segs (filter (lambda (s) (eq? (hash-ref s 'type #f) 'header)) segs))
    (check-equal? (length hdr-segs) 1)
    (check-equal? (hash-ref (car hdr-segs) 'text) "Hello World"))

  (test-case "assistant message with list gets list-item segments"
    (define msg (hash 'role "assistant" 'text "- item one\n- item two" 'kind 'message 'meta (hasheq)))
    (define plan (render-message-descriptor msg theme))
    (define segs (hash-ref plan 'segments))
    (define list-segs (filter (lambda (s) (eq? (hash-ref s 'type #f) 'list-item)) segs))
    (check-equal? (length list-segs) 2))))
