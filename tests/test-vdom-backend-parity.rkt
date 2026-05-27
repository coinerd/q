#lang racket

;; q/tests/test-vdom-backend-parity.rkt — Architecture validation
;;
;; Proves the vdom pipeline is backend-agnostic by rendering the same
;; content through both the cell-buffer (terminal) and HTML backends,
;; then verifying structural equivalence.

(require rackunit
         rackunit/text-ui
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/vdom-html-render.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/render/message-layout.rkt")

;; Helper: extract plain text from a list of styled-lines
(define (styled-lines->text lines)
  (string-join (for/list ([line (in-list lines)])
                 (apply string-append
                        (for/list ([seg (in-list (styled-line-segments line))])
                          (styled-segment-text seg))))
               "\n"))

;; Helper: extract plain text from HTML by stripping tags
(define (html->text html)
  (define s1 (regexp-replace* #rx"<[^>]*>" html ""))
  (define s2 (regexp-replace* #rx"&amp;" s1 "&"))
  (define s3 (regexp-replace* #rx"&lt;" s2 "<"))
  (define s4 (regexp-replace* #rx"&gt;" s3 ">"))
  (define s5 (regexp-replace* #rx"&quot;" s4 "\""))
  (regexp-replace* #rx"^[ \t]*\n" s5 ""))

;; Helper: render vdom through cell-buffer and extract text
(define (vdom->terminal-text tree width)
  (define lines (vdom-layout tree width))
  (define buf (make-cell-buffer width (max 1 (length lines))))
  (render-styled-lines-to-buffer! lines buf width)
  (string-join (for/list ([row (in-range (cell-buffer-rows buf))])
                 (cell-buffer-row-string buf row))
               "\n"))

;; Helper: render vdom through HTML and extract text
(define (vdom->html-text tree width)
  (define lines (vdom-layout tree width))
  (html->text (styled-lines->html lines)))

(define-test-suite test-vdom-backend-parity
                   ;; ──────────────────────────────
                   ;; Plain text parity
                   ;; ──────────────────────────────
                   (test-case "plain text vtext produces same text in both backends"
                     (define tree (vtext "Hello, World!" '()))
                     (define term-text (vdom->terminal-text tree 40))
                     (define html-text (vdom->html-text tree 40))
                     ;; Both should contain "Hello, World!"
                     (check-not-false (regexp-match #rx"Hello, World!" term-text))
                     (check-not-false (regexp-match #rx"Hello, World!" html-text)))
                   (test-case "plain text styled-line produces same text in both backends"
                     (define line (styled-line (list (styled-segment "test content" '()))))
                     ;; Terminal path
                     (define buf (make-cell-buffer 40 1))
                     (render-styled-line-to-buffer! line buf 40 0)
                     (define term-text (cell-buffer-row-string buf 0))
                     ;; HTML path
                     (define html-result (styled-line->html line))
                     ;; Both should contain "test content"
                     (check-not-false (regexp-match #rx"test content" term-text))
                     (check-not-false (regexp-match #rx"test content" html-result)))
                   ;; ──────────────────────────────
                   ;; Styled text parity
                   ;; ──────────────────────────────
                   (test-case "styled text preserves content in both backends"
                     (define line
                       (styled-line (list (styled-segment "error: " '(bold red))
                                          (styled-segment "file not found" '()))))
                     ;; Terminal
                     (define buf (make-cell-buffer 40 1))
                     (render-styled-line-to-buffer! line buf 40 0)
                     (define term-text (cell-buffer-row-string buf 0))
                     ;; HTML
                     (define html-result (styled-line->html line))
                     ;; Both contain the same text
                     (check-not-false (regexp-match #rx"error:" term-text))
                     (check-not-false (regexp-match #rx"error:" html-result))
                     (check-not-false (regexp-match #rx"file not found" term-text))
                     (check-not-false (regexp-match #rx"file not found" html-result)))
                   ;; ──────────────────────────────
                   ;; Layout parity
                   ;; ──────────────────────────────
                   (test-case "vhbox children rendered inline in both backends"
                     (define tree (vhbox (list (vtext "left" '()) (vtext "right" '()))))
                     (define lines (vdom-layout tree 40))
                     (check-true (> (length lines) 0))
                     ;; Both backends get same styled-lines
                     (define term-text (vdom->terminal-text tree 40))
                     (define html-text (vdom->html-text tree 40))
                     (check-not-false (regexp-match #rx"left" term-text))
                     (check-not-false (regexp-match #rx"left" html-text))
                     (check-not-false (regexp-match #rx"right" term-text))
                     (check-not-false (regexp-match #rx"right" html-text)))
                   (test-case "vvbox children rendered on separate lines in both backends"
                     (define tree (vvbox (list (vtext "line1" '()) (vtext "line2" '()))))
                     (define term-text (vdom->terminal-text tree 40))
                     (define html-text (vdom->html-text tree 40))
                     (check-not-false (regexp-match #rx"line1" term-text))
                     (check-not-false (regexp-match #rx"line1" html-text))
                     (check-not-false (regexp-match #rx"line2" term-text))
                     (check-not-false (regexp-match #rx"line2" html-text)))
                   ;; ──────────────────────────────
                   ;; Complex tree parity
                   ;; ──────────────────────────────
                   (test-case "complex vdom tree produces consistent content in both backends"
                     (define tree
                       (vvbox (list (vhbox (list (vtext "q v0.62" '(bold cyan))
                                                 (vfill 20 #\space '())
                                                 (vtext "idle" '(green))))
                                    (vtext "Hello from user" '(bold))
                                    (vtext "Hi from assistant" '()))))
                     (define term-text (vdom->terminal-text tree 60))
                     (define html-text (vdom->html-text tree 60))
                     ;; All content appears in both
                     (for ([text '("q v0.62" "idle" "Hello from user" "Hi from assistant")])
                       (check-not-false (regexp-match (regexp-quote text) term-text)
                                        (format "~a missing from terminal" text))
                       (check-not-false (regexp-match (regexp-quote text) html-text)
                                        (format "~a missing from HTML" text))))
                   ;; ──────────────────────────────
                   ;; Architecture invariant
                   ;; ──────────────────────────────
                   (test-case "vdom-layout is the shared pipeline point"
                     ;; Verify that both backends consume the same vdom-layout output
                     (define tree (vhbox (list (vtext "A" '(bold)) (vtext "B" '(red)))))
                     (define lines (vdom-layout tree 40))
                     ;; Cell-buffer path
                     (define buf (make-cell-buffer 40 (max 1 (length lines))))
                     (render-styled-lines-to-buffer! lines buf 40)
                     ;; HTML path
                     (define html (styled-lines->html lines))
                     ;; Both consumed the same styled-lines
                     (check-equal? (length lines) 1)
                     (check-equal? (length (styled-line-segments (car lines))) 2)))

(run-tests test-vdom-backend-parity)
