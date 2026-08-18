#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;; q/tests/test-markdown-parser.rkt — BUG-0004 GFM table support tests
;;
;; Parse-level (token structure, alignment variants, malformed fallback)
;; and render-level (plain-line alignment, width fitting, in-column wrapping).

(require rackunit
         racket/string
         racket/list
         "../util/markdown.rkt"
         "../tui/markdown.rkt"
         "../tui/render/message-layout.rkt")

;; ---------- helpers ----------

(define (table-tokens toks)
  (filter (lambda (t) (eq? (md-token-type t) 'table)) toks))

(define (text-tokens toks)
  (filter (lambda (t) (eq? (md-token-type t) 'text)) toks))

(define sample-table
  (string-append "| Layer | File | Responsibility |\n"
                 "|-------|:----:|---------------:|\n"
                 "| parse | util/markdown.rkt | tokens |\n"
                 "| render | tui/markdown.rkt | styled-lines |\n"))

;; ---------- parse level ----------

(test-case "GFM table lexes to one structured 'table token"
  (define toks (parse-markdown sample-table))
  (define tables (table-tokens toks))
  (check-equal? (length tables) 1)
  (define content (md-token-content (car tables)))
  (check-pred list? content)
  (check-equal? (length content) 3) ; header, alignments, rows
  (check-equal? (car content) '("Layer" "File" "Responsibility"))
  (check-equal? (cadr content) '(left center right))
  (check-equal? (caddr content)
                '(("parse" "util/markdown.rkt" "tokens") ("render" "tui/markdown.rkt"
                                                                   "styled-lines")))
  ;; no literal separator row survives as text
  (for ([t (in-list (text-tokens toks))])
    (check-false (string-contains? (md-token-content t) "---")
                 (format "separator leaked into text token: ~s" (md-token-content t)))))

(test-case "alignment variants: default, left, center, right"
  (check-equal? (table-alignment-from-cell ":---") 'left)
  (check-equal? (table-alignment-from-cell ":---:") 'center)
  (check-equal? (table-alignment-from-cell "---:") 'right)
  (check-equal? (table-alignment-from-cell "----") 'left))

(test-case "try-parse-table: header + delimiter + body"
  (define r (try-parse-table (list "| a | b |" "|---|---|" "| 1 | 2 |") 0))
  (check-pred list? r)
  (check-equal? (car r) '("a" "b"))
  (check-equal? (cadr r) '(left left))
  (check-equal? (caddr r) '(("1" "2")))
  (check-equal? (cadddr r) 3)) ; next-line index

(test-case "try-parse-table: no delimiter row -> #f (fallback to paragraph)"
  (check-false (try-parse-table (list "| a | b |" "| 1 | 2 |") 0)))

(test-case "malformed table (no delimiter row) parses as plain text — fallback preserved"
  (define md "not a table\n| a | b |\n| 1 | 2 |\nstill not a table")
  (define toks (parse-markdown md))
  (check-equal? (table-tokens toks) '())
  ;; text content is intact
  (define joined (string-append* (map md-token-content (text-tokens toks))))
  (check-true (string-contains? joined "| a | b |"))
  (check-true (string-contains? joined "| 1 | 2 |")))

(test-case "non-table markdown unaffected: bold/italic/code/lists still tokenize"
  (define toks (parse-markdown "**bold** and `code`\n- item one\n- item two"))
  (check-equal? (table-tokens toks) '())
  (check-true (for/or ([t (in-list toks)])
                (eq? (md-token-type t) 'bold)))
  (check-true (for/or ([t (in-list toks)])
                (eq? (md-token-type t) 'unordered-list))))

;; ---------- render level (plain lines) ----------

(test-case "markdown-table->plain-lines: aligned padded columns, no pipes"
  (define lines
    (markdown-table->plain-lines (list '("a" "bb" "ccc") '(left center right) '(("1" "2" "3")))))
  (check-equal? (length lines) 3) ; header + delimiter + body
  (for ([l (in-list lines)])
    (check-false (string-contains? l "|") l))
  ;; header cells padded to column widths
  (check-equal? (first lines) "a  bb  ccc"))

;; ---------- render level (TUI, width-aware) ----------

(test-case "TUI render: table lines fit terminal width"
  (define tbl "| h1 | h2 |\n|---|---|\n| aaaaaaaaaaaaaaaaaaaaaaaa | b |")
  (define width 20)
  (define lines
    (for/list ([sl (in-list (markdown->styled-lines tbl width))])
      (styled-line-plain-text sl)))
  (for ([l (in-list lines)])
    (check-true (<= (string-length l) width) (format "line exceeds width: ~s" l))))

(test-case "TUI render: over-wide cell wraps within its column, first line keeps header"
  (define tbl "| name | desc |\n|---|---|\n| x | aaaa bbbb cccc dddd |")
  (define width 24)
  (define lines
    (for/list ([sl (in-list (markdown->styled-lines tbl width))])
      (styled-line-plain-text sl)))
  (for ([l (in-list lines)])
    (check-true (<= (string-length l) width) (format "line exceeds width: ~s" l)))
  (check-true (string-contains? (first lines) "name"))
  ;; wrapped cell content all present across continuation lines
  (define joined (string-join lines "\n"))
  (for ([part '("aaaa" "bbbb" "cccc" "dddd")])
    (check-true (string-contains? joined part) part)))

(test-case "message-layout table path: table-token->styled-lines fits width"
  (define tok
    (md-token 'table (list '("Key" "Value") '(left left) '(("alpha" "one") ("beta" "two")))))
  (define lines
    (for/list ([sl (in-list (table-token->styled-lines tok 16))])
      (styled-line-plain-text sl)))
  (for ([l (in-list lines)])
    (check-true (<= (string-length l) 16) (format "line exceeds width: ~s" l))))
