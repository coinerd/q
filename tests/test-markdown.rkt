#lang racket

;; q/tests/test-markdown.rkt — tests for q/util/markdown.rkt

(require rackunit
         rackunit/text-ui
         "../util/markdown.rkt")

(define markdown-suite
  (test-suite
   "markdown parsing tests"

   ;; ============================================================
   ;; md-token struct
   ;; ============================================================

   (test-case "md-token is transparent"
     (define tok (md-token 'text "hello"))
     (check-equal? (md-token-type tok) 'text)
     (check-equal? (md-token-content tok) "hello"))

   (test-case "md-token? predicate"
     (check-true (md-token? (md-token 'bold "x")))
     (check-false (md-token? "not a token")))

   ;; ============================================================
   ;; Empty / trivial inputs
   ;; ============================================================

   (test-case "empty string produces empty list"
     (check-equal? (parse-markdown "") '()))

   (test-case "plain text passthrough via parse-markdown"
     (define result (parse-markdown "hello world"))
     (check-equal? result (list (md-token 'text "hello world"))))

   (test-case "plain text via parse-inline-markdown"
     (define result (parse-inline-markdown "plain text"))
     (check-equal? result (list (md-token 'text "plain text"))))

   (test-case "empty string inline produces empty list"
     (check-equal? (parse-inline-markdown "") '()))

   ;; ============================================================
   ;; Inline code
   ;; ============================================================

   (test-case "inline code"
     (define result (parse-inline-markdown "use `foo` here"))
     (check-equal? result
                   (list (md-token 'text "use ")
                         (md-token 'code "foo")
                         (md-token 'text " here"))))

   (test-case "inline code only"
     (define result (parse-inline-markdown "`bar`"))
     (check-equal? result (list (md-token 'code "bar"))))

   (test-case "multiple inline code segments"
     (define result (parse-inline-markdown "`a` and `b`"))
     (check-equal? result
                   (list (md-token 'code "a")
                         (md-token 'text " and ")
                         (md-token 'code "b"))))

   ;; ============================================================
   ;; Bold
   ;; ============================================================

   (test-case "bold text"
     (define result (parse-inline-markdown "**hello**"))
     (check-equal? result (list (md-token 'bold "hello"))))

   (test-case "bold within text"
     (define result (parse-inline-markdown "say **world** now"))
     (check-equal? result
                   (list (md-token 'text "say ")
                         (md-token 'bold "world")
                         (md-token 'text " now"))))

   (test-case "multiple bold segments"
     (define result (parse-inline-markdown "**a** and **b**"))
     (check-equal? result
                   (list (md-token 'bold "a")
                         (md-token 'text " and ")
                         (md-token 'bold "b"))))

   (test-case "bold without closing → plain text"
     (define result (parse-inline-markdown "**unclosed"))
     (check-equal? result (list (md-token 'text "**unclosed"))))

   ;; ============================================================
   ;; Italic
   ;; ============================================================

   (test-case "italic text"
     (define result (parse-inline-markdown "*hello*"))
     (check-equal? result (list (md-token 'italic "hello"))))

   (test-case "italic within text"
     (define result (parse-inline-markdown "say *world* now"))
     (check-equal? result
                   (list (md-token 'text "say ")
                         (md-token 'italic "world")
                         (md-token 'text " now"))))

   (test-case "italic without closing → plain text"
     (define result (parse-inline-markdown "*unclosed"))
     (check-equal? result (list (md-token 'text "*unclosed"))))

   ;; ============================================================
   ;; Bold and italic interaction
   ;; ============================================================

   (test-case "bold and italic in same line"
     (define result (parse-inline-markdown "**bold** and *italic*"))
     (check-equal? result
                   (list (md-token 'bold "bold")
                         (md-token 'text " and ")
                         (md-token 'italic "italic"))))

   (test-case "bold does not consume italic asterisks"
     ;; **b* should not be treated as bold — bold needs ** on both sides
     (define result (parse-inline-markdown "**bold** *italic*"))
     (check-equal? (length result) 3)
     (check-equal? (md-token-type (car result)) 'bold)
     (check-equal? (md-token-type (caddr result)) 'italic))

   ;; ============================================================
   ;; Links
   ;; ============================================================

   (test-case "link"
     (define result (parse-inline-markdown "[click](http://example.com)"))
     (check-equal? result
                   (list (md-token 'link (cons "http://example.com" "click")))))

   (test-case "link within text"
     (define result (parse-inline-markdown "see [docs](url) here"))
     (check-equal? result
                   (list (md-token 'text "see ")
                         (md-token 'link (cons "url" "docs"))
                         (md-token 'text " here"))))

   (test-case "incomplete link — no closing paren → plain text"
     (define result (parse-inline-markdown "[text](unclosed"))
     ;; The [ and ( are found but no ) — should be plain text
     (check-equal? (length result) 1)
     (check-equal? (md-token-type (car result)) 'text))

   (test-case "incomplete link — no opening paren → plain text"
     (define result (parse-inline-markdown "[text]nope"))
     (check-equal? result (list (md-token 'text "[text]nope"))))

   ;; ============================================================
   ;; Headers
   ;; ============================================================

   (test-case "h1 header"
     (define result (parse-markdown "# Title"))
     (check-equal? result (list (md-token 'header (cons 1 "Title")))))

   (test-case "h3 header"
     (define result (parse-markdown "### Section"))
     (check-equal? result (list (md-token 'header (cons 3 "Section")))))

   (test-case "h6 header"
     (define result (parse-markdown "###### Deep"))
     (check-equal? result (list (md-token 'header (cons 6 "Deep")))))

   (test-case "header with trailing text"
     (define result (parse-markdown "## Hello World"))
     (check-equal? result (list (md-token 'header (cons 2 "Hello World")))))

   (test-case "not a header — no space after hashes"
     (define result (parse-markdown "###nope"))
     (check-equal? result (list (md-token 'text "###nope"))))

   ;; ============================================================
   ;; Code blocks
   ;; ============================================================

   (test-case "code block with language"
     (define result (parse-markdown "```racket\n(define x 1)\n```"))
     (check-equal? result
                   (list (md-token 'code-block (cons "racket" "(define x 1)\n"))
                         (md-token 'newline "\n"))))

   (test-case "code block without language"
     (define result (parse-markdown "```\nsome code\n```"))
     (check-equal? result
                   (list (md-token 'code-block (cons #f "some code\n"))
                         (md-token 'newline "\n"))))

   (test-case "multi-line code block"
     (define result (parse-markdown "```racket\n(line 1)\n(line 2)\n(line 3)\n```"))
     (check-equal? (length result) 2)
     (check-equal? (md-token-type (car result)) 'code-block)
     (check-equal? (md-token-type (cadr result)) 'newline)
     (check-equal? (car (md-token-content (car result))) "racket")
     (check-true (string-contains? (cdr (md-token-content (car result))) "(line 1)"))
     (check-true (string-contains? (cdr (md-token-content (car result))) "(line 3)")))

   (test-case "code block preceded and followed by text"
     (define result (parse-markdown "before\n```racket\ncode\n```\nafter"))
     ;; before + newline + code-block + newline + after = 5
     (check-equal? (length result) 5)
     (check-equal? (md-token-type (car result)) 'text)
     (check-equal? (md-token-type (cadr result)) 'newline)
     (check-equal? (md-token-type (caddr result)) 'code-block)
     (check-equal? (md-token-type (cadddr result)) 'newline)
     (check-equal? (md-token-type (car (cddddr result))) 'text))

   ;; ============================================================
   ;; Mixed / multi-line
   ;; ============================================================

   (test-case "multiple lines with mixed constructs"
     (define result (parse-markdown "# Title\nSome **bold** text\n```\ncode\n```\nEnd"))
     ;; Should have: header, newline, text+bold+text, newline, code-block, newline, text
     (check-true (>= (length result) 7))
     (check-equal? (md-token-type (car result)) 'header)
     ;; Find the bold token
     (check-not-false (member (md-token 'bold "bold") result))
     ;; Find the newline tokens
     (check-true (>= (length (filter (lambda (t) (eq? (md-token-type t) 'newline)) result)) 2)))

   (test-case "single newline produces two text tokens with newline"
     (define result (parse-markdown "line1\nline2"))
     (check-equal? result
                   (list (md-token 'text "line1")
                         (md-token 'newline "\n")
                         (md-token 'text "line2"))))

   ;; ============================================================
   ;; Edge cases
   ;; ============================================================

   (test-case "lone asterisk is plain text"
     (define result (parse-inline-markdown "3 * 5"))
     (check-equal? (length result) 1)
     (check-equal? (md-token-type (car result)) 'text))

   (test-case "double asterisks without content"
     (define result (parse-inline-markdown "****"))
     ;; Could be empty bold or two empty bolds — either way no crash
     (check-true (list? result)))

   (test-case "backtick without pair is plain text"
     (define result (parse-inline-markdown "a ` b"))
     (check-equal? result (list (md-token 'text "a ` b"))))

   (test-case "code block content is not inline-parsed"
     (define result (parse-markdown "```\n**not bold**\n```"))
     (check-equal? (length result) 2)
     (check-equal? (md-token-type (car result)) 'code-block)
     (check-equal? (md-token-type (cadr result)) 'newline)
     (check-true (string-contains? (cdr (md-token-content (car result))) "**not bold**")))

   (test-case "link and bold in same line"
     (define result (parse-inline-markdown "**bold** [link](url)"))
     (check-equal? result
                   (list (md-token 'bold "bold")
                         (md-token 'text " ")
                         (md-token 'link (cons "url" "link")))))

   ;; ============================================================
   ;; New token types (#405)
   ;; ============================================================

   (test-case "horizontal rule: ---"
     (define result (parse-line "---"))
     (check-equal? result (list (md-token 'hr #t))))

   (test-case "horizontal rule: ***"
     (define result (parse-line "***"))
     (check-equal? result (list (md-token 'hr #t))))

   (test-case "horizontal rule: ___"
     (define result (parse-line "___"))
     (check-equal? result (list (md-token 'hr #t))))

   (test-case "horizontal rule with spaces"
     (define result (parse-line "- - -"))
     (check-equal? result (list (md-token 'hr #t))))

   (test-case "unordered list: dash"
     (define result (parse-line "- item one"))
     (check-equal? (md-token-type (car result)) 'unordered-list)
     (check-equal? (car (md-token-content (car result))) 0))

   (test-case "unordered list: asterisk"
     (define result (parse-line "* item two"))
     (check-equal? (md-token-type (car result)) 'unordered-list))

   (test-case "unordered list: plus"
     (define result (parse-line "+ item three"))
     (check-equal? (md-token-type (car result)) 'unordered-list))

   (test-case "unordered list: indented"
     (define result (parse-line "  - nested item"))
     (check-equal? (md-token-type (car result)) 'unordered-list)
     (check-equal? (car (md-token-content (car result))) 1))

   (test-case "ordered list"
     (define result (parse-line "1. first item"))
     (check-equal? (md-token-type (car result)) 'ordered-list)
     (check-equal? (car (md-token-content (car result))) 0)
     (check-equal? (cadr (md-token-content (car result))) 1))

   (test-case "ordered list: indented"
     (define result (parse-line "  3. nested"))
     (check-equal? (md-token-type (car result)) 'ordered-list)
     (check-equal? (car (md-token-content (car result))) 1)
     (check-equal? (cadr (md-token-content (car result))) 3))

   (test-case "blockquote"
     (define result (parse-line "> quoted text"))
     (check-equal? (md-token-type (car result)) 'blockquote)
     (check-equal? (car (md-token-content (car result))) 1))

   (test-case "strikethrough"
     (define result (parse-inline-markdown "~~deleted~~"))
     (check-equal? result (list (md-token 'strikethrough "deleted"))))

   (test-case "strikethrough in text"
     (define result (parse-inline-markdown "keep ~~remove~~ keep"))
     (check-equal? (length result) 3)
     (check-equal? (md-token-type (cadr result)) 'strikethrough)
     (check-equal? (md-token-content (cadr result)) "remove"))
   ))

(run-tests markdown-suite 'verbose)
