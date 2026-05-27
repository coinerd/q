#lang racket

;; q/tests/test-vdom-html-render.rkt — Tests for HTML render backend

(require rackunit
         rackunit/text-ui
         "../tui/vdom.rkt"
         "../tui/vdom-html-render.rkt"
         "../tui/render/message-layout.rkt")

(define-test-suite
 test-vdom-html-render
 ;; ──────────────────────────────
 ;; HTML escape
 ;; ──────────────────────────────
 (test-case "html-escape escapes special characters"
   (check-equal? (html-escape "<script>") "&lt;script&gt;")
   (check-equal? (html-escape "a & b") "a &amp; b")
   (check-equal? (html-escape "\"hello\"") "&quot;hello&quot;"))
 ;; ──────────────────────────────
 ;; Style → CSS
 ;; ──────────────────────────────
 (test-case "empty styles produce empty CSS"
   (check-equal? (styles->css '()) ""))
 (test-case "bold style produces font-weight:bold"
   (define css (styles->css '(bold)))
   (check-not-false (regexp-match #rx"font-weight:bold" css)))
 (test-case "color style produces CSS color"
   (define css (styles->css '(red)))
   (check-not-false (regexp-match #rx"color:#cd0000" css)))
 (test-case "multiple styles combine"
   (define css (styles->css '(bold cyan)))
   (check-not-false (regexp-match #rx"font-weight:bold" css))
   (check-not-false (regexp-match #rx"color:#00cdcd" css)))
 (test-case "dim style adds opacity"
   (define css (styles->css '(dim red)))
   (check-not-false (regexp-match #rx"opacity:0.6" css)))
 ;; ──────────────────────────────
 ;; ANSI color → CSS
 ;; ──────────────────────────────
 (test-case "ansi-color->css maps standard colors"
   (check-equal? (ansi-color->css 0) "#000000")
   (check-equal? (ansi-color->css 1) "#cd0000")
   (check-equal? (ansi-color->css 7) "#e5e5e5")
   (check-equal? (ansi-color->css 15) "#ffffff"))
 ;; ──────────────────────────────
 ;; vtext → HTML
 ;; ──────────────────────────────
 (test-case "plain vtext renders as plain text"
   (define result (vdom->html (vtext "hello" '())))
   (check-equal? result "hello"))
 (test-case "styled vtext renders with span"
   (define result (vdom->html (vtext "hello" '(bold))))
   (check-not-false (regexp-match #rx"<span style=" result))
   (check-not-false (regexp-match #rx">hello</span>" result)))
 (test-case "vtext with special chars is escaped"
   (define result (vdom->html (vtext "<b>" '())))
   (check-equal? result "&lt;b&gt;"))
 ;; ──────────────────────────────
 ;; vhbox → HTML
 ;; ──────────────────────────────
 (test-case "vhbox renders children inline"
   (define tree (vhbox (list (vtext "a" '()) (vtext "b" '()))))
   (define result (vdom->html tree))
   (check-not-false (regexp-match #rx"display:inline-flex" result))
   (check-not-false (regexp-match #rx"a" result))
   (check-not-false (regexp-match #rx"b" result)))
 ;; ──────────────────────────────
 ;; vvbox → HTML
 ;; ──────────────────────────────
 (test-case "vvbox renders children stacked"
   (define tree (vvbox (list (vtext "line1" '()) (vtext "line2" '()))))
   (define result (vdom->html tree))
   (check-not-false (regexp-match #rx"flex-direction:column" result))
   (check-not-false (regexp-match #rx"line1" result))
   (check-not-false (regexp-match #rx"line2" result)))
 ;; ──────────────────────────────
 ;; vfill → HTML
 ;; ──────────────────────────────
 (test-case "vfill renders as inline-block with width"
   (define tree (vfill 10 #\space '()))
   (define result (vdom->html tree))
   (check-not-false (regexp-match #rx"width:10em" result))
   (check-not-false (regexp-match #rx"display:inline-block" result)))
 ;; ──────────────────────────────
 ;; voverlay → HTML
 ;; ──────────────────────────────
 (test-case "voverlay renders with absolute positioning"
   (define anchor (vtext "base" '()))
   (define content (vtext "overlay" '()))
   (define tree (voverlay content anchor 5 2))
   (define result (vdom->html tree))
   (check-not-false (regexp-match #rx"position:relative" result))
   (check-not-false (regexp-match #rx"position:absolute" result))
   (check-not-false (regexp-match #rx"left:5ch" result))
   (check-not-false (regexp-match #rx"top:2ch" result)))
 ;; ──────────────────────────────
 ;; styled-line → HTML
 ;; ──────────────────────────────
 (test-case "plain styled-line renders in q-line div"
   (define line (styled-line (list (styled-segment "hello" '()))))
   (define result (styled-line->html line))
   (check-not-false (regexp-match #rx"<div class=\"q-line\">" result))
   (check-not-false (regexp-match #rx"hello" result)))
 (test-case "styled-line with colors renders spans"
   (define line (styled-line (list (styled-segment "error" '(bold red)) (styled-segment " ok" '()))))
   (define result (styled-line->html line))
   (check-not-false (regexp-match #rx"font-weight:bold" result))
   (check-not-false (regexp-match #rx"color:#cd0000" result))
   (check-not-false (regexp-match #rx"ok" result)))
 ;; ──────────────────────────────
 ;; styled-lines → HTML
 ;; ──────────────────────────────
 (test-case "styled-lines produces multiple divs"
   (define lines
     (list (styled-line (list (styled-segment "line1" '())))
           (styled-line (list (styled-segment "line2" '())))))
   (define result (styled-lines->html lines))
   (check-equal? (length (regexp-match* #rx"<div class=\"q-line\">" result)) 2))
 ;; ──────────────────────────────
 ;; Full frame → HTML
 ;; ──────────────────────────────
 (test-case "frame->html produces complete HTML document"
   (define header (list (styled-line (list (styled-segment "q v0.62.2" '(bold))))))
   (define transcript (list (styled-line (list (styled-segment "hello world" '())))))
   (define status (styled-line (list (styled-segment "model:gemini|idle" '(green)))))
   (define input-line (styled-line (list (styled-segment "> type here" '(cyan)))))
   (define result (frame->html header transcript status input-line))
   (check-not-false (regexp-match #rx"<!DOCTYPE html>" result))
   (check-not-false (regexp-match #rx"q-header" result))
   (check-not-false (regexp-match #rx"q-transcript" result))
   (check-not-false (regexp-match #rx"q-status" result))
   (check-not-false (regexp-match #rx"q-input" result))
   (check-not-false (regexp-match #rx"q v0.62.2" result))
   (check-not-false (regexp-match #rx"hello world" result))))

(run-tests test-vdom-html-render)
