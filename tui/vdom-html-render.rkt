#lang racket/base

;; q/tui/vdom-html-render.rkt — HTML render backend for VDOM
;;
;; Converts vdom trees and styled-lines to HTML strings.
;; This is the second render backend (after cell-buffer/terminal),
;; validating the backend-agnostic architecture claim.
;;
;; Pipeline: vnode → vdom-layout → styled-lines → vdom-html-render → HTML string
;; Also:     vnode → vdom->html (direct conversion)

(require racket/contract
         racket/string
         racket/list
         "vdom.rkt"
         "render/message-layout.rkt")

;; ============================================================
;; Color/style mappings
;; ============================================================

;; ANSI color index → CSS hex color
(define (ansi-color->css n)
  (case n
    [(0) "#000000"] ; black
    [(1) "#cd0000"] ; red
    [(2) "#00cd00"] ; green
    [(3) "#cdcd00"] ; yellow
    [(4) "#0000ee"] ; blue
    [(5) "#cd00cd"] ; magenta
    [(6) "#00cdcd"] ; cyan
    [(7) "#e5e5e5"] ; white
    [(8) "#7f7f7f"] ; bright-black
    [(9) "#ff0000"] ; bright-red
    [(10) "#00ff00"] ; bright-green
    [(11) "#ffff00"] ; bright-yellow
    [(12) "#5c5cff"] ; bright-blue
    [(13) "#ff00ff"] ; bright-magenta
    [(14) "#00ffff"] ; bright-cyan
    [(15) "#ffffff"] ; bright-white
    [else "#e5e5e5"]))

;; Style symbols → CSS property list
(define (styles->css styles)
  (define fg-color #f)
  (define bg-color #f)
  (define bold? #f)
  (define underline? #f)
  (define italic? #f)
  (define dim? #f)
  (for ([s (in-list styles)])
    (case s
      [(bold) (set! bold? #t)]
      [(italic) (set! italic? #t)]
      [(underline) (set! underline? #t)]
      [(dim) (set! dim? #t)]
      [(red) (set! fg-color 1)]
      [(green) (set! fg-color 2)]
      [(yellow) (set! fg-color 3)]
      [(blue) (set! fg-color 4)]
      [(magenta) (set! fg-color 5)]
      [(cyan) (set! fg-color 6)]
      [(white) (set! fg-color 7)]
      [(bright-black) (set! fg-color 8)]
      [(bright-red) (set! fg-color 9)]
      [(bright-green) (set! fg-color 10)]
      [(bright-yellow) (set! fg-color 11)]
      [(bright-blue) (set! fg-color 12)]
      [(bright-magenta) (set! fg-color 13)]
      [(bright-cyan) (set! fg-color 14)]
      [(bright-white) (set! fg-color 15)]
      [else (void)]))
  (define parts '())
  (when (and fg-color (not dim?))
    (set! parts (cons (format "color:~a" (ansi-color->css fg-color)) parts)))
  (when (and fg-color dim?)
    (set! parts (cons (format "color:~a;opacity:0.6" (ansi-color->css fg-color)) parts)))
  (when bold?
    (set! parts (cons "font-weight:bold" parts)))
  (when italic?
    (set! parts (cons "font-style:italic" parts)))
  (when underline?
    (set! parts (cons "text-decoration:underline" parts)))
  (string-join (reverse parts) ";"))

;; ============================================================
;; HTML escaping
;; ============================================================

(define (html-escape s)
  (define s1 (string-replace s "&" "&amp;"))
  (define s2 (string-replace s1 "<" "&lt;"))
  (define s3 (string-replace s2 ">" "&gt;"))
  (define s4 (string-replace s3 "\"" "&quot;"))
  s4)

;; ============================================================
;; Vnode → HTML
;; ============================================================

;; Convert a vdom tree to an HTML fragment string.
;; Width is used for vfill sizing.
(define (vdom->html node [width 80])
  (cond
    [(vtext? node)
     (define css (styles->css (vtext-style node)))
     (define text (html-escape (vtext-text node)))
     (if (string=? css "")
         text
         (format "<span style=\"~a\">~a</span>" css text))]
    [(vhbox? node)
     (define children (string-join (map (lambda (c) (vdom->html c width)) (vhbox-children node)) ""))
     (format "<span style=\"display:inline-flex\">~a</span>" children)]
    [(vvbox? node)
     (define children (string-join (map (lambda (c) (vdom->html c width)) (vvbox-children node)) ""))
     (format "<div style=\"display:flex;flex-direction:column\">~a</div>" children)]
    [(vfill? node)
     (define n (vfill-width node))
     (define ch
       (if (vfill-char node)
           (vfill-char node)
           #\space))
     (define text (html-escape (make-string n ch)))
     (define css (styles->css (vfill-style node)))
     (if (string=? css "")
         (format "<span style=\"display:inline-block;width:~aem\">~a</span>" n text)
         (format "<span style=\"display:inline-block;width:~aem;~a\">~a</span>" n css text))]
    [(voverlay? node)
     ;; Overlay: content positioned on top of anchor
     (define anchor-html (vdom->html (voverlay-anchor node) width))
     (define content-html (vdom->html (voverlay-content node) width))
     (format
      "<div style=\"position:relative\">~a<div style=\"position:absolute;left:~ach;top:~ach\">~a</div></div>"
      anchor-html
      (voverlay-col node)
      (voverlay-row node)
      content-html)]
    [else ""]))

;; ============================================================
;; Styled-line → HTML
;; ============================================================

;; Convert a styled-line to HTML (one row).
(define (styled-line->html line)
  (define segs
    (for/list ([seg (in-list (styled-line-segments line))])
      (define text (html-escape (styled-segment-text seg)))
      (define css (styles->css (styled-segment-style seg)))
      (if (string=? css "")
          text
          (format "<span style=\"~a\">~a</span>" css text))))
  (format "<div class=\"q-line\">~a</div>" (string-join segs "")))

;; Convert a list of styled-lines to HTML fragment.
(define (styled-lines->html lines)
  (string-join (map styled-line->html lines) "\n"))

;; ============================================================
;; Full frame HTML
;; ============================================================

;; Render a complete frame: header lines, transcript lines, status line, input line.
;; Returns a complete HTML document string.
(define (frame->html header-lines transcript-lines status-line input-line #:width [width 80])
  (define header-html (styled-lines->html header-lines))
  (define transcript-html (styled-lines->html transcript-lines))
  (define status-html (styled-line->html status-line))
  (define input-html (styled-line->html input-line))
  (format
   "<!DOCTYPE html>
<html><head><meta charset=\"utf-8\">
<style>
.q-frame { font-family:monospace; font-size:14px; line-height:1.4; background:#1a1a2e; color:#e5e5e5; padding:0; margin:0; width:~aem; }
.q-header { border-bottom:1px solid #333; padding:2px 4px; }
.q-transcript { flex-grow:1; overflow-y:auto; padding:2px 4px; }
.q-status { border-top:1px solid #333; padding:2px 4px; }
.q-input { border-top:1px solid #333; padding:2px 4px; }
.q-line { white-space:pre; }
</style></head>
<body>
<div class=\"q-frame\" style=\"display:flex;flex-direction:column;height:100vh\">
<div class=\"q-header\">~a</div>
<div class=\"q-transcript\">~a</div>
<div class=\"q-status\">~a</div>
<div class=\"q-input\">~a</div>
</div>
</body></html>"
   width
   header-html
   transcript-html
   status-html
   input-html))

;; ============================================================
;; Provides
;; ============================================================

(provide (contract-out [vdom->html (->* (vnode?) (exact-nonnegative-integer?) string?)]
                       [styled-line->html (-> styled-line? string?)]
                       [styled-lines->html (-> (listof styled-line?) string?)]
                       [frame->html
                        (->* ((listof styled-line?) (listof styled-line?) styled-line? styled-line?)
                             (#:width exact-nonnegative-integer?)
                             string?)]
                       [html-escape (-> string? string?)]
                       [styles->css (-> (listof symbol?) string?)]
                       [ansi-color->css (-> exact-nonnegative-integer? string?)]))
