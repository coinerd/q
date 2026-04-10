#lang racket/base

;; q/util/export-html.rkt — Convert session messages to styled HTML
;;
;; Provides:
;;   session->html — (listof message?) -> string?

(require racket/string
         racket/format
         racket/list
         "../agent/types.rkt")

(provide session->html)

;; ── HTML escaping ──

(define (html-escape s)
  (define s1 (string-replace s "&" "&amp;"))
  (define s2 (string-replace s1 "<" "&lt;"))
  (define s3 (string-replace s2 ">" "&gt;"))
  (define s4 (string-replace s3 "\"" "&quot;"))
  (define s5 (string-replace s4 "'" "&#39;"))
  s5)

;; ── Helpers ──

(define (symbol->display-string s)
  (if (symbol? s) (symbol->string s) s))

;; ── CSS ──

(define INLINE-CSS
  "body { font-family: system-ui, sans-serif; max-width: 800px; margin: 2em auto; color: #222; background: #fafafa; }
.message { margin: 1.5em 0; padding: 1em; border-left: 3px solid #ccc; background: #fff; }
.message.user { border-color: #4a90d9; }
.message.assistant { border-color: #2d8659; }
.message.system { border-color: #e6a817; }
.role { font-weight: bold; font-size: 1.1em; margin-bottom: 0.5em; }
.timestamp { color: #888; font-size: 0.85em; margin-left: 1em; }
.content p { margin: 0.5em 0; }
.content pre { background: #f4f4f4; padding: 0.75em; overflow-x: auto; border-radius: 4px; }
.content code { font-family: monospace; }
details { margin: 0.5em 0; }
summary { cursor: pointer; font-weight: bold; color: #555; }
.error { color: #c33; border-left: 3px solid #c33; padding-left: 0.5em; }")

;; ── Content-part rendering ──

(define (render-text-part-html tp)
  (define text (text-part-text tp))
  (define lines (string-split text "\n"))
  (string-join
   (for/list ([line (in-list lines)])
     (format "<p>~a</p>" (html-escape line)))
   "\n"))

(define (render-tool-call-part-html tcp)
  (define name (tool-call-part-name tcp))
  (define args (tool-call-part-arguments tcp))
  (define args-str
    (cond
      [(string? args) args]
      [(hash? args) (~a args)]
      [else (~a args)]))
  (format "<details><summary>Tool: ~a</summary><pre><code>~a</code></pre></details>"
          (html-escape (~a name))
          (html-escape args-str)))

(define (render-tool-result-part-html trp)
  (define content (tool-result-part-content trp))
  (define is-err? (tool-result-part-is-error? trp))
  (define content-str
    (cond
      [(string? content) content]
      [(list? content)
       (string-join
        (for/list ([p (in-list content)])
          (cond
            [(string? p) p]
            [(hash? p) (~a p)]
            [else (~a p)]))
        "\n")]
      [(hash? content) (~a content)]
      [else (~a content)]))
  (if is-err?
      (format "<div class=\"error\"><strong>⚠ Error:</strong><pre><code>~a</code></pre></div>"
              (html-escape content-str))
      (format "<pre><code>~a</code></pre>"
              (html-escape content-str))))

(define (render-content-part-html cp)
  (cond
    [(text-part? cp) (render-text-part-html cp)]
    [(tool-call-part? cp) (render-tool-call-part-html cp)]
    [(tool-result-part? cp) (render-tool-result-part-html cp)]
    [else (format "<p>~a</p>" (html-escape (~a cp)))]))

;; ── Main entry point ──

(define (session->html messages)
  ;; Converts a list of message structs to a full HTML5 document.
  ;; Messages sorted by timestamp for deterministic output.
  (define sorted
    (sort messages < #:key message-timestamp))
  (define body-parts
    (for/list ([msg (in-list sorted)])
      (render-message-html msg)))
  (define body (string-join body-parts "\n"))
  (string-append
   "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   "<title>q Session Export</title>\n<style>\n" INLINE-CSS "\n</style>\n"
   "</head>\n<body>\n" body "\n</body>\n</html>"))

(define (render-message-html msg)
  (define role (symbol->display-string (message-role msg)))
  (define kind (symbol->display-string (message-kind msg)))
  (define ts (message-timestamp msg))
  (define content-parts (message-content msg))
  (define content-html
    (string-join
     (for/list ([cp (in-list content-parts)])
       (render-content-part-html cp))
     "\n"))
  (format "<div class=\"message ~a\"><div class=\"role\">~a (~a)<span class=\"timestamp\">~a</span></div><div class=\"content\">~a</div></div>"
          (html-escape role)
          (html-escape role)
          (html-escape kind)
          ts
          content-html))
