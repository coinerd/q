#lang racket/base

;; q/util/export-markdown.rkt — Convert session messages to Markdown
;;
;; Provides:
;;   session->markdown — (listof message?) -> string?

(require racket/string
         racket/format
         racket/list
         "../util/protocol-types.rkt")

(provide session->markdown)

;; ── Helpers ──

(define (symbol->display-string s)
  (if (symbol? s) (symbol->string s) s))

(define (message-sort-key msg)
  (message-timestamp msg))

;; ── Content-part rendering ──

(define (render-text-part tp)
  (text-part-text tp))

(define (render-tool-call-part tcp)
  (define name (tool-call-part-name tcp))
  (define args (tool-call-part-arguments tcp))
  (define args-str
    (cond
      [(string? args) args]
      [(hash? args) (~a args)]
      [else (~a args)]))
  (string-append
   (format "**Tool: ~a**\n" name)
   "```json\n"
   args-str
   "\n```\n"))

(define (render-tool-result-part trp)
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
      (string-append "> ⚠ Error:\n```\n" content-str "\n```\n")
      (string-append "```\n" content-str "\n```\n")))

(define (render-content-part cp)
  (cond
    [(text-part? cp) (render-text-part cp)]
    [(tool-call-part? cp) (render-tool-call-part cp)]
    [(tool-result-part? cp) (render-tool-result-part cp)]
    [else (~a cp)]))

;; ── Main entry point ──

(define (session->markdown messages)
  ;; Converts a list of message structs to a Markdown string.
  ;; Messages are sorted by timestamp for deterministic output.
  (define sorted
    (sort messages < #:key message-sort-key))
  (define parts
    (for/list ([msg (in-list sorted)])
      (render-message msg)))
  (string-join parts "\n"))

(define (render-message msg)
  (define role (symbol->display-string (message-role msg)))
  (define kind (symbol->display-string (message-kind msg)))
  (define ts (message-timestamp msg))
  (define content-parts (message-content msg))
  (define heading (format "### ~a (~a)" role kind))
  (define ts-line (format "_Timestamp: ~a_" ts))
  (define body
    (string-join
     (for/list ([cp (in-list content-parts)])
       (render-content-part cp))
     "\n"))
  (string-join (list heading ts-line body) "\n"))
