#lang racket/base

;; extensions/session-export.rkt — Session Export Extension
;; Phase E: Exports session JSONL files to HTML, JSON, or Markdown.

(require racket/string
         racket/file
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

(provide the-extension
         session-export-extension
         handle-session-export)

(define (read-session-lines path)
  (filter non-empty-string? (string-split (file->string path) "\n")))

(define (html-escape s)
  (let* ([s (string-replace s "&" "&amp;")]
         [s (string-replace s "<" "&lt;")]
         [s (string-replace s ">" "&gt;")]
         [s (string-replace s "\"" "&quot;")])
    s))

(define (entry->markdown line idx)
  (with-handlers ([exn:fail? (lambda (e) (format "### Entry ~a\n~a\n" idx line))])
    (define obj (string->jsexpr line))
    (format "### ~a\n~a\n"
            (string-titlecase (hash-ref obj 'role "unknown"))
            (hash-ref obj 'content ""))))

(define (entry->html line)
  (with-handlers ([exn:fail? (lambda (e) (format "<pre>~a</pre>" (html-escape line)))])
    (define obj (string->jsexpr line))
    (define role (hash-ref obj 'role "unknown"))
    (format "<div class=\"~a\"><h4>~a</h4><pre>~a</pre></div>"
            (html-escape role)
            (html-escape role)
            (html-escape (hash-ref obj 'content "")))))

(define (handle-session-export args [exec-ctx #f])
  (define path (hash-ref args 'session_path ""))
  (define fmt (hash-ref args 'format "markdown"))
  (define output (hash-ref args 'output_path ""))

  (when (string=? path "")
    (error 'session-export "session_path is required"))
  (unless (file-exists? path)
    (error 'session-export (format "File not found: ~a" path)))

  (define lines (read-session-lines path))
  (define result
    (cond
      [(string=? fmt "html")
       (string-append "<html><head><title>Session</title></head><body>"
                      (string-join (map entry->html lines) "\n")
                      "</body></html>")]
      [(string=? fmt "json")
       (jsexpr->string (for/list ([l lines])
                         (with-handlers ([exn:fail? (lambda (e) (hasheq 'raw l))])
                           (string->jsexpr l))))]
      [else
       (string-join (for/list ([l lines]
                               [i (in-naturals 1)])
                      (entry->markdown l i))
                    "\n")]))

  (cond
    [(non-empty-string? output)
     (call-with-output-file output (lambda (out) (display result out)) #:exists 'replace)
     (make-success-result (list (hasheq 'type "text" 'text (format "Exported to ~a" output))))]
    [else (make-success-result (list (hasheq 'type "text" 'text result)))]))

(define (register-export-tools ctx)
  (ext-register-tool! ctx
                      "session-export"
                      "Export session logs to HTML, JSON, or Markdown."
                      (hasheq 'type
                              "object"
                              'required
                              '("session_path")
                              'properties
                              (hasheq 'session_path
                                      (hasheq 'type "string" 'description "Session JSONL file path")
                                      'format
                                      (hasheq 'type "string" 'description "html|json|markdown")
                                      'output_path
                                      (hasheq 'type "string" 'description "Output file path")))
                      handle-session-export)
  (hook-pass ctx))

(define-q-extension session-export-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-export-tools)

(define the-extension session-export-extension)
