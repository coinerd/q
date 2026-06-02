#lang racket/base

;; util/tool-display.rkt — shared tool-call display helpers
;;
;; Extracted from cli/render.rkt and tui/state-events.rkt to eliminate
;; near-duplicate arg extraction and formatting logic.

(require racket/contract
         racket/format
         json
         (only-in "../string-helpers.rkt" truncate-string))

(provide (contract-out
          [extract-arg-summary (-> any/c (or/c string? #f))]
          [format-tool-call-display (-> string? any/c string?)]))

;; Extract a short summary string from tool-call arguments.
;; Returns #f if no useful detail can be extracted.
(define (extract-arg-summary args-raw)
  (define args
    (cond
      [(hash? args-raw) args-raw]
      [(string? args-raw)
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (define parsed (string->jsexpr args-raw))
         (and (hash? parsed) parsed))]
      [else #f]))
  (cond
    [(and args (hash? args))
     (define cmd
       (or (hash-ref args 'command #f)
           (hash-ref args 'path #f)
           (hash-ref args 'pattern #f)
           #f))
     (if cmd
         (truncate-string (~a cmd) 100)
         #f)]
    [else #f]))

;; Format a tool call as "[tool: name: detail]" or "[tool: name]".
(define (format-tool-call-display name args-raw)
  (define detail (extract-arg-summary args-raw))
  (if detail
      (format "[tool: ~a: ~a]" name detail)
      (format "[tool: ~a]" name)))
