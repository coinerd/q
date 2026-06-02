#lang racket/base

;; q/util/export-json.rkt — Convert session messages to structured JSON
;;
;; Provides:
;;   session->json-string — (listof message?) -> string?

(require racket/contract
         racket/string
         racket/format
         racket/list
         json
         (only-in "../message.rkt" message->jsexpr))

(provide (contract-out [session->json-string (-> list? string?)]))

;; ── Main entry point ──

(define (session->json-string messages)
  ;; Converts a list of message structs to a JSON string.
  ;; Wraps entries in a session envelope with metadata.
  (define entries
    (for/list ([msg (in-list messages)])
      (message->jsexpr msg)))
  (define envelope
    (hasheq 'session
            (hasheq 'entries entries 'exported_at (current-seconds) 'export_format_version "0.5.3")))
  (jsexpr->string envelope))
