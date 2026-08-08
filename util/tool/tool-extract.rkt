#lang racket/base

;; util/tool/tool-extract.rkt — pure tool-call extraction from messages
;;
;; Extracted from runtime/tool-coordinator.rkt to make the pure extraction
;; logic available to all layers without importing the Runtime tool-coordinator
;; implementation.
;;
;; Provides:
;;   extract-tool-calls-from-messages — extract tool-call structs from assistant messages
;;   find-malformed-tool-calls — locate tool-call parts with invalid JSON arguments
;;   parse-tool-call-args — safely parse raw tool-call arguments to hash

(require racket/contract
         (only-in "../json/json-helpers.rkt" ensure-hash-args)
         (only-in "../message/message.rkt" message-role message-content)
         (only-in "../content/content-parts.rkt"
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments)
         "tool-types.rkt")

(provide (contract-out [extract-tool-calls-from-messages (-> (listof any/c) (listof tool-call?))]
                       [find-malformed-tool-calls (-> (listof any/c) list?)]
                       [parse-tool-call-args (-> any/c (or/c hash? #f))]))

;; Parse raw tool-call arguments to a hash, returning #f on failure.
(define (parse-tool-call-args raw)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (ensure-hash-args raw)))

;; Extract tool-call structs from assistant messages.
;; Skips tool-call parts whose arguments are not valid JSON.
(define (extract-tool-calls-from-messages messages)
  (for*/list ([msg (in-list messages)]
              #:when (eq? (message-role msg) 'assistant)
              [part (in-list (message-content msg))]
              #:when (tool-call-part? part)
              [parsed (in-value (parse-tool-call-args (tool-call-part-arguments part)))]
              #:when parsed)
    (make-tool-call (tool-call-part-id part) (tool-call-part-name part) parsed)))

;; Locate assistant tool-call parts whose arguments are not valid JSON.
;; Returns (listof (hash 'id _ 'name _ 'raw _)) in message order.
(define (find-malformed-tool-calls messages)
  (for*/list ([msg (in-list messages)]
              #:when (eq? (message-role msg) 'assistant)
              [part (in-list (message-content msg))]
              #:when (tool-call-part? part)
              [parsed (in-value (parse-tool-call-args (tool-call-part-arguments part)))]
              #:when (not parsed))
    (define raw (tool-call-part-arguments part))
    (hasheq 'id (tool-call-part-id part) 'name (tool-call-part-name part) 'raw raw)))
