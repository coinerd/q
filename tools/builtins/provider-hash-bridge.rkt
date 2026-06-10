#lang racket/base

;; tools/builtins/provider-hash-bridge.rkt — message→provider hash conversion
;; STABILITY: internal
;;
;; M10 (v0.97.15): Extracted from spawn-subagent.rkt for reuse.
;; Converts q message/content-part structs to provider-facing JSON hashes.

(require racket/string
         (only-in "../../util/content/content-parts.rkt"
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  tool-result-part?
                  tool-result-part-tool-call-id
                  tool-result-part-content
                  tool-result-part-is-error?
                  content-part?)
         (only-in "../../util/message/message.rkt" message-role message-content))

(provide message->provider-hash
         content-part->provider-hash
         messages->provider-hashes)

;; Convert a message struct to a provider-facing hash for JSON serialization.
(define (message->provider-hash msg)
  (define role-sym (message-role msg))
  (define role-str
    (if (symbol? role-sym)
        (symbol->string role-sym)
        (format "~a" role-sym)))
  (define content (message-content msg))
  (define content-val
    (cond
      [(string? content) content]
      [(null? content) ""]
      [(and (list? content) (andmap string? content)) (string-join content "\n")]
      [(and (list? content) (andmap content-part? content))
       (for/list ([cp (in-list content)])
         (content-part->provider-hash cp))]
      [(list? content)
       (for/list ([c (in-list content)])
         (cond
           [(string? c) c]
           [(content-part? c) (content-part->provider-hash c)]
           [(hash? c) c]
           [else (format "~a" c)]))]
      [else (format "~a" content)]))
  (hasheq 'role role-str 'content content-val))

;; Convert content-part to provider-facing hash for JSON serialization.
(define (content-part->provider-hash cp)
  (cond
    [(text-part? cp) (hasheq 'type "text" 'text (text-part-text cp))]
    [(tool-call-part? cp)
     (hasheq 'type
             "tool_call"
             'id
             (or (tool-call-part-id cp) "")
             'name
             (or (tool-call-part-name cp) "")
             'arguments
             (tool-call-part-arguments cp))]
    [(tool-result-part? cp)
     (hasheq 'type
             "tool_result"
             'tool_call_id
             (or (tool-result-part-tool-call-id cp) "")
             'content
             (tool-result-part-content cp)
             'is_error
             (tool-result-part-is-error? cp))]
    [else (hasheq 'type "text" 'text (format "~a" cp))]))

;; Convert a list of message structs to provider-facing JSON hashes.
(define (messages->provider-hashes msgs)
  (map message->provider-hash msgs))
