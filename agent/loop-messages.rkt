#lang racket/base

;; agent/loop-messages.rkt — Message handling helpers
;;
;; Pure and near-pure utilities for building, validating, and
;; transforming messages for the agent loop.  Also provides the
;; shared emit! helper used by loop.rkt and loop-stream.rkt.
;;
;; Extracted from loop.rkt (decomposition step).

(require racket/string
         racket/list
         racket/set
         racket/match
         "../util/protocol-types.rkt"
         "event-bus.rkt"
         "state.rkt"
         "../util/ids.rkt"
         (only-in "../util/content-helpers.rkt" result-content->string)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload))

(provide usage-empty?
         parts->text-string
         emit!
         valid-api-message-sequence?
         merge-consecutive-roles
         build-raw-messages
         handle-hook-result)

;; ============================================================
;; Helpers
;; ============================================================

;; Check whether usage hash is empty/zero (provider didn't return real usage).
(define (usage-empty? u)
  (and (hash? u)
       (or (hash-empty? u)
           (and (zero? (hash-ref u 'prompt_tokens 0)) (zero? (hash-ref u 'completion_tokens 0))))))

;; Extract plain text from a list of content parts (text-part only)
(define (parts->text-string parts)
  (cond
    [(string? parts) parts]
    [(list? parts)
     (string-join (for/list ([p (in-list parts)]
                             #:when (text-part? p))
                    (text-part-text p))
                  "")]
    [else (format "~a" parts)]))

;; Emit an event on the bus and optionally record in state
(define (emit! bus session-id turn-id event-name payload #:state [state #f])
  (define evt (make-event event-name (now-seconds) session-id turn-id payload))
  (publish! bus evt)
  (when state
    (state-add-event! state evt))
  evt)

;; ============================================================
;; Message sequence validation
;; ============================================================

;; Check that tool messages always follow assistant messages with tool_calls,
;; and that every tool_call_id in tool messages has a matching assistant tool_call.
(define (valid-api-message-sequence? msgs)
  (define tool-call-ids (mutable-set))
  (define prev-role #f)
  (for/and ([m (in-list msgs)]
            [i (in-naturals)])
    (define role (hash-ref m 'role #f))
    (cond
      [(and (equal? role "tool") (not (or (equal? prev-role "assistant") (equal? prev-role "tool"))))
       (log-warning "INVALID: msg[~a] role=tool follows role=~a (expected assistant or tool)"
                    i
                    prev-role)
       #f]
      [(equal? role "assistant")
       (define tcs (hash-ref m 'tool_calls #f))
       (when tcs
         (for ([tc (in-list tcs)])
           (set-add! tool-call-ids (hash-ref tc 'id #f))))
       #t]
      [(equal? role "tool")
       (define tcid (hash-ref m 'tool_call_id #f))
       (cond
         [(not tcid)
          (log-warning "INVALID: msg[~a] role=tool has no tool_call_id" i)
          #f]
         [(not (set-member? tool-call-ids tcid))
          (log-warning "INVALID: msg[~a] role=tool tool_call_id=~a not found in preceding tool_calls"
                       i
                       tcid)
          #f]
         [else #t])]
      [else #t])))

;; ============================================================
;; Role merging
;; ============================================================

;; Merge consecutive messages with the same role.
;; Some providers (GLM, some OpenAI-compatible) reject consecutive same-role messages.
(define (merge-consecutive-roles msgs)
  (reverse (for/fold ([acc '()]) ([msg (in-list msgs)])
             (cond
               [(null? acc) (list msg)]
               [(equal? (hash-ref msg 'role #f) (hash-ref (car acc) 'role #f))
                ;; Same role: merge content
                (define new-content
                  (string-append (hash-ref (car acc) 'content "") "\n\n" (hash-ref msg 'content "")))
                (cons (hash-set (car acc) 'content new-content) (cdr acc))]
               [else (cons msg acc)]))))

;; ============================================================
;; build-raw-messages
;; ============================================================

;; build-raw-messages : (listof message?) -> (listof hash?)
;; PURE function — converts a list of message? structs into raw OpenAI-format
;; hash messages. No side effects, no bus/state needed.
(define (build-raw-messages context)
  (define raw-msgs
    (append*
     (for/list ([msg (in-list context)])
       (define role (message-role msg))
       (define parts (message-content msg))
       (cond
         ;; user -> simple text message
         [(eq? role 'user) (list (hasheq 'role "user" 'content (parts->text-string parts)))]

         ;; assistant -> text + optional tool_calls
         [(eq? role 'assistant)
          (define text-parts (filter text-part? parts))
          (define tc-parts (filter tool-call-part? parts))
          (define text-content (parts->text-string text-parts))
          (if (null? tc-parts)
              ;; text-only assistant message
              (list (hasheq 'role "assistant" 'content text-content))
              ;; assistant with tool calls -- OpenAI format
              ;; Note: GLM rejects 'null for content, so we omit the field when empty
              (let* ([tool-calls-list (for/list ([tc (in-list tc-parts)])
                                        (hasheq 'id
                                                (tool-call-part-id tc)
                                                'type
                                                "function"
                                                'function
                                                (hasheq 'name
                                                        (tool-call-part-name tc)
                                                        'arguments
                                                        (tool-call-part-arguments tc))))]
                     [assistant-msg (if (string=? text-content "")
                                        ;; No text content: omit content field (GLM compatible)
                                        (hasheq 'role "assistant" 'tool_calls tool-calls-list)
                                        ;; Has text content: include content field
                                        (hasheq 'role
                                                "assistant"
                                                'content
                                                text-content
                                                'tool_calls
                                                tool-calls-list))])
                (list assistant-msg)))]

         ;; tool -> one OpenAI message per tool-result-part
         [(eq? role 'tool)
          (for/list ([p (in-list parts)]
                     #:when (tool-result-part? p))
            (hasheq 'role
                    "tool"
                    'tool_call_id
                    (tool-result-part-tool-call-id p)
                    'content
                    (result-content->string (tool-result-part-content p))))]

         ;; fallback -- unknown role
         [else (list (hasheq 'role (symbol->string role) 'content (parts->text-string parts)))]))))
  ;; Safety net: merge consecutive user messages.
  ;; Some providers (GLM) reject consecutive same-role messages.
  (merge-consecutive-roles raw-msgs))

;; ============================================================
;; Match-based hook dispatch (v0.29.1: §10 Match Dispatch)
;; ============================================================

(define (handle-hook-result result on-block on-continue #:on-amend [on-amend #f])
  (cond
    [(not (hook-result? result)) (on-continue)]
    [else
     (match (hook-result-action result)
       ['block (on-block (hook-result-payload result))]
       ['amend
        (when on-amend
          (on-amend (hook-result-payload result)))
        (on-continue)]
       [_ (on-continue)])]))
