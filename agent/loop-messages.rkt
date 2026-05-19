#lang racket/base

;; agent/loop-messages.rkt — Message handling helpers
;;
;; Pure and near-pure utilities for building, validating, and
;; transforming messages for the agent loop.
;;
;; Extracted from loop.rkt (decomposition step).

(require racket/contract
         racket/string
         racket/list
         racket/set
         racket/match
         "../util/protocol-types.rkt"
         (only-in "../util/content-helpers.rkt" result-content->string)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload))

(provide (contract-out [usage-empty? (-> (or/c hash? #f) boolean?)]
                       [parts->text-string (-> (or/c string? list? any/c) string?)]
                       [valid-api-message-sequence? (-> (listof hash?) boolean?)]
                       [merge-consecutive-roles (-> (listof hash?) (listof hash?))]
                       [build-raw-messages (-> (listof message?) (listof hash?))]
                       [classify-hook-result
                        (-> any/c (or/c 'pass (list/c 'block any/c) (list/c 'amend any/c)))]))

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

;; ============================================================
;; Message sequence validation
;; ============================================================

;; Check that tool messages always follow assistant messages with tool_calls,
;; and that every tool_call_id in tool messages has a matching assistant tool_call.
(define (valid-api-message-sequence? msgs)
  ;; L-03: Pure immutable implementation using for/fold accumulator
  (define-values (result _seen-ids _prev-role)
    (for/fold ([valid? #t]
               [tool-call-ids (set)]
               [prev-role #f])
              ([m (in-list msgs)]
               [i (in-naturals)]
               #:break (not valid?))
      (define role (hash-ref m 'role #f))
      (cond
        [(and (equal? role "tool")
              (not (or (equal? prev-role "assistant") (equal? prev-role "tool"))))
         (log-warning "INVALID: msg[~a] role=tool follows role=~a (expected assistant or tool)"
                      i
                      prev-role)
         (values #f tool-call-ids role)]
        [(equal? role "assistant")
         (define tcs (hash-ref m 'tool_calls #f))
         (define new-ids
           (if tcs
               (for/fold ([s tool-call-ids]) ([tc (in-list tcs)])
                 (set-add s (hash-ref tc 'id #f)))
               tool-call-ids))
         (values #t new-ids role)]
        [(equal? role "tool")
         (define tcid (hash-ref m 'tool_call_id #f))
         (cond
           [(not tcid)
            (log-warning "INVALID: msg[~a] role=tool has no tool_call_id" i)
            (values #f tool-call-ids role)]
           [(not (set-member? tool-call-ids tcid))
            (log-warning
             "INVALID: msg[~a] role=tool tool_call_id=~a not found in preceding tool_calls"
             i
             tcid)
            (values #f tool-call-ids role)]
           [else (values #t tool-call-ids role)])]
        [else (values #t tool-call-ids role)])))
  result)

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

;; v0.32.4: Data-return replacement for CPS handle-hook-result.
;; Returns a discriminated union: (list 'block payload), (list 'amend payload), or 'pass.
;; Callers use `match` instead of callbacks.
(define (classify-hook-result result)
  (cond
    [(not (hook-result? result)) 'pass]
    [else
     (match (hook-result-action result)
       ['block (list 'block (hook-result-payload result))]
       ['amend (list 'amend (hook-result-payload result))]
       [_ 'pass])]))
