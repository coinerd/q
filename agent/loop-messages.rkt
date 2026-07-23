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
         (only-in "../util/content/content-parts.rkt" text-part-text text-part?)
         (only-in "../util/message/message.rkt" message?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/message/provider-transport.rkt"
                  [messages->provider-hashes serialize-provider-messages]))

(provide (contract-out [usage-empty? (-> (or/c hash? #f) boolean?)]
                       [parts->text-string (-> (or/c string? list? any/c) string?)]
                       [valid-api-message-sequence? (-> (listof hash?) boolean?)]
                       [merge-consecutive-roles (-> (listof hash?) (listof hash?))]
                       [build-raw-messages (-> (listof message?) (listof hash?))]
                       [classify-hook-result
                        (-> any/c (or/c 'pass (list/c 'block any/c) (list/c 'amend any/c)))]
                       [message-role->api-role (-> symbol? string?)]))

;; ============================================================
;; Helpers
;; ============================================================

;; Convert internal message role symbol to API role string.
;; Centralizes role mapping so providers don't repeat this logic.
(define (message-role->api-role role)
  (case role
    [(user) "user"]
    [(assistant) "assistant"]
    [(system) "system"]
    [(tool) "tool"]
    [else (symbol->string role)]))

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
  (reverse
   (for/fold ([acc '()]) ([msg (in-list msgs)])
     (cond
       [(null? acc) (list msg)]
       [(equal? (hash-ref msg 'role #f) (hash-ref (car acc) 'role #f))
        ;; Never merge tool messages (each has its own correlation ID), or
        ;; assistant tool-call turns (merging only content would drop calls).
        (if (or (equal? (hash-ref msg 'role #f) "tool")
                (and (equal? (hash-ref msg 'role #f) "assistant")
                     (or (pair? (hash-ref msg 'tool_calls '()))
                         (pair? (hash-ref (car acc) 'tool_calls '())))))
            (cons msg acc) ; keep separate
            (let ()
              ;; M6: handle both string content and list content (image blocks)
              (define prev-content (hash-ref (car acc) 'content ""))
              (define this-content (hash-ref msg 'content ""))
              (define new-content
                (cond
                  [(and (string? prev-content) (string? this-content))
                   (string-append prev-content "\n\n" this-content)]
                  [(list? prev-content)
                   (append prev-content
                           (if (list? this-content)
                               this-content
                               (list this-content)))]
                  [(list? this-content) (append (list prev-content) this-content)]
                  [else
                   (string-append (format "~a" prev-content) "\n\n" (format "~a" this-content))]))
              (cons (hash-set (car acc) 'content new-content) (cdr acc))))]
       [else (cons msg acc)]))))

;; ============================================================
;; collect-system-messages-front
;; ============================================================

;; F8-FIX: Collect all system messages to front and merge into one.
;; Providers with strict Jinja templates (qwen, GLM) require system
;; messages ONLY at position 0. Upstream context assembly may interleave
;; system-instruction messages after user/assistant messages.
(define (collect-system-messages-front msgs)
  (define sys-msgs (filter (lambda (m) (equal? (hash-ref m 'role #f) "system")) msgs))
  (define non-sys-msgs (filter (lambda (m) (not (equal? (hash-ref m 'role #f) "system"))) msgs))
  (cond
    [(null? sys-msgs) non-sys-msgs]
    [(null? (cdr sys-msgs)) (append sys-msgs non-sys-msgs)]
    [else
     ;; Merge multiple system messages into one
     (define merged-content (string-join (map (lambda (m) (hash-ref m 'content "")) sys-msgs) "\n\n"))
     (cons (hasheq 'role "system" 'content merged-content) non-sys-msgs)]))

;; ============================================================
;; build-raw-messages
;; ============================================================

;; build-raw-messages : (listof message?) -> (listof hash?)
;; PURE function — converts a list of message? structs into raw OpenAI-format
;; hash messages. No side effects, no bus/state needed.
(define (build-raw-messages context)
  (define raw-msgs (serialize-provider-messages context))
  ;; Safety net: merge consecutive user messages.
  ;; Some providers (GLM) reject consecutive same-role messages.
  (define merged (collect-system-messages-front (merge-consecutive-roles raw-msgs)))
  ;; Two-pass orphan cleanup:
  ;;   Pass 1: Collect tool_call_ids from tool messages → answered-ids
  ;;   Pass 2: Strip orphaned assistant tool_calls + orphaned tool results
  ;;
  ;; Context assembly may drop assistant messages while keeping tool results,
  ;; or drop tool results while keeping assistant tool_calls. Both cases
  ;; cause API errors.
  (define answered-ids
    (for/fold ([ids (set)]) ([m (in-list merged)])
      (if (equal? (hash-ref m 'role #f) "tool")
          (let ([tcid (hash-ref m 'tool_call_id #f)])
            (if (and (string? tcid) (> (string-length tcid) 0))
                (set-add ids tcid)
                ids))
          ids)))
  (define merged-clean
    (let loop ([msgs merged]
               [assistant-ids (set)]
               [acc '()])
      (if (null? msgs)
          (reverse acc)
          (let* ([m (car msgs)]
                 [role (hash-ref m 'role #f)])
            (cond
              [(equal? role "assistant")
               (define tcs (hash-ref m 'tool_calls #f))
               (cond
                 [tcs
                  ;; Strip orphaned tool_calls whose id is not in answered-ids
                  (define kept-tcs
                    (for/list ([tc (in-list tcs)]
                               #:when (set-member? answered-ids (hash-ref tc 'id #f)))
                      tc))
                  (define new-ids
                    (for/fold ([s assistant-ids]) ([tc (in-list kept-tcs)])
                      (set-add s (hash-ref tc 'id #f))))
                  (cond
                    [(null? kept-tcs)
                     ;; All tool_calls were orphaned — strip entire tool_calls field
                     (when (pair? tcs)
                       (log-warning "BUILD-RAW: stripping ~a orphaned tool_calls from assistant msg"
                                    (length tcs)))
                     (loop (cdr msgs) assistant-ids (cons (hash-remove m 'tool_calls) acc))]
                    [(< (length kept-tcs) (length tcs))
                     ;; Some tool_calls were orphaned
                     (log-warning "BUILD-RAW: kept ~a of ~a tool_calls in assistant msg"
                                  (length kept-tcs)
                                  (length tcs))
                     (loop (cdr msgs) new-ids (cons (hash-set m 'tool_calls kept-tcs) acc))]
                    [else (loop (cdr msgs) new-ids (cons m acc))])]
                 [else (loop (cdr msgs) assistant-ids (cons m acc))])]
              [(equal? role "tool")
               (define tcid (hash-ref m 'tool_call_id #f))
               (if (and tcid (set-member? assistant-ids tcid))
                   (loop (cdr msgs) assistant-ids (cons m acc))
                   (begin
                     (log-warning "BUILD-RAW: dropping orphaned tool msg with tool_call_id=~a" tcid)
                     (loop (cdr msgs) assistant-ids acc)))]
              [else (loop (cdr msgs) assistant-ids (cons m acc))])))))
  ;; Safety net: providers with enable_thinking (qwen3/llama.cpp) reject
  ;; assistant-last messages as "prefill". Strip trailing assistant messages.
  (define merged-trimmed
    (if (and (pair? merged-clean) (equal? (hash-ref (last merged-clean) 'role #f) "assistant"))
        (begin
          (log-warning "DIAG: build-raw-messages: trimming trailing assistant message (~a total)"
                       (length merged-clean))
          (drop-right merged-clean 1))
        merged-clean))
  merged-trimmed)

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
