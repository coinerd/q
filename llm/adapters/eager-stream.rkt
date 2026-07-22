#lang racket/base

;; llm/adapters/eager-stream.rkt — Generic eager-stream adapter
;;
;; Adapts a non-streaming (synchronous) LLM completion function into a
;; streaming generator that yields stream-chunks as if they arrived
;; incrementally over SSE.
;;
;; Use case: providers (like Kimi via Anthropic-compatible API) that do
;; not support native streaming but work with non-streaming JSON responses.
;; Instead of duplicating HTTP logic, this adapter wraps a plain
;; completion function into the provider-stream contract.
;;
;; The completion function receives a model-request? and returns a jsexpr?
;; (the full parsed JSON response body).  The adapter:
;;   1. Parses the response into a model-response
;;   2. Emits content_block_delta chunks for each text part
;;   3. Emits tool-call delta chunks for each tool-call part
;;   4. Emits a final done chunk with usage and finish-reason

(require racket/contract
         racket/generator
         racket/match
         json
         "../model.rkt")

;; ============================================================
;; Contract for the completion function
;; ============================================================

(define completion-fn/c (-> model-request? jsexpr?))

;; ============================================================
;; Conversion helpers (response content → stream chunks)
;; ============================================================

;; content->chunks : list? -> (listof stream-chunk?)
;; Convert a list of response content blocks into a list of stream-chunks.
;; Text blocks become text deltas; tool-call blocks become tool-call deltas.
(define (content->chunks content-parts)
  (for/list ([part (in-list content-parts)]
             [idx (in-naturals)])
    (cond
      [(hash-ref part 'text #f) (make-stream-chunk (hash-ref part 'text) #f #f #f)]
      [(equal? (hash-ref part 'type #f) "tool-call")
       (define raw-id (hash-ref part 'id #f))
       (define tcid
         (if (and (string? raw-id) (> (string-length raw-id) 0))
             raw-id
             (let ([sym (gensym)]) (format "tc_~a" sym))))
       (define args-str (jsexpr->string (hash-ref part 'arguments (hasheq))))
       (make-stream-chunk #f
                          (hasheq 'index
                                  idx
                                  'id
                                  tcid
                                  'function
                                  (hasheq 'name (hash-ref part 'name "") 'arguments args-str))
                          #f
                          #f)]
      [else (make-stream-chunk "" #f #f #f)])))

;; ============================================================
;; Eager-stream generator
;; ============================================================

;; eager-stream : completion-fn/c model-request? [#:parse-response parse-fn]
;;               [#:default-model string?] -> generator?
;;
;; Build a streaming generator from a non-streaming completion function.
;;
;; The completion-fn receives a model-request? (with streaming disabled)
;; and must return a jsexpr? — the parsed JSON response body.
;;
;; The #:parse-response function converts a jsexpr? into a model-response?.
;; If not provided, the adapter attempts jsexpr->model-response; if that
;; fails, it falls back to using the raw jsexpr.
;;
;; The generator yields stream-chunk? values (text deltas, tool-call deltas,
;; done chunk with usage) and then #f repeatedly at stream end.
(define (eager-stream completion-fn
                      req
                      #:parse-response [parse-response #f]
                      #:default-model [default-model #f])
  (define resp-json (completion-fn req))
  (define resp
    (if parse-response
        (parse-response resp-json)
        (with-handlers ([exn:fail? (lambda (e) resp-json)])
          (jsexpr->model-response resp-json))))
  (define content-parts (model-response-content resp))
  (define chunks
    (append (content->chunks content-parts)
            (let ([usage (model-response-usage resp)]
                  [stop-reason (model-response-stop-reason resp)])
              (if usage
                  (list (make-stream-chunk #f
                                           #f
                                           usage
                                           #t
                                           #:finish-reason (if stop-reason
                                                               (symbol->string stop-reason)
                                                               #f)))
                  '()))))
  (generator ()
             (for ([ch (in-list chunks)])
               (yield ch))
             (let loop ()
               (yield #f)
               (loop))))

;; ============================================================
;; Provide
;; ============================================================

(provide (contract-out [eager-stream
                        (->* (completion-fn/c model-request?)
                             (#:parse-response (or/c procedure? #f) #:default-model (or/c string? #f))
                             generator?)]))
