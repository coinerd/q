#lang racket/base

;; tests/workflows/fixtures/mock-provider.rkt — scripted provider
;;
;; Deterministic mock provider that consumes a script of responses.
;; Each script entry is consumed in order across turns (calls to stream/send).

(require racket/generator
         json
         "../../../llm/provider.rkt"
         (only-in "../../../llm/model.rkt"
                  make-model-response
                  stream-chunk))

(provide make-scripted-provider
         text-response
         tool-call-response
         done-response
         error-response)

;; ============================================================
;; Convenience constructors for script entries
;; ============================================================

(define (text-response text)
  (hasheq 'type "text" 'text text))

(define (tool-call-response id name arguments)
  (hasheq 'type "tool-call" 'id id 'name name 'arguments arguments))

(define (done-response)
  (hasheq 'type "done"))

(define (error-response message)
  (hasheq 'type "error" 'message message))

;; ============================================================
;; Scripted provider
;; ============================================================

;; make-scripted-provider : (listof hash) -> provider?
;; Each element of script is consumed per provider call (stream invocation).
;; For multi-turn with tool calls:
;;   (list (tool-call-response "tc-1" "read" (hash 'path "foo.rkt"))
;;         (text-response "The file contains hello world"))

(define (make-scripted-provider script
                                 #:name [name "scripted-mock"]
                                 #:usage [usage (hasheq 'prompt_tokens 10
                                                        'completion_tokens 5
                                                        'total_tokens 15)])
  (define idx (box 0))

  (define (next-entry!)
    (define i (unbox idx))
    (set-box! idx (add1 i))
    (if (< i (length script))
        (list-ref script i)
        (hasheq 'type "text" 'text "done")))

  (define (entry->content entry)
    (define typ (hash-ref entry 'type "text"))
    (cond
      [(equal? typ "tool-call")
       (define args (hash-ref entry 'arguments (hash)))
       (list (hash 'type "tool-call"
                   'id (hash-ref entry 'id "tc-unknown")
                   'name (hash-ref entry 'name "unknown")
                   'arguments (if (hash? args) (jsexpr->string args) args)))]
      [(equal? typ "error")
       (list (hash 'type "text" 'text (format "Error: ~a" (hash-ref entry 'message "unknown"))))]
      [(equal? typ "done")
       (list (hash 'type "text" 'text "done"))]
      [else
       (list (hash 'type "text" 'text (hash-ref entry 'text "")))]))

  (define (entry->stop-reason entry)
    (if (equal? (hash-ref entry 'type "text") "tool-call")
        'tool-calls
        'stop))

  ;; send proc (non-streaming — required by make-provider but not used by agent loop)
  (define (send-proc req)
    (define entry (next-entry!))
    (make-model-response
     (entry->content entry)
     usage
     name
     (entry->stop-reason entry)))

  ;; stream proc (used by agent loop via provider-stream)
  (define (stream-proc req)
    (define entry (next-entry!))
    (define typ (hash-ref entry 'type "text"))
    (cond
      [(equal? typ "tool-call")
       (define args (hash-ref entry 'arguments (hash)))
       (define args-str (if (hash? args) (jsexpr->string args) args))
       ;; Delta format must match what accumulate-tool-call-deltas expects:
       ;;   { 'id "...", 'function { 'name "...", 'arguments "..." } }
       (define tc-delta (hasheq 'id (hash-ref entry 'id "tc-unknown")
                                'index 0
                                'function (hasheq 'name (hash-ref entry 'name "unknown")
                                                  'arguments args-str)))
       (list (make-stream-chunk #f tc-delta #f #f)
             (make-stream-chunk #f #f usage #t))]
      [(equal? typ "text")
       (define text (hash-ref entry 'text ""))
       (if (string=? text "")
           (list (make-stream-chunk #f #f usage #t))
           (list (make-stream-chunk text #f #f #f)
                 (make-stream-chunk #f #f usage #t)))]
      [(equal? typ "error")
       (define msg (format "Error: ~a" (hash-ref entry 'message "unknown")))
       (list (make-stream-chunk msg #f #f #f)
             (make-stream-chunk #f #f usage #t))]
      [else
       (list (make-stream-chunk (format "~a" entry) #f #f #f)
             (make-stream-chunk #f #f usage #t))]))

  (make-provider (lambda () name)
                 (lambda () (hash 'streaming #t 'token-counting #t))
                 send-proc
                 stream-proc))
