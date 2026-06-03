#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/helpers/provider-scenarios.rkt — Data-first fake provider scenarios
;;
;; Reusable scenario factories for testing provider interactions.
;; Plain functions and structs — no macros.

(require racket/list
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         )

(provide scenario-response
         scenario-text
         scenario-tool-call
         scenario-multi-tool
         scenario-streaming
         scenario-streaming-with-tools
         scenario-error
         scenario-rate-limit
         scenario-finish-length
         make-scenario-provider
         captured-requests
         clear-captured!)

;; ---------------------------------------------------------------------------
;; Response scenario constructors
;; ---------------------------------------------------------------------------

(define (scenario-text text #:usage [usage (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)])
  "Create a simple text response scenario."
  (make-model-response
   (list (hash 'type "text" 'text text))
   usage
   "scenario"
   'stop))

(define (scenario-tool-call name #:arguments [args (hash)] #:id [id "tc-1"]
                             #:usage [usage (hasheq 'prompt-tokens 10 'completion-tokens 8 'total-tokens 18)])
  "Create a single tool-call response scenario."
  (make-model-response
   (list (hash 'type "tool-call" 'id id 'name name 'arguments args))
   usage
   "scenario"
   'tool-calls))

(define (scenario-multi-tool calls
                              #:usage [usage (hasheq 'prompt-tokens 12 'completion-tokens 10 'total-tokens 22)])
  "Create a multi-tool-call response scenario.  calls = ((name args) ...)"
  (make-model-response
   (for/list ([c (in-list calls)]
              [i (in-naturals)])
     (hash 'type "tool-call"
           'id (format "tc-~a" i)
           'name (car c)
           'arguments (if (null? (cdr c)) (hash) (cadr c))))
   usage
   "scenario"
   'tool-calls))

(define (scenario-response stop-reason content-parts
                            #:usage [usage (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)]
                            #:model [model-name "scenario"])
  "Create a generic response with explicit stop-reason and content parts."
  (make-model-response content-parts usage model-name stop-reason))

(define (scenario-finish-length text
                                 #:usage [usage (hasheq 'prompt-tokens 100 'completion-tokens 50 'total-tokens 150)])
  "Create a response with finish_reason=length (truncated)."
  (make-model-response
   (list (hash 'type "text" 'text text))
   usage
   "scenario"
   'length))

;; ---------------------------------------------------------------------------
;; Streaming scenario constructor
;; ---------------------------------------------------------------------------

(define (scenario-streaming chunks-texts #:final-usage [usage (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)])
  "Create a streaming scenario that emits text chunks then a final chunk with usage."
  (append
   (for/list ([text (in-list chunks-texts)])
     (make-stream-chunk text #f #f #f))
   (list (make-stream-chunk #f #f usage #t))))

(define (scenario-streaming-with-tools tool-calls #:final-usage [usage (hasheq 'prompt-tokens 10 'completion-tokens 8 'total-tokens 18)])
  "Streaming scenario that emits tool-call chunks."
  (append
   (for/list ([tc (in-list tool-calls)]
              [i (in-naturals)])
     (make-stream-chunk
      #f
      (hasheq 'index i
              'id (format "tc-~a" i)
              'function (hasheq 'name (car tc)
                                'arguments (if (string? (cadr tc))
                                               (cadr tc)
                                               "{}")))
      #f
      #f))
   (list (make-stream-chunk #f #f usage #t))))

;; ---------------------------------------------------------------------------
;; Error scenario constructors
;; ---------------------------------------------------------------------------

(define (scenario-error message #:category [category 'server-error] #:status [status 500])
  "Create an error scenario (returns error on send/stream)."
  (list 'error message category status))

(define (scenario-rate-limit #:retry-after [retry-after 1])
  "Create a retryable rate-limit error scenario."
  (list 'rate-limit "rate limited" 'rate-limit 429 retry-after))

;; ---------------------------------------------------------------------------
;; Provider factory with request capture
;; ---------------------------------------------------------------------------

(define (captured-requests capture-box)
  "Get the list of captured requests from a capture box."
  (unbox capture-box))

(define (clear-captured! capture-box)
  "Clear captured requests."
  (set-box! capture-box '()))

(define (make-scenario-provider responses #:name [name "scenario"])
  "Create a provider that returns responses in sequence.
   Captures request bodies in a box returned as the second value.
   Returns (values provider capture-box)."
  (define idx (box 0))
  (define capture-box (box '()))

  (define (get-next)
    (if (< (unbox idx) (length responses))
        (list-ref responses (unbox idx))
        (last responses)))

  (define (advance!)
    (set-box! idx (add1 (unbox idx))))

  (define (handle-send req)
    (set-box! capture-box (append (unbox capture-box) (list req)))
    (define resp (get-next))
    (advance!)
    (cond
      [(and (list? resp) (eq? (car resp) 'error))
       (raise (exn:fail (cadr resp) (current-continuation-marks)))]
      [(and (list? resp) (eq? (car resp) 'rate-limit))
       (raise (exn:fail (cadr resp) (current-continuation-marks)))]
      [else resp]))

  (define (handle-stream req)
    (set-box! capture-box (append (unbox capture-box) (list req)))
    (define resp (get-next))
    (advance!)
    (cond
      [(and (list? resp) (eq? (car resp) 'error))
       (raise (exn:fail (cadr resp) (current-continuation-marks)))]
      [(and (list? resp) (eq? (car resp) 'rate-limit))
       (raise (exn:fail (cadr resp) (current-continuation-marks)))]
      [(list? resp)
       ;; streaming chunks list
       (in-list resp)]
      [else
       ;; model-response → generate text chunks
       (define content-parts (model-response-content resp))
       (append
        (for/list ([part (in-list content-parts)]
                   #:when (equal? (hash-ref part 'type #f) "text"))
          (make-stream-chunk (hash-ref part 'text "") #f #f #f))
        (list (make-stream-chunk #f #f (model-response-usage resp) #t)))]))

  (values
   (make-provider
    (lambda () name)
    (lambda () (hash 'streaming #t 'token-counting #t))
    handle-send
    handle-stream)
   capture-box))
