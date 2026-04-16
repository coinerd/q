#lang racket/base

;; llm/provider.rkt — provider interface contract
;;
;; Defines the provider struct and the generic interface that all
;; LLM provider adapters must satisfy. A provider wraps a dispatch
;; procedure that maps operation symbols to implementations.
;;
;; Dispatch protocol:
;;   'name          → string
;;   'capabilities  → hash
;;   'send          → (model-request → model-response) procedure
;;   'stream        → (model-request → (listof stream-chunk)) procedure

(require racket/contract
         racket/generator
         racket/string
         "model.rkt")

(provide provider?
         validate-api-key!
         ensure-model-setting
         (contract-out [provider-name (-> provider? string?)]
                       [provider-send (-> provider? model-request? any/c)]
                       [provider-stream (-> provider? model-request? any/c)]
                       [provider-capabilities (-> provider? hash?)]
                       [provider-count-tokens (-> provider? any/c (or/c #f integer?))]
                       [make-provider (-> procedure? procedure? procedure? procedure? provider?)]
                       [make-mock-provider
                        (->* (any/c) (#:name string? #:stream-chunks (or/c #f list?)) provider?)]))

;; ============================================================
;; Model-setting helper (ARCH-08)
;; ============================================================

;; Ensure the model-request has a 'model setting; if missing, set it to default-model.
(define (ensure-model-setting req default-model)
  (if (hash-has-key? (model-request-settings req) 'model)
      req
      (make-model-request (model-request-messages req)
                          (model-request-tools req)
                          (hash-set (model-request-settings req) 'model default-model))))

;; ============================================================
;; API key validation helper
;; ============================================================

;; Validates that the API key is present and non-empty.
;; Raises exn:fail with a provider-specific message if missing.
(define (validate-api-key! provider-name env-var config)
  (define api-key (hash-ref config 'api-key ""))
  (when (or (not api-key) (not (string? api-key)) (string=? (string-trim api-key) ""))
    (raise
     (exn:fail
      (format
       "~a API key not set. Set ~a environment variable or add 'api-key' to config.json. Run 'q config' for setup."
       provider-name
       env-var)
      (current-continuation-marks)))))

;; ============================================================
;; Provider struct
;; ============================================================

;; A provider wraps a dispatch function that takes a symbol and returns
;; the corresponding implementation.
(struct provider (dispatch) #:transparent)

;; ============================================================
;; Generic interface
;; ============================================================

(define (provider-name p)
  ((provider-dispatch p) 'name))

(define (provider-send p req)
  (define send-proc ((provider-dispatch p) 'send))
  (send-proc req))

(define (provider-stream p req)
  (define stream-proc ((provider-dispatch p) 'stream))
  (stream-proc req))

(define (provider-capabilities p)
  ((provider-dispatch p) 'capabilities))

;; ============================================================
;; Constructor
;; ============================================================

;; Creates a provider from four thunks/procs:
;;   name-proc:  (→ string)
;;   caps-proc:  (→ hash)
;;   send-proc:  (model-request → model-response)
;;   stream-proc: (model-request → (or/c generator? (listof stream-chunk?))) procedure
;;     The stream proc may return a generator (yielding stream-chunk? values then #f)
;;     or a list of stream-chunk? values (automatically wrapped in a generator).
(define (stream-result->generator result)
  (cond
    ;; Already a generator (generators are procedures)
    [(procedure? result) result]
    [(list? result)
     (generator ()
                (for ([ch (in-list result)])
                  (yield ch))
                (yield #f))]
    [else
     (error 'stream-result->generator
            "expected generator or list of stream-chunks, got: ~a"
            result)]))

(define (make-provider name-proc caps-proc send-proc stream-proc)
  (provider (lambda (op)
              (case op
                [(name) (name-proc)] ; name-proc is a thunk, call it
                [(capabilities) (caps-proc)] ; caps-proc is a thunk, call it
                [(send) send-proc]
                [(stream) (lambda (req) (stream-result->generator (stream-proc req)))]
                [(count-tokens) (lambda (req) #f)] ; not supported by default
                [else (error 'provider "unknown operation: ~a" op)]))))

;; ============================================================
;; count-tokens protocol method
;; ============================================================

;; Returns #f for providers that don't support token counting,
;; or an integer count for providers that do.
(define (provider-count-tokens p req)
  (define count-proc ((provider-dispatch p) 'count-tokens))
  (count-proc req))

;; ============================================================
;; Mock provider (for testing)
;; ============================================================

(define (make-mock-provider response #:name [name "mock"] #:stream-chunks [stream-chunks #f])
  (define chunks
    (or stream-chunks
        (let ([content-parts (model-response-content response)])
          (define text-parts
            (filter (lambda (c) (equal? (hash-ref c 'type #f) "text")) content-parts))
          (append (for/list ([tp (in-list text-parts)])
                    (make-stream-chunk (hash-ref tp 'text "") #f #f #f))
                  (list (make-stream-chunk #f #f (model-response-usage response) #t))))))
  (make-provider (lambda () name)
                 (lambda () (hasheq 'streaming #t 'token-counting #t))
                 (lambda (req) response)
                 (lambda (req)
                   ;; Return a generator that yields each chunk then #f
                   (generator ()
                              (for ([ch (in-list chunks)])
                                (yield ch))
                              (yield #f)))))
