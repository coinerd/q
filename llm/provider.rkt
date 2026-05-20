#lang racket/base
;; TR NOTE: Remains #lang racket/base due to define-generics incompatibility with Typed Racket.

;; llm/provider.rkt — provider interface contract
;;
;; Defines the provider generic interface that all LLM provider adapters
;; must satisfy. Uses racket/generic for type-safe dispatch.
;;
;; Generic methods:
;;   provider-name         → string
;;   provider-capabilities → hash
;;   provider-send         → (model-request → model-response)
;;   provider-stream       → (model-request → generator)
;;   provider-count-tokens → (model-request → (or/c #f integer?))

(require racket/contract
         racket/match
         racket/generator
         racket/generic
         racket/string
         "model.rkt"
         (only-in "../util/errors.rkt" raise-credential-error))

(provide provider?
         validate-api-key!
         ensure-model-setting
         gen:provider
         (contract-out
          [provider-name (-> provider? string?)]
          [provider-send (-> provider? model-request? model-response?)]
          [provider-stream (-> provider? model-request? generator?)]
          [provider-capabilities (-> provider? hash?)]
          [provider-count-tokens (-> provider? model-request? (or/c #f exact-nonnegative-integer?))]
          [make-provider
           (-> (-> string?)
               (-> hash?)
               (-> model-request? model-response?)
               (-> model-request? (or/c generator? list?))
               provider?)]
          [make-mock-provider
           (->* (model-response?) (#:name string? #:stream-chunks (or/c #f list?)) provider?)]))

;; ============================================================
;; Model-setting helper (ARCH-08)
;; ============================================================

(define (ensure-model-setting req default-model)
  (if (hash-has-key? (model-request-settings req) 'model)
      req
      (make-model-request (model-request-messages req)
                          (model-request-tools req)
                          (hash-set (model-request-settings req) 'model default-model))))

;; ============================================================
;; API key validation helper
;; ============================================================

(define (validate-api-key! provider-name env-var config)
  (define api-key (hash-ref config 'api-key ""))
  (when (or (not api-key) (not (string? api-key)) (string=? (string-trim api-key) ""))
    (raise-credential-error
     (format
      "~a API key not set. Set ~a environment variable or add 'api-key' to config.json. Run 'q config' for setup."
      provider-name
      env-var)
     provider-name)))

;; ============================================================
;; Generic provider interface
;; ============================================================

(define-generics provider
                 (provider-name provider)
                 (provider-capabilities provider)
                 (provider-send provider req)
                 (provider-stream provider req)
                 (provider-count-tokens provider req)
                 #:fallbacks [(define (provider-count-tokens p req)
                                #f)])

;; ============================================================
;; Helper
;; ============================================================

(define (stream-result->generator result)
  (match result
    [(? procedure?) result]
    [(? list?)
     (generator ()
                (for ([ch (in-list result)])
                  (yield ch))
                (yield #f))]
    [_
     (raise-arguments-error 'stream-result->generator
                            "expected generator or list of stream-chunks"
                            "got"
                            result)]))

;; ============================================================
;; Internal struct implementing gen:provider
;; ============================================================

;; Simple procedure-based provider. Adapters create this via make-provider.
;; Direct struct users can implement gen:provider themselves for custom behavior.
(struct proc-provider (name-proc caps-proc send-proc stream-proc)
  #:transparent
  #:methods gen:provider
  [(define (provider-name p)
     ((proc-provider-name-proc p)))
   (define (provider-capabilities p)
     ((proc-provider-caps-proc p)))
   (define (provider-send p req)
     ((proc-provider-send-proc p) req))
   (define (provider-stream p req)
     (stream-result->generator ((proc-provider-stream-proc p) req)))
   (define (provider-count-tokens p req)
     #f)])

;; ============================================================
;; Backward-compatible constructor
;; ============================================================

;; Creates a provider implementing gen:provider from four procedures.
(define (make-provider name-proc caps-proc send-proc stream-proc)
  (proc-provider name-proc caps-proc send-proc stream-proc))

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
                   (generator ()
                              (for ([ch (in-list chunks)])
                                (yield ch))
                              (yield #f)))))
