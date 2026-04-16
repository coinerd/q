#lang racket/base

;; tests/helpers/mock-provider.rkt — Shared mock provider factories
;;
;; Canonical mock providers for unit and integration tests.
;; Replaces duplicated definitions across test files.

(require json
         racket/list
         racket/string
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         (only-in "../../tools/tool.rkt" make-tool-registry))

(provide make-multi-mock-provider
         make-simple-mock-provider
         make-tool-call-mock-provider
         make-test-config)

;; Multi-response mock provider: returns different responses for
;; successive turns.  stream is called first (no advance), then
;; send (advances index).
(define (make-multi-mock-provider responses)
  (define idx (box 0))
  (define (current-response)
    (if (< (unbox idx) (length responses))
        (list-ref responses (unbox idx))
        (last responses)))
  (make-provider
   (lambda () "multi-mock")
   (lambda () (hash 'streaming #t 'token-counting #t))
   ;; send: return current response and advance
   (lambda (req)
     (define resp (current-response))
     (set-box! idx (add1 (unbox idx)))
     resp)
   ;; stream: generate chunks from current response and advance
   (lambda (req)
     (define resp (current-response))
     (set-box! idx (add1 (unbox idx)))
     (define content-parts (model-response-content resp))
     (append
      (for/list ([part (in-list content-parts)])
        (cond
          [(equal? (hash-ref part 'type #f) "text")
           (make-stream-chunk (hash-ref part 'text "") #f #f #f)]
          [(equal? (hash-ref part 'type #f) "tool-call")
           (make-stream-chunk #f
                          (hasheq 'index 0
                                  'id (hash-ref part 'id "")
                                  'function (hasheq 'name (hash-ref part 'name "")
                                                    'arguments (let ([args (hash-ref part 'arguments (hash))])
                                                                 (if (string? args) args
                                                                     (format "{~a}" (string-join
                                                                                      (for/list ([(k v) (in-hash args)])
                                                                                        (format "\"~a\":\"~a\"" k v))
                                                                                      ","))))))
                          #f #f)]
          [else (make-stream-chunk #f #f #f #f)]))
      (list (make-stream-chunk #f #f (model-response-usage resp) #t))))))

;; Simple text-only mock provider: returns text strings in sequence.
(define (make-simple-mock-provider . texts)
  (define idx (box 0))
  (make-provider
   (lambda () "mock-simple")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (define i (unbox idx))
     (set-box! idx (add1 i))
     (define text (if (< i (length texts)) (list-ref texts i) "done"))
     (make-model-response
      (list (hash 'type "text" 'text text))
      (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
      "mock-model"
      'stop))
   (lambda (req)
     (define i (unbox idx))
     (set-box! idx (add1 i))
     (define text (if (< i (length texts)) (list-ref texts i) "done"))
     (list (make-stream-chunk text #f #f #f)
           (make-stream-chunk #f #f
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         #t)))))

;; Mock provider: first call returns a tool-call via stream, second returns text.
(define (make-tool-call-mock-provider tool-name tool-args response-text)
  (define call-count (box 0))
  (make-provider
   (lambda () "mock-tool-call")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (set-box! call-count (add1 (unbox call-count)))
     (cond
       [(= (unbox call-count) 1)
        (make-model-response
         (list (hash 'type "tool-call"
                     'id "tc-mock-1"
                     'name tool-name
                     'arguments tool-args))
         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
         "mock-model"
         'tool-calls)]
       [else
        (make-model-response
         (list (hash 'type "text" 'text response-text))
         (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
         "mock-model"
         'stop)]))
   (lambda (req)
     (set-box! call-count (add1 (unbox call-count)))
     (cond
       [(<= (unbox call-count) 1)
        (list (make-stream-chunk #f
                            (hasheq 'id "tc-mock-1"
                                    'name tool-name
                                    'arguments (if (hash? tool-args)
                                                   (jsexpr->string tool-args)
                                                   tool-args))
                            #f #f)
              (make-stream-chunk #f #f
                            (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                            #t))]
       [else
        (list (make-stream-chunk response-text #f #f #f)
              (make-stream-chunk #f #f
                            (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
                            #t))]))))

;; Create a standard test config hash for agent-session.
(define (make-test-config dir bus prov [reg #f])
  (hash 'provider prov
        'tool-registry (or reg (make-tool-registry))
        'event-bus bus
        'session-dir dir))
