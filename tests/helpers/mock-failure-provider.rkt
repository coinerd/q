#lang racket/base

;; tests/helpers/mock-failure-provider.rkt — Mock provider with failure injection
;;
;; Wraps mock-provider.rkt patterns with configurable failure modes
;; for testing error recovery paths in the agent loop and TUI.
;;
;; Failure modes:
;;   'timeout          — raises exn:fail with timeout message
;;   'rate-limit       — raises exn:fail with 429 message
;;   'auth             — raises exn:fail with 401 message
;;   'context-overflow — raises exn:fail with context length message
;;   'partial-response — sends N chunks then completes (simulates partial)
;;   'max-iterations   — raises exn:fail with max.iterations message
;;   'none             — succeeds normally

(require racket/generator
         "../../llm/model.rkt"
         "../../llm/provider.rkt")

(provide make-failure-provider)

;; ============================================================
;; Failure message templates
;; ============================================================

(define FAILURE-MESSAGES
  (hasheq 'timeout
          "HTTP read timeout after 120s"
          'rate-limit
          "HTTP 429 rate limit exceeded"
          'auth
          "401 Unauthorized: invalid API key"
          'context-overflow
          "context_length exceeded: too many tokens"
          'partial-response
          "HTTP read timeout after partial response"
          'max-iterations
          "max.iterations reached: 25"))

;; ============================================================
;; Constructor
;; ============================================================

;; make-failure-provider : #:failure-mode symbol? #:fail-times nat?
;;                         #:timeout-secs nat? #:partial-chunks nat?
;;                         #:response-text string?
;;                         -> (values provider? (-> (listof (cons symbol? any/c))))
;;
;; Returns the provider AND a stats accessor procedure.
;; The stats procedure returns: ((mode . <mode>) (fail-times . N)
;;   (fail-count . N) (success-count . N))
;;
;; After fail-times failures, the provider succeeds with a normal response.
;; #:fail-times 0 means always succeed.

(define (make-failure-provider #:failure-mode [mode 'timeout]
                               #:fail-times [fail-times 1]
                               #:timeout-secs [timeout-secs 120]
                               #:partial-chunks [partial-chunks 2]
                               #:response-text [response-text "Success after retry"])
  ;; Mutable state captured in closure
  (define fail-count (box 0))
  (define success-count (box 0))

  (define (should-fail?)
    (< (unbox fail-count) fail-times))

  (define (do-fail)
    (define msg (hash-ref FAILURE-MESSAGES mode "unknown error"))
    (set-box! fail-count (add1 (unbox fail-count)))
    (raise (exn:fail msg (current-continuation-marks))))

  (define (do-success)
    (set-box! success-count (add1 (unbox success-count)))
    (make-model-response (list (hash 'type "text" 'text response-text))
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         "mock-failure-model"
                         'stop))

  (define (send-proc req)
    (if (should-fail?)
        (do-fail)
        (do-success)))

  (define (stream-proc req)
    (cond
      [(not (should-fail?))
       (set-box! success-count (add1 (unbox success-count)))
       (list (make-stream-chunk response-text #f #f #f)
             (make-stream-chunk #f
                                #f
                                (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                                #t))]
      [(eq? mode 'partial-response)
       (set-box! fail-count (add1 (unbox fail-count)))
       (define partial-text
         (apply string-append
                (for/list ([i (in-range partial-chunks)])
                  (format "Chunk~a " (add1 i)))))
       (list (make-stream-chunk partial-text #f #f #f)
             (make-stream-chunk #f
                                #f
                                (hasheq 'prompt-tokens 10 'completion-tokens 3 'total-tokens 13)
                                #t))]
      [else (do-fail)]))

  (define prov
    (make-provider (lambda () "mock-failure")
                   (lambda () (hasheq 'streaming #t 'token-counting #t))
                   send-proc
                   stream-proc))

  ;; Stats accessor
  (define (failure-stats)
    (list (cons 'mode mode)
          (cons 'fail-times fail-times)
          (cons 'fail-count (unbox fail-count))
          (cons 'success-count (unbox success-count))))

  (values prov failure-stats))
