#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit
;; @not-test true ;; v1.00.13 W0 (#9454): committed red — promoted in W2 (#9466)

;; tests/test-provider-network-policy-conformance.rkt
;; v1.00.13 (RL-3/RL-10): cross-adapter policy conformance harness.
;;
;; One policy fixture (per-model `request` + legacy `sse-read`) must produce
;; the SAME mechanism arguments at the common stream/body boundary for all
;; four streaming adapters (openai-compatible, anthropic, azure-openai,
;; gemini) and the same body-read budget on all four non-streaming paths.
;;
;; The boundary is observed through `current-request-mechanism-observer`
;; (llm/stream.rkt, landed with the W2 wiring): a parameter procedure invoked
;; by `stream-sse-events` (kind=stream: initial/thinking/content/total) and by
;; `make-provider-http-request` (kind=body-read: read-timeout budget) with the
;; arguments each adapter actually hands to the shared mechanism.
;;
;; W0 red mode: the observer parameter does not exist yet (guarded
;; dynamic-require); the file compiles cleanly and fails its first check.
;; W2 (#9466) lands the observer + adapter wiring and removes the marker.

(require rackunit
         racket/tcp
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../llm/stream.rkt"
                  current-http-request-timeout
                  current-model-timeouts
                  current-model-sse-read-timeouts))

;; ————————————————————————————————————————————————————————————
;; Observer resolution (guarded: red until W2 lands)
;; ————————————————————————————————————————————————————————————

(define (stream-ref sym)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require '"../llm/stream.rkt" sym)))

(define observer (stream-ref 'current-request-mechanism-observer))

(define observer-landed? (procedure? observer))

(define (observer-missing-error)
  (fail "current-request-mechanism-observer (W2 #9466) not yet landed: conformance harness is red"))

;; ————————————————————————————————————————————————————————————
;; Local HTTP peer: answers any request with 200 + optional payload
;; ————————————————————————————————————————————————————————————

(define (make-local-peer #:response [response-bytes #f])
  (define listener (tcp-listen 0 16 #t "127.0.0.1"))
  (define-values (_h p _rh _rp) (tcp-addresses listener #t))
  (define url (format "http://127.0.0.1:~a" p))
  (define server
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (_) (void))])
                (for ([_ (in-range 32)])
                  (define-values (in out) (tcp-accept listener))
                  ;; drain request head + content-length body
                  (define content-length
                    (let loop ([len 0])
                      (define line (read-line in 'any))
                      (cond
                        [(or (eof-object? line) (string=? line "")) len]
                        [else
                         (define m (regexp-match #px"(?i:^Content-Length: *([0-9]+))" line))
                         (loop (if m (string->number (cadr m)) len))])))
                  (when (positive? content-length)
                    (read-bytes content-length in))
                  (display
                   (bytes-append #"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: close\r\n\r\n"
                                 (or response-bytes #"{\"ok\":true}"))
                   out)
                  (flush-output out)
                  (close-output-port out)
                  (close-input-port in))))))
  (lambda (op)
    (case op
      [(url) url]
      [(stop) (tcp-close listener)
              (unless (thread-dead? server) (kill-thread server))])))

;; ————————————————————————————————————————————————————————————
;; Adapter fixtures
;; ————————————————————————————————————————————————————————————

(define (adapter-makers url)
  (list (list "openai-compatible"
              (lambda () ((dynamic-require '"../llm/openai-compatible.rkt"
                                           'make-openai-compatible-provider)
                          (hash 'api-key "test-key" 'base-url url 'model "conformance-model"))))
        (list "anthropic"
              (lambda () ((dynamic-require '"../llm/anthropic.rkt" 'make-anthropic-provider)
                          (hash 'api-key "test-key" 'base-url url 'model "conformance-model"))))
        (list "azure-openai"
              (lambda () ((dynamic-require '"../llm/azure-openai.rkt" 'make-azure-openai-provider)
                          (hash 'api-key "test-key" 'base-url url 'model "conformance-model"
                                'api-version "2024-02-15-preview"))))
        (list "gemini"
              (lambda () ((dynamic-require '"../llm/gemini.rkt" 'make-gemini-provider)
                          (hash 'api-key "test-key" 'base-url url 'model "conformance-model"))))))

;; Run one adapter's stream path against the peer, recording what the shared
;; mechanism boundary received. Returns the list of observed hashes.
(define (observe-stream provider)
  (define recorded (box '()))
  (parameterize ([observer (lambda (info) (set-box! recorded (cons info (unbox recorded))))])
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (define gen (provider-stream provider
                                   (make-model-request '() '() (hash 'model "conformance-model"))))
      (gen)) ; pull once: the mechanism boundary fires before the first read
    (unbox recorded)))

;; Run one adapter's send (non-streaming) path, recording the body-read budget
;; the shared HTTP boundary received.
(define (observe-send provider)
  (define recorded (box '()))
  (parameterize ([observer (lambda (info) (set-box! recorded (cons info (unbox recorded))))])
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (provider-send provider
                     (make-model-request '() '() (hash 'model "conformance-model"))))
    (unbox recorded)))

(define (stream-entries recorded)
  (filter (lambda (h) (eq? (hash-ref h 'kind #f) 'stream)) recorded))

(define (body-read-entries recorded)
  (filter (lambda (h) (eq? (hash-ref h 'kind #f) 'body-read)) recorded))

;; ————————————————————————————————————————————————————————————
;; Conformance matrix
;; ————————————————————————————————————————————————————————————

;; F1: deepseek-style legacy config — request 900, sse-read 600.
;; Expected resolved mechanism args (frozen in test-request-network-policy.rkt):
;;   initial 120, thinking 300 (600 capped), content 60, total 1800
;; Expected body-read budget: 600 (legacy sse-read feeds body-read)
(define (parameterize/f1 thunk)
  (parameterize ([current-http-request-timeout 600]
                 [current-model-timeouts (hash "conformance-model" 900)]
                 [current-model-sse-read-timeouts (hash "conformance-model" 600)])
    (thunk)))

;; F2: small budget, no legacy override — request 90.
;;   initial 90, thinking 90, content 60, total 600; body-read 120 fallback
(define (parameterize/f2 thunk)
  (parameterize ([current-http-request-timeout 600]
                 [current-model-timeouts (hash "conformance-model" 90)]
                 [current-model-sse-read-timeouts (hash)])
    (thunk)))

(define (mechanism-args entry)
  (hash 'initial (hash-ref entry 'initial)
        'thinking (hash-ref entry 'thinking)
        'content (hash-ref entry 'content)
        'total (hash-ref entry 'total)))

(define (check-stream-conformance fixture-name parametrize expected-args expected-body)
  (unless observer-landed? (observer-missing-error))
  (define peer (make-local-peer))
  (define results
    (parametrize
     (lambda ()
       (for/list ([ (name make) (in-list (adapter-makers (peer 'url)))])
         (define provider (make))
         (define stream-obs (stream-entries (observe-stream provider)))
         (define body-obs (body-read-entries (observe-send provider)))
         (list name stream-obs body-obs)))))
  (define baseline #f)
  (for ([ (name stream-obs body-obs) (in-list results)])
    (check-true (pair? stream-obs)
                (format "~a ~a: the shared stream mechanism must observe one entry" fixture-name name))
    (when (pair? stream-obs)
      (define args (mechanism-args (car stream-obs)))
      (cond
        [(not baseline) (set! baseline args)]
        [else (check-equal? args baseline
                            (format "~a: ~a mechanism args must match the first adapter" fixture-name name))])
      (check-equal? args expected-args
                    (format "~a: ~a resolved policy values" fixture-name name)))
    (check-true (pair? body-obs)
                (format "~a ~a: the shared body-read mechanism must observe one entry" fixture-name name))
    (when (pair? body-obs)
      (check-equal? (hash-ref (car body-obs) 'read-timeout #f) expected-body
                    (format "~a: ~a body-read budget" fixture-name name))))
  (peer 'stop))

(test-case "F1 conformance: legacy request=900 sse-read=600 across all adapters"
  (check-stream-conformance
   "F1" parameterize/f1
   (hash 'initial 120 'thinking 300 'content 60 'total 1800)
   600))

(test-case "F2 conformance: request=90, no legacy override across all adapters"
  (check-stream-conformance
   "F2" parameterize/f2
   (hash 'initial 90 'thinking 90 'content 60 'total 600)
   120))

(test-case "legacy sse-read=600 cannot widen initial/content for ANY adapter"
  (unless observer-landed? (observer-missing-error))
  (define peer (make-local-peer))
  (define results
    (parameterize/f1
     (lambda ()
       (for/list ([ (name make) (in-list (adapter-makers (peer 'url)))])
         (define stream-obs (stream-entries (observe-stream (make))))
         (list name (and (pair? stream-obs) (mechanism-args (car stream-obs))))))))
  (for ([ (name args) (in-list results)])
    (when args
      (check-equal? (hash-ref args 'initial) 120 (format "~a initial bound" name))
      (check-equal? (hash-ref args 'content) 60 (format "~a content bound" name))))
  (peer 'stop))
