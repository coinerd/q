#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit
;; @not-test true ;; v1.00.13 W0 (#9454): committed red — promoted in W3/W4 (#9473/#9478)

;; tests/test-network-failure-context.rkt
;; v1.00.13 (RL-5/RL-7/RL-8): structured failure context contract.
;;
;; Freezes the W3/W4 contracts:
;;   - llm/http-helpers.rkt: build-network-failure-context (status+headers →
;;     structured context hash incl. parsed retry-after; retry-relevant
;;     headers only — authorization/cookie values never retained)
;;   - llm/http-helpers.rkt: parse-retry-after-header (delta-seconds AND
;;     HTTP-date, deterministic clock seam via #:now-ms)
;;   - llm/provider-errors.rkt: provider-error-context (structured context
;;     accessor on provider errors)
;;   - runtime/auto-retry.rkt: structured-retry-after-ms (retry delay source
;;     reads structured fields, never the human message) and heartbeat-aware
;;     held-request? classification
;;
;; W0 red mode: the W3 symbols do not exist (guarded dynamic-require); the
;; heartbeat assertions are assertion-red against today's classifier.

(require rackunit
         (only-in "../llm/provider-errors.rkt" provider-error provider-error?)
         (only-in "../util/error/errors.rkt" q-llm-error-category))

;; ————————————————————————————————————————————————————————————
;; W3/W4 symbol resolution (guarded)
;; ————————————————————————————————————————————————————————————

(define (helpers-ref sym)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require '"../llm/http-helpers.rkt" sym)))

(define (retry-ref sym)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require '"../runtime/auto-retry.rkt" sym)))

(define (errors-ref sym)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require '"../llm/provider-errors.rkt" sym)))

(define build-network-failure-context (helpers-ref 'build-network-failure-context))
(define parse-retry-after-header (helpers-ref 'parse-retry-after-header))
(define provider-error-context (errors-ref 'provider-error-context))
(define structured-retry-after-ms (retry-ref 'structured-retry-after-ms))
(define held-request? (retry-ref 'held-request?))

(define context-landed? (and (procedure? build-network-failure-context)
                             (procedure? parse-retry-after-header)
                             (procedure? provider-error-context)))

(define (context-missing-error)
  (fail "structured failure context (W3 #9473) not yet landed: contract is red"))

;; ————————————————————————————————————————————————————————————
;; build-network-failure-context: 429 + Retry-After fixture
;; ————————————————————————————————————————————————————————————

(test-case "HTTP 429 with Retry-After: 17 produces structured retry metadata"
  (unless context-landed? (context-missing-error))
  (define ctx (build-network-failure-context
               #:kind 'http-status
               #:status 429
               #:headers '("HTTP/1.1 429 Too Many Requests"
                           "Content-Type: application/json"
                           "Retry-After: 17"
                           "x-ratelimit-remaining: 3")))
  (check-equal? (hash-ref ctx 'http-status #f) 429)
  (check-equal? (hash-ref ctx 'retry-after-ms #f) 17000)
  (check-true (member "Retry-After: 17" (hash-ref ctx 'response-headers '()))
              "the raw Retry-After header must survive into the context"))

(test-case "response headers are redacted to retry-relevant values only"
  (unless context-landed? (context-missing-error))
  (define ctx (build-network-failure-context
               #:kind 'http-status
               #:status 429
               #:headers '("Retry-After: 17"
                           "Authorization: Bearer sk-secret-value"
                           "Set-Cookie: session=secret")))
  (define retained (hash-ref ctx 'response-headers '()))
  (for ([header (in-list retained)])
    (check-false (regexp-match? #rx"(?i:authorization|cookie)" header)
                 (format "sensitive header leaked into failure context: ~a" header))))

(test-case "parse-retry-after-header: delta-seconds and floats"
  (unless context-landed? (context-missing-error))
  (check-equal? (parse-retry-after-header "17") 17000)
  (check-equal? (parse-retry-after-header "2.5") 2500)
  (check-equal? (parse-retry-after-header " 30 ") 30000)
  (check-equal? (parse-retry-after-header "garbage") #f)
  (check-equal? (parse-retry-after-header "") #f))

(test-case "parse-retry-after-header: HTTP-date with deterministic clock seam"
  (unless context-landed? (context-missing-error))
  ;; Fixed clock: Wed, 21 Oct 2015 07:28:00 GMT = 1445412480
  (define fixed-ms (* 1445412480 1000))
  (define (now-ms) fixed-ms)
  ;; date exactly now → 0 delay
  (check-equal? (parse-retry-after-header "Wed, 21 Oct 2015 07:28:00 GMT" #:now-ms now-ms)
                0)
  ;; date 30 s in the future → 30000 ms
  (check-equal? (parse-retry-after-header "Wed, 21 Oct 2015 07:28:30 GMT" #:now-ms now-ms)
                30000)
  ;; date in the past → clamped to 0, not negative
  (check-equal? (parse-retry-after-header "Wed, 21 Oct 2015 07:27:00 GMT" #:now-ms now-ms)
                0))

;; ————————————————————————————————————————————————————————————
;; provider-error structured context + retry consumption (RL-7)
;; ————————————————————————————————————————————————————————————

(define (context-error msg ctx)
  (provider-error msg (current-continuation-marks) ctx 'rate-limit 429))

(test-case "provider-error-context exposes structured fields"
  (unless context-landed? (context-missing-error))
  (define e (context-error "rate limited"
                           (hash 'retry-after-ms 17000 'http-status 429)))
  (check-equal? (hash-ref (provider-error-context e) 'retry-after-ms) 17000)
  (check-equal? (hash-ref (provider-error-context e) 'http-status) 429))

(test-case "structured-retry-after-ms reads structured fields, never the message"
  (unless (and context-landed? (procedure? structured-retry-after-ms))
    (context-missing-error))
  ;; structured context wins
  (define e (context-error "rate limited (no header text)"
                           (hash 'retry-after-ms 17000 'http-status 429)))
  (check-equal? (structured-retry-after-ms e) 17000)
  ;; NO structured context: even though the human message mentions a header
  ;; value, retry must NOT parse it out of the text (RL-7: no string protocol)
  (define e2 (context-error "API rate limited. Retry-After: 99" (hash)))
  (check-equal? (structured-retry-after-ms e2) #f)
  ;; no context at all → #f
  (check-equal? (structured-retry-after-ms
                 (provider-error "boom" (current-continuation-marks) (hash) 'network #f))
                #f))

;; ————————————————————————————————————————————————————————————
;; Heartbeat-aware held-request classification (RL-8, W4)
;; ————————————————————————————————————————————————————————————

(define (stream-timeout heartbeats? data? phase chars)
  ((dynamic-require '"../llm/stream.rkt" 'exn:fail:network:timeout:stream)
   "HTTP read timeout"
   (current-continuation-marks)
   heartbeats? data? phase chars))

(test-case "heartbeat-only initial stall is live-but-no-content, not a held request"
  (unless (procedure? held-request?)
    (fail "held-request? unresolvable from runtime/auto-retry.rkt"))
  ;; zero data + zero heartbeat + initial timeout → held/dead peer (unchanged)
  (check-true (held-request? (stream-timeout #f #f 'initial 0)))
  ;; heartbeat-only: the peer proved liveness via SSE comments; it must NOT
  ;; be classified identically to a zero-liveness dead peer
  (check-false (held-request? (stream-timeout #t #f 'initial 0))
               "RL-8: heartbeat-only stream must not be classified as held request"))

(test-case "mid-stream stalls remain retryable regardless of heartbeat state"
  (unless (procedure? held-request?)
    (fail "held-request? unresolvable from runtime/auto-retry.rkt"))
  (check-false (held-request? (stream-timeout #f #t 'thinking 40)))
  (check-false (held-request? (stream-timeout #t #t 'content 900))))
