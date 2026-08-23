#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit

;; tests/test-request-network-policy.rkt
;; Unification (RL-1/RL-2/RL-9): the resolved request-network policy contract.
;;
;; Freezes the semantics of `llm/request-policy.rkt`:
;;
;;   request-budget : preserves the per-model `request` meaning
;;   connect/ttfb   : (min request-budget 120) — dedicated bound, never the
;;                    broad request budget (RL-4, enforced in W4)
;;   initial idle   : (min request-budget 120) — dead-peer detection; never
;;                    widened by model config
;;   thinking idle  : (min request-budget (min (or thinking-override 120) 300))
;;                    where thinking-override = explicit thinking-idle
;;                    > legacy sse-read > 120 default
;;   content idle   : 60 — never widened by model config
;;   stream total   : (max 600 (* 2 request-budget)) — total wall-clock budget
;;                    (NOT an inactivity detector); frozen from the
;;                    characterization of the intentional openai-compatible
;;                    formula (v0.45.12 L1). See W0 ownership map.
;;   body-read      : explicit body-read > legacy sse-read > fallback
;;
;; Legacy `sse-read` must NEVER influence connect/ttfb, initial, or content.
;; Committed red in W0 (#9454); green since W1 (#9461).

(require rackunit
         (only-in "../llm/stream.rkt"
                  http-stream-timeout-default
                  http-read-timeout-default
                  max-thinking-gap-secs)
         (only-in "../llm/request-policy.rkt"
                  current-http-request-timeout
                  current-model-timeouts
                  current-model-sse-read-timeouts
                  current-model-thinking-idle-timeouts
                  current-model-body-read-timeouts
                  resolve-request-network-policy-for-model))

;; ————————————————————————————————————————————————————————————
;; W1 module resolution (guarded: red until W1 lands)
;; ————————————————————————————————————————————————————————————

(define (policy-ref sym)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require '"../llm/request-policy.rkt" sym)))

(define request-network-policy? (policy-ref 'request-network-policy?))
(define resolve-request-network-policy (policy-ref 'resolve-request-network-policy))
;; field accessors (contract per plan §3.1)
(define p-request-budget (policy-ref 'request-network-policy-request-budget-secs))
(define p-connect-ttfb (policy-ref 'request-network-policy-connect-ttfb-secs))
(define p-initial-idle (policy-ref 'request-network-policy-initial-idle-secs))
(define p-thinking-idle (policy-ref 'request-network-policy-thinking-idle-secs))
(define p-content-idle (policy-ref 'request-network-policy-content-idle-secs))
(define p-stream-total (policy-ref 'request-network-policy-stream-total-secs))
(define p-body-read (policy-ref 'request-network-policy-body-read-budget-secs))

(define resolver-landed?
  (and (procedure? request-network-policy?)
       (procedure? resolve-request-network-policy)
       (procedure? p-request-budget)
       (procedure? p-stream-total)))

(define (policy-landed-error)
  (fail "llm/request-policy.rkt (W1 #9461) not yet landed: resolver contract is red"))

;; convenience resolver wrapper with the frozen default fallback
(define (resolve #:request-timeout rt
                 #:sse-read-override [sse #f]
                 #:thinking-idle-override [think #f]
                 #:body-read-override [body #f]
                 #:body-read-fallback [fallback http-read-timeout-default])
  (resolve-request-network-policy #:request-timeout rt
                                  #:sse-read-override sse
                                  #:thinking-idle-override think
                                  #:body-read-override body
                                  #:body-read-fallback fallback))

(test-case "W1 contract: resolver module exists and produces the policy struct"
  (unless resolver-landed? (policy-landed-error))
  (define p (resolve #:request-timeout 600))
  (check-pred request-network-policy? p))

(test-case "request budget preserves the per-model request meaning"
  (unless resolver-landed? (policy-landed-error))
  (for ([rt (in-list '(1 60 120 300 600 900 1800))])
    (define p (resolve #:request-timeout rt))
    (check-equal? (p-request-budget p) rt)))

(test-case "connect/ttfb is a dedicated bounded value (min request 120)"
  (unless resolver-landed? (policy-landed-error))
  (check-equal? (p-connect-ttfb (resolve #:request-timeout 900)) 120)
  (check-equal? (p-connect-ttfb (resolve #:request-timeout 90)) 90)
  ;; an established-but-silent peer can never consume the 900 s budget
  (check-true (< (p-connect-ttfb (resolve #:request-timeout 900)) 900)))

(test-case "initial idle is (min request 120) and never widened by config"
  (unless resolver-landed? (policy-landed-error))
  (check-equal? (p-initial-idle (resolve #:request-timeout 900)) 120)
  (check-equal? (p-initial-idle (resolve #:request-timeout 45)) 45)
  ;; legacy sse-read of 600 must not widen the initial dead-peer window
  (check-equal? (p-initial-idle (resolve #:request-timeout 900
                                         #:sse-read-override 600))
                120)
  ;; explicit thinking-idle must not widen it either
  (check-equal? (p-initial-idle (resolve #:request-timeout 900
                                         #:thinking-idle-override 600))
                120))

(test-case "thinking idle honors explicit > legacy > default, capped at 300"
  (unless resolver-landed? (policy-landed-error))
  ;; default 120
  (check-equal? (p-thinking-idle (resolve #:request-timeout 900)) 120)
  ;; legacy sse-read feeds only the thinking window, capped at 300
  (check-equal? (p-thinking-idle (resolve #:request-timeout 900
                                          #:sse-read-override 600))
                max-thinking-gap-secs)
  (check-equal? (p-thinking-idle (resolve #:request-timeout 900
                                          #:sse-read-override 200))
                200)
  ;; explicit thinking-idle wins over legacy sse-read
  (check-equal? (p-thinking-idle (resolve #:request-timeout 900
                                          #:sse-read-override 600
                                          #:thinking-idle-override 240))
                240)
  ;; explicit is still capped at 300
  (check-equal? (p-thinking-idle (resolve #:request-timeout 900
                                          #:thinking-idle-override 400))
                max-thinking-gap-secs)
  ;; request budget clamps the phase value
  (check-equal? (p-thinking-idle (resolve #:request-timeout 90
                                          #:thinking-idle-override 240))
                90))

(test-case "content idle is fixed at 60 and never widened by config"
  (unless resolver-landed? (policy-landed-error))
  (check-equal? (p-content-idle (resolve #:request-timeout 900)) 60)
  (check-equal? (p-content-idle (resolve #:request-timeout 900
                                         #:sse-read-override 600))
                60)
  (check-equal? (p-content-idle (resolve #:request-timeout 900
                                         #:thinking-idle-override 300))
                60))

(test-case "stream total is the frozen (max 600 (* 2 request)) total budget"
  (unless resolver-landed? (policy-landed-error))
  (check-equal? (p-stream-total (resolve #:request-timeout 900)) 1800)
  (check-equal? (p-stream-total (resolve #:request-timeout 300)) 600)
  (check-equal? (p-stream-total (resolve #:request-timeout 120)) 600)
  ;; total is a budget, not a liveness detector: config cannot shrink the
  ;; per-phase windows through it
  (check-equal? (p-stream-total (resolve #:request-timeout 900
                                         #:sse-read-override 30))
                1800))

(test-case "body-read precedence: explicit > legacy sse-read > fallback"
  (unless resolver-landed? (policy-landed-error))
  (check-equal? (p-body-read (resolve #:request-timeout 600)) http-read-timeout-default)
  (check-equal? (p-body-read (resolve #:request-timeout 600 #:sse-read-override 300))
                300)
  (check-equal? (p-body-read (resolve #:request-timeout 600
                                      #:sse-read-override 300
                                      #:body-read-override 45))
                45)
  (check-equal? (p-body-read (resolve #:request-timeout 600 #:body-read-override 45))
                45))

(test-case "legacy sse-read never influences connect, initial, or content"
  (unless resolver-landed? (policy-landed-error))
  (define wo (resolve #:request-timeout 900))
  (define wi (resolve #:request-timeout 900 #:sse-read-override 600))
  (check-equal? (p-connect-ttfb wi) (p-connect-ttfb wo))
  (check-equal? (p-initial-idle wi) (p-initial-idle wo))
  (check-equal? (p-content-idle wi) (p-content-idle wo))
  (check-not-equal? (p-thinking-idle wi) (p-thinking-idle wo)))

(test-case "resolver validates: zero/negative durations fail configuration early"
  (unless resolver-landed? (policy-landed-error))
  (check-exn exn:fail? (lambda () (resolve #:request-timeout 0)))
  (check-exn exn:fail? (lambda () (resolve #:request-timeout -5)))
  (check-exn exn:fail? (lambda () (resolve #:request-timeout 600
                                           #:sse-read-override 0)))
  (check-exn exn:fail? (lambda () (resolve #:request-timeout 600
                                           #:thinking-idle-override -1)))
  (check-exn exn:fail? (lambda () (resolve #:request-timeout 600
                                           #:body-read-override 0)))
  (check-exn exn:fail? (lambda () (resolve #:request-timeout 600
                                           #:body-read-fallback 0))))

(test-case "resolve-request-network-policy-for-model reads the wired parameters"
  (unless resolver-landed? (policy-landed-error))
  ;; deepseek-style legacy config through the parameter path
  (parameterize ([current-http-request-timeout 600]
                 [current-model-timeouts (hash "deepseek" 900)]
                 [current-model-sse-read-timeouts (hash "deepseek" 600)]
                 [current-model-thinking-idle-timeouts (hash)]
                 [current-model-body-read-timeouts (hash)])
    (define p (resolve-request-network-policy-for-model "deepseek"))
    (check-equal? (p-request-budget p) 900)
    (check-equal? (p-initial-idle p) 120)
    (check-equal? (p-thinking-idle p) 300)
    (check-equal? (p-content-idle p) 60)
    (check-equal? (p-stream-total p) 1800)
    (check-equal? (p-body-read p) 600))
  ;; explicit semantic keys win over the legacy alias
  (parameterize ([current-http-request-timeout 600]
                 [current-model-timeouts (hash "m" 600)]
                 [current-model-sse-read-timeouts (hash "m" 600)]
                 [current-model-thinking-idle-timeouts (hash "m" 240)]
                 [current-model-body-read-timeouts (hash "m" 45)])
    (define p (resolve-request-network-policy-for-model "m"))
    (check-equal? (p-thinking-idle p) 240)
    (check-equal? (p-body-read p) 45))
  ;; unknown model falls back to the global request budget
  (parameterize ([current-http-request-timeout 300]
                 [current-model-timeouts (hash)]
                 [current-model-sse-read-timeouts (hash)]
                 [current-model-thinking-idle-timeouts (hash)]
                 [current-model-body-read-timeouts (hash)])
    (define p (resolve-request-network-policy-for-model #f))
    (check-equal? (p-request-budget p) 300)
    (check-equal? (p-stream-total p) 600)))

(test-case "property sweep: invariants hold across the config matrix"
  (unless resolver-landed? (policy-landed-error))
  (for* ([rt (in-list '(1 30 60 90 120 300 600 900 1800))]
         [sse (in-list (list #f 30 120 300 600))]
         [think (in-list (list #f 60 240 400))])
    (define p (resolve #:request-timeout rt
                       #:sse-read-override sse
                       #:thinking-idle-override think))
    (for ([ (label v) (in-hash (hash 'request-budget (p-request-budget p)
                                     'connect-ttfb (p-connect-ttfb p)
                                     'initial (p-initial-idle p)
                                     'thinking (p-thinking-idle p)
                                     'content (p-content-idle p)
                                     'stream-total (p-stream-total p)
                                     'body-read (p-body-read p)))])
      (check-true (and (real? v) (positive? v))
                  (format "~a must be positive for rt=~a sse=~a think=~a" label rt sse think)))
    (check-true (<= (p-initial-idle p) 120))
    (check-true (<= (p-thinking-idle p) max-thinking-gap-secs))
    (check-true (<= (p-thinking-idle p) rt))
    (check-equal? (p-content-idle p) http-stream-timeout-default)
    (check-true (<= (p-connect-ttfb p) 120))
    (check-equal? (p-stream-total p) (max 600 (* 2 rt))
                  "stream total is exactly (max 600 (* 2 request)) — see frozen formula")))
