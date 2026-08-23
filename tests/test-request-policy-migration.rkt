#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit

;; tests/test-request-policy-migration.rkt
;; Unification W5 (#9483): legacy-config migration proof (PLAN §6 W5 task 4).
;;
;; Proves that old configs containing only `request` + `sse-read` resolve to
;; the intended compatible values, that new explicit fields win over the
;; legacy alias, and that the safety caps hold for the known DeepSeek/Kimi
;; shapes.

(require rackunit
         (only-in "../llm/request-policy.rkt"
                  resolve-request-network-policy-for-model
                  request-network-policy-request-budget-secs
                  request-network-policy-connect-ttfb-secs
                  request-network-policy-initial-idle-secs
                  request-network-policy-thinking-idle-secs
                  request-network-policy-content-idle-secs
                  request-network-policy-stream-total-secs
                  request-network-policy-body-read-budget-secs
                  current-http-request-timeout
                  current-model-timeouts
                  current-model-sse-read-timeouts
                  current-model-thinking-idle-timeouts
                  current-model-body-read-timeouts))

(define (resolve/model model
                       #:request [req #f]
                       #:sse-read [sse #f]
                       #:thinking-idle [think #f]
                       #:body-read [body #f])
  (parameterize ([current-http-request-timeout 600]
                 [current-model-timeouts (if req (hash model req) (hash))]
                 [current-model-sse-read-timeouts (if sse (hash model sse) (hash))]
                 [current-model-thinking-idle-timeouts (if think (hash model think) (hash))]
                 [current-model-body-read-timeouts (if body (hash model body) (hash))])
    (resolve-request-network-policy-for-model model)))

(test-case "DeepSeek legacy shape: request=900, sse-read=600"
  (define p (resolve/model "deepseek-v4-flash" #:request 900 #:sse-read 600))
  (check-equal? (request-network-policy-request-budget-secs p) 900)
  ;; legacy sse-read preserves the slow-body allowance...
  (check-equal? (request-network-policy-body-read-budget-secs p) 600)
  ;; ...and the slow-reasoning allowance, capped at 300
  (check-equal? (request-network-policy-thinking-idle-secs p) 300)
  ;; ...but CANNOT widen the safety detectors
  (check-equal? (request-network-policy-initial-idle-secs p) 120)
  (check-equal? (request-network-policy-content-idle-secs p) 60)
  (check-equal? (request-network-policy-connect-ttfb-secs p) 120)
  (check-equal? (request-network-policy-stream-total-secs p) 1800))

(test-case "Kimi legacy shape: sse-read=300 keeps the intended allowances"
  (define p (resolve/model "kimi-k2" #:request 600 #:sse-read 300))
  (check-equal? (request-network-policy-thinking-idle-secs p) 300)
  (check-equal? (request-network-policy-body-read-budget-secs p) 300)
  (check-equal? (request-network-policy-initial-idle-secs p) 120)
  (check-equal? (request-network-policy-content-idle-secs p) 60))

(test-case "plain legacy shape: only request (no sse-read)"
  (define p (resolve/model "glm-5.1" #:request 900))
  (check-equal? (request-network-policy-thinking-idle-secs p) 120)
  (check-equal? (request-network-policy-body-read-budget-secs p) 120)
  (check-equal? (request-network-policy-stream-total-secs p) 1800))

(test-case "explicit semantic keys win over the legacy alias"
  (define p (resolve/model "m" #:request 600 #:sse-read 600
                           #:thinking-idle 240 #:body-read 45))
  (check-equal? (request-network-policy-thinking-idle-secs p) 240)
  (check-equal? (request-network-policy-body-read-budget-secs p) 45)
  ;; explicit keys still cannot widen the safety detectors
  (check-equal? (request-network-policy-initial-idle-secs p) 120)
  (check-equal? (request-network-policy-content-idle-secs p) 60))

(test-case "explicit thinking-idle over the 300 cap is still capped"
  (define p (resolve/model "m" #:request 600 #:thinking-idle 400))
  (check-equal? (request-network-policy-thinking-idle-secs p) 300))

(test-case "request budget clamps phase values"
  (define p (resolve/model "fast" #:request 90 #:sse-read 300))
  (check-equal? (request-network-policy-initial-idle-secs p) 90)
  (check-equal? (request-network-policy-thinking-idle-secs p) 90)
  (check-equal? (request-network-policy-stream-total-secs p) 600)
  (check-equal? (request-network-policy-body-read-budget-secs p) 300))
