#lang racket

;; @speed fast  ;; @suite agent

;; tests/test-routing-policy.rkt — W3 (v0.99.9) Risk-Based Routing Policy Tests
;;
;; Tests the routing policy in tool-gateway.rkt:
;; - Default 'local-only policy routes everything local
;; - 'risk-based policy routes high/critical to remote, rest local
;; - Parameter is thread-local and configurable

(require rackunit
         rackunit/text-ui
         "../agent/roles/tool-gateway.rkt"
         "../util/message/mas-envelope.rkt")

;; Helper: create a test envelope with given risk level
(define (make-test-envelope risk-level)
  (make-mas-envelope 'supervisor
                     'tool-gateway
                     'shell-exec
                     (hasheq 'action "test")
                     #:risk-level risk-level))

(define suite
  (test-suite "Risk-Based Routing Policy (v0.99.9 W3)"

    ;; ── Default policy ──

    (test-case "current-routing-policy defaults to local-only"
      (check-equal? (current-routing-policy) 'local-only))

    ;; ── local-only policy ──

    (test-case "local-only routes low risk to local"
      (parameterize ([current-routing-policy 'local-only])
        (define env (make-test-envelope 'low))
        (check-equal? (route-execution-request env) 'local)))

    (test-case "local-only routes high risk to local"
      (parameterize ([current-routing-policy 'local-only])
        (define env (make-test-envelope 'high))
        (check-equal? (route-execution-request env) 'local)))

    (test-case "local-only routes critical risk to local"
      (parameterize ([current-routing-policy 'local-only])
        (define env (make-test-envelope 'critical))
        (check-equal? (route-execution-request env) 'local)))

    ;; ── risk-based policy ──

    (test-case "risk-based routes low risk to local"
      (parameterize ([current-routing-policy 'risk-based])
        (define env (make-test-envelope 'low))
        (check-equal? (route-execution-request env) 'local)))

    (test-case "risk-based routes medium risk to local"
      (parameterize ([current-routing-policy 'risk-based])
        (define env (make-test-envelope 'medium))
        (check-equal? (route-execution-request env) 'local)))

    (test-case "risk-based routes high risk to remote"
      (parameterize ([current-routing-policy 'risk-based])
        (define env (make-test-envelope 'high))
        (check-equal? (route-execution-request env) 'remote)))

    (test-case "risk-based routes critical risk to remote"
      (parameterize ([current-routing-policy 'risk-based])
        (define env (make-test-envelope 'critical))
        (check-equal? (route-execution-request env) 'remote)))

    ;; ── Edge cases ──

    (test-case "unknown policy defaults to local"
      (parameterize ([current-routing-policy 'some-unknown-policy])
        (define env (make-test-envelope 'high))
        (check-equal? (route-execution-request env) 'local)))

    ;; ── Parameter is thread-local ──

    (test-case "parameterize does not change default outside scope"
      (define env (make-test-envelope 'high))
      (parameterize ([current-routing-policy 'risk-based])
        (check-equal? (route-execution-request env) 'remote))
      ;; Outside parameterize, back to default
      (check-equal? (route-execution-request env) 'local))

    ;; ── Different risk levels produce expected routing ──

    (test-case "risk-based: all four risk levels produce correct routing"
      (parameterize ([current-routing-policy 'risk-based])
        (for ([risk (in-list '(low medium high critical))]
              [expected (in-list '(local local remote remote))])
          (define env (make-test-envelope risk))
          (check-equal? (route-execution-request env)
                        expected
                        (format "risk ~a should route to ~a" risk expected)))))))

(run-tests suite)
