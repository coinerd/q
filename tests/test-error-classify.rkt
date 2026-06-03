#lang racket/base

;; tests/test-error-classify.rkt — Failure-domain classifier unit tests
;;
;; Verifies the 7-domain error classifier classifies exceptions correctly.

(require rackunit
         rackunit/text-ui
         racket/list
         "../util/error/error-classify.rkt")

(define (make-exn msg)
  (exn:fail msg (current-continuation-marks)))

(define classify-tests
  (test-suite "error-classify"

    ;; ── Domain registry tests ──────────────────────────────

    (test-case "error-domains returns all 7 domains"
      (define domains (error-domains))
      (check-equal? (length domains) 7 "Must have 7 domains")
      (for ([d '(contract io network provider session tool tui)])
        (check-not-false (member d domains) (format "Must include ~a" d))))

    (test-case "error-domain? recognizes valid domains"
      (for ([d '(contract io network provider session tool tui)])
        (check-true (error-domain? d) (format "~a must be a valid domain" d))))

    (test-case "error-domain? rejects invalid domains"
      (check-false (error-domain? 'unknown))
      (check-false (error-domain? "contract"))
      (check-false (error-domain? 42)))

    ;; ── Contract domain ────────────────────────────────────

    (test-case "contract violation classified as contract"
      (define result (classify-error (make-exn "contract violation: expected string?, given 42")))
      (check-not-false result "Must classify contract errors")
      (check-equal? (first result) 'contract))

    (test-case "blaming message classified as contract"
      (define result (classify-error (make-exn "blaming: q/util/contracts")))
      (check-not-false result)
      (check-equal? (first result) 'contract))

    ;; ── Network domain ─────────────────────────────────────

    (test-case "connection refused classified as network"
      (define result (classify-error (make-exn "connection refused to localhost:8080")))
      (check-not-false result)
      (check-equal? (first result) 'network))

    (test-case "SSL certificate classified as network"
      (define result (classify-error (make-exn "SSL certificate verification failed")))
      (check-not-false result)
      (check-equal? (first result) 'network))

    ;; ── Provider domain ────────────────────────────────────

    (test-case "401 unauthorized classified as provider"
      (define result (classify-error (make-exn "HTTP 401 unauthorized")))
      (check-not-false result)
      (check-equal? (first result) 'provider))

    (test-case "429 rate limit classified as provider"
      (define result (classify-error (make-exn "HTTP 429 rate limit exceeded")))
      (check-not-false result)
      (check-equal? (first result) 'provider))

    (test-case "API key error classified as provider"
      (define result (classify-error (make-exn "Invalid API key provided")))
      (check-not-false result)
      (check-equal? (first result) 'provider))

    (test-case "502 bad gateway classified as provider"
      (define result (classify-error (make-exn "HTTP 502 bad gateway")))
      (check-not-false result)
      (check-equal? (first result) 'provider))

    ;; ── I/O domain ─────────────────────────────────────────

    (test-case "file not found classified as io"
      (define result (classify-error (make-exn "file not found: /tmp/test.txt")))
      (check-not-false result)
      (check-equal? (first result) 'io))

    (test-case "permission denied classified as io"
      (define result (classify-error (make-exn "permission denied: /root/.config")))
      (check-not-false result)
      (check-equal? (first result) 'io))

    (test-case "invalid JSON classified as io"
      (define result (classify-error (make-exn "read-json: expected string")))
      (check-not-false result)
      (check-equal? (first result) 'io))

    ;; ── Session domain ─────────────────────────────────────

    (test-case "hash-ref error classified as session"
      (define result (classify-error (make-exn "hash-ref: no value found for key")))
      (check-not-false result)
      (check-equal? (first result) 'session))

    (test-case "session error classified as session"
      (define result (classify-error (make-exn "session initialization failed")))
      (check-not-false result)
      (check-equal? (first result) 'session))

    ;; ── Tool domain ────────────────────────────────────────

    (test-case "tool timeout classified as tool"
      (define result (classify-error (make-exn "tool timeout: bash execution exceeded 30s")))
      (check-not-false result)
      (check-equal? (first result) 'tool))

    (test-case "subprocess error classified as tool"
      (define result (classify-error (make-exn "subprocess exited with non-zero status")))
      (check-not-false result)
      (check-equal? (first result) 'tool))

    (test-case "sandbox error classified as tool"
      (define result (classify-error (make-exn "sandbox violation: disallowed operation")))
      (check-not-false result)
      (check-equal? (first result) 'tool))

    ;; ── TUI domain ─────────────────────────────────────────

    (test-case "terminal error classified as tui"
      (define result (classify-error (make-exn "terminal does not support true color")))
      (check-not-false result)
      (check-equal? (first result) 'tui))

    (test-case "render error classified as tui"
      (define result (classify-error (make-exn "render error: invalid escape sequence")))
      (check-not-false result)
      (check-equal? (first result) 'tui))

    ;; ── Unknown ────────────────────────────────────────────

    (test-case "unknown error returns #f"
      (define result (classify-error (make-exn "something completely unexpected")))
      (check-false result "Unknown errors should return #f"))

    ;; ── Output structure ───────────────────────────────────

    (test-case "classified result has 3 elements"
      (define result (classify-error (make-exn "connection refused")))
      (check-equal? (length result) 3 "Result must be (domain message suggestions)")
      (check-true (symbol? (first result)) "Domain must be a symbol")
      (check-true (string? (second result)) "Message must be a string")
      (check-true (and (list? (third result)) (andmap string? (third result)))
                  "Suggestions must be a list of strings"))))

(run-tests classify-tests)
