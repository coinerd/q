#lang racket/base

;; tests/test-w3-correctness.rkt — W3 correctness fixes regression tests
;; M1: dead unless guard, M2: externalizable? default, M8: gateway-start!/stop! removed,
;; L3: port comments (already correct), L7: current-execution-plane-timeout-ms wired

(require rackunit
         rackunit/text-ui
         (only-in "../util/message/mas-envelope.rkt" make-mas-envelope)
         (only-in "../tools/tool.rkt" make-tool make-success-result)
         (only-in "../tools/tool-struct.rkt" tool-externalizable?)
         "../sandbox/gateway-bridge.rkt"
         "../agent/roles/tool-gateway.rkt")

(define suite
  (test-suite "W3 Correctness Fixes"

    ;; ── M1: execute-via-worker returns error hash for non-envelope ──
    (test-case "M1: execute-via-worker rejects non-envelope with error hash"
      (define result (execute-via-worker "not-an-envelope"))
      (check-equal? (hash-ref result 'status) 'error)
      (check-equal? (hash-ref result 'role) 'tool-gateway)
      (check-equal? (hash-ref result 'error-message) "execute-via-worker: expected mas-envelope?"))

    ;; ── M2: externalizable? defaults to #f ──
    (test-case "M2: make-tool defaults externalizable? to #f"
      (define t (make-tool "test" "desc" (hasheq) (lambda (args) (make-success-result "ok"))))
      (check-false (tool-externalizable? t)))

    (test-case "M2: make-tool with #:externalizable? #t works"
      (define t
        (make-tool "bash"
                   "desc"
                   (hasheq)
                   (lambda (args) (make-success-result "ok"))
                   #:externalizable? #t))
      (check-true (tool-externalizable? t)))

    ;; ── M8: gateway-start!/gateway-stop! no longer exist ──
    (test-case "M8: current-tool-executor defaults to stub"
      (check-eq? (current-tool-executor) default-tool-executor))

    ;; ── L7: current-execution-plane-timeout-ms is a parameter ──
    (test-case "L7: current-execution-plane-timeout-ms parameter exists"
      (check-true (procedure? current-execution-plane-timeout-ms))
      ;; Verify it defaults to 120000
      (check-equal? (current-execution-plane-timeout-ms) 120000))))

(run-tests suite)
