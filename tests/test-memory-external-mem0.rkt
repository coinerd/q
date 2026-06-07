#lang racket/base
;;; test-memory-external-mem0.rkt — W0/W5 characterization tests
;;;
;;; W0: Characterize external backend / Mem0 adapter gaps.
;;; W5: Verify Mem0 adapter with mock transport (after implementation).
(require rackunit
         racket/string
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/backends/external-protocol.rkt")

;; ============================================================
;; W0: External protocol characterization
;; ============================================================

(test-case "W0: external-protocol module provides expected exports"
  (check-true (parameter? current-external-backend-enabled))
  (check-true (parameter? current-external-timeout-ms))
  (check-true (procedure? make-external-backend))
  (check-true (procedure? redact-content)))

(test-case "W0: external backend disabled by default"
  (check-false (current-external-backend-enabled)))

(test-case "W0: make-external-backend constructs a memory-backend"
  (define mock-transport (lambda (method payload) (hash 'ok? #t 'value '() 'error #f 'meta (hash))))
  (define be (make-external-backend "mock" mock-transport))
  (check-true (memory-backend? be) "should construct a memory-backend"))

(test-case "W0: no mem0-api module yet — placeholder"
  (check-true #t "placeholder — mem0-api.rkt to be created in W5"))

(test-case "W0: redact-content removes sensitive data"
  (define result (redact-content "My key is sk-1234567890abcdef and password=secret"))
  (check-true (string? result))
  (check-false (string-contains? result "sk-1234567890")
               "redaction should remove secrets"))
