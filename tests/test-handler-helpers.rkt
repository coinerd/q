#lang racket/base

;; @speed fast
;; @suite fast

;; W7 v0.99.35: Tests for handler-helpers.rkt
;; Pure functions extracted from core-handlers.rkt.
;; Tests cover: key normalization, hash lookup, payload extraction,
;; retry error type labels, tool progress status text.

(require rackunit
         rackunit/text-ui
         (only-in "../util/event/event.rkt" make-event)
         "../tui/state-events/handler-helpers.rkt")

;; ============================================================
;; kebab->camel tests
;; ============================================================

(define-test-suite kebab->camel-tests
                   (test-case "single-word symbol unchanged"
                     (check-equal? (kebab->camel 'idle) 'idle)
                     (check-equal? (kebab->camel 'verdict) 'verdict))
                   (test-case "two-word symbol camelized"
                     (check-equal? (kebab->camel 'artifact-count) 'artifactCount)
                     (check-equal? (kebab->camel 'risk-level) 'riskLevel))
                   (test-case "multi-word symbol camelized"
                     (check-equal? (kebab->camel 'tool-call-count) 'toolCallCount)
                     (check-equal? (kebab->camel 'a-b-c-d) 'aBCD)))

;; ============================================================
;; hash-ref-multi tests
;; ============================================================

(define-test-suite
 hash-ref-multi-tests
 (test-case "finds kebab-case key"
   (check-equal? (hash-ref-multi (hasheq 'artifact-count 5) 'artifact-count 'artifactCount) 5))
 (test-case "finds camelCase key"
   (check-equal? (hash-ref-multi (hasheq 'artifactCount 7) 'artifact-count 'artifactCount) 7))
 (test-case "returns default when neither found"
   (check-equal? (hash-ref-multi (hasheq 'other 1) 'artifact-count 'artifactCount 'fallback)
                 'fallback)))

;; ============================================================
;; verification-payload-ref tests
;; ============================================================

(define-test-suite
 verification-payload-ref-tests
 (test-case "extracts from direct hash payload"
   (define evt (make-event 'test-event 0 #f #f (hasheq 'artifact-count 5)))
   (check-equal? (verification-payload-ref evt 'artifact-count 0) 5))
 (test-case "extracts from GSD-wrapped data sub-hash"
   (define evt (make-event 'test-event 0 #f #f (hasheq 'data (hasheq 'artifact-count 3))))
   (check-equal? (verification-payload-ref evt 'artifact-count 0) 3))
 (test-case "finds camelCase variant"
   (define evt (make-event 'test-event 0 #f #f (hasheq 'artifactCount 7)))
   (check-equal? (verification-payload-ref evt 'artifact-count 0) 7))
 (test-case "finds camelCase variant in data sub-hash"
   (define evt (make-event 'test-event 0 #f #f (hasheq 'data (hasheq 'artifactCount 9))))
   (check-equal? (verification-payload-ref evt 'artifact-count 0) 9))
 (test-case "returns default for missing key"
   (define evt (make-event 'test-event 0 #f #f (hasheq 'other 1)))
   (check-equal? (verification-payload-ref evt 'missing-key 'fallback) 'fallback))
 (test-case "returns default for non-hash payload"
   (define evt (make-event 'test-event 0 #f #f "not-a-hash"))
   (check-equal? (verification-payload-ref evt 'artifact-count 42) 42)))

;; ============================================================
;; retry-error-type-label tests
;; ============================================================

(define-test-suite retry-error-type-label-tests
                   (test-case "timeout"
                     (check-equal? (retry-error-type-label 'timeout) "LLM timeout"))
                   (test-case "rate-limit"
                     (check-equal? (retry-error-type-label 'rate-limit) "rate limited"))
                   (test-case "context-overflow"
                     (check-equal? (retry-error-type-label 'context-overflow) "context too large"))
                   (test-case "provider-error"
                     (check-equal? (retry-error-type-label 'provider-error) "server error"))
                   (test-case "unknown type returns #f"
                     (check-false (retry-error-type-label 'unknown))
                     (check-false (retry-error-type-label #f))))

;; ============================================================
;; tool-progress-status-text tests
;; ============================================================

(define-test-suite tool-progress-status-text-tests
                   (test-case "single tool shows name"
                     (check-equal? (tool-progress-status-text "bash" 1 1) "bash running"))
                   (test-case "multiple tools shows batch count"
                     (check-equal? (tool-progress-status-text "bash" 5 2)
                                   "2 tools running (5 pending)"))
                   (test-case "zero total shows single tool name"
                     (check-equal? (tool-progress-status-text "read" 0 0) "read running")))

;; ============================================================
;; All tests
;; ============================================================

(define-test-suite all-handler-helpers-tests
                   kebab->camel-tests
                   hash-ref-multi-tests
                   verification-payload-ref-tests
                   retry-error-type-label-tests
                   tool-progress-status-text-tests)

(module+ test
  (run-tests all-handler-helpers-tests))

(module+ main
  (run-tests all-handler-helpers-tests))
