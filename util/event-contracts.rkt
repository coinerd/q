#lang racket/base
;; STABILITY: stable

;; util/event-contracts.rkt — Reusable contracts for structured event payloads
;;
;; These catch payload shape regressions at emission time.
;; Priority: events consumed by extensions and TUI subscribers.
;;
;; Policy: Adding new keys to payloads is OK (forward-compatible).
;; Removing or renaming keys will cause contract failures.
;; Only the top events consumed by extensions need contracts —
;; internal-only events remain uncontracted for performance.

(require racket/contract)

(provide reason-payload/c
         session-id-payload/c
         iteration-payload/c
         budget-payload/c
         compact-result-payload/c
         error-detail-payload/c
         injection-count-payload/c
         turn-cancelled-payload/c
         iteration-decision-payload/c
         delta-payload/c
         model-name-payload/c
         tool-name-payload/c
         duration-payload/c
         error-type-payload/c)

;; Simple reason payloads: (hasheq 'reason String ...)
(define reason-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'reason) (string? (hash-ref h 'reason #f))))))

;; Session ID payloads: (hasheq 'session-id String ...)
(define session-id-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'session-id) (string? (hash-ref h 'session-id #f))))))

;; Iteration tracking: (hasheq 'iteration Natural ...)
(define iteration-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'iteration)
                (exact-nonnegative-integer? (hash-ref h 'iteration #f))))))

;; Budget check: (hasheq 'estimated-tokens Natural 'budget Natural 'max-tokens Natural)
(define budget-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'estimated-tokens)
                (hash-has-key? h 'budget)
                (hash-has-key? h 'max-tokens)
                (exact-nonnegative-integer? (hash-ref h 'estimated-tokens #f))
                (exact-nonnegative-integer? (hash-ref h 'budget #f))
                (exact-nonnegative-integer? (hash-ref h 'max-tokens #f))))))

;; Compact result: (hasheq 'original-size Natural 'new-size Natural ...)
(define compact-result-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'original-size)
                (or (hash-has-key? h 'new-size) (hash-has-key? h 'kept-count))))))

;; Error detail: (hasheq 'error String ...)
(define error-detail-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'error) (string? (hash-ref h 'error #f))))))

;; Injection count: (hasheq 'count Natural ...)
(define injection-count-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'count) (exact-nonnegative-integer? (hash-ref h 'count #f))))))

;; Turn cancelled: (hasheq 'reason String 'iteration Natural ...)
(define turn-cancelled-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'reason)
                (string? (hash-ref h 'reason #f))
                (hash-has-key? h 'iteration)
                (exact-nonnegative-integer? (hash-ref h 'iteration #f))))))

;; Iteration decision: (hasheq 'iteration Natural 'termination Any ...)
(define iteration-decision-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'iteration)
                (exact-nonnegative-integer? (hash-ref h 'iteration #f))
                (hash-has-key? h 'termination)))))

;; ============================================================
;; Value-type contracts for top-5 event types (I-11, v0.35.0)
;; ============================================================

;; delta-payload/c: delta must be string when present
(define delta-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'delta) (string? (hash-ref h 'delta #f))))))

;; model-name-payload/c: model field must be string
(define model-name-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'model) (string? (hash-ref h 'model #f))))))

;; tool-name-payload/c: toolName field must be string
(define tool-name-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'toolName) (string? (hash-ref h 'toolName #f))))))

;; duration-payload/c: durationMs must be non-negative number
(define duration-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'durationMs)
                (let ([v (hash-ref h 'durationMs #f)]) (and (number? v) (>= v 0)))))))

;; error-type-payload/c: error must be string when present
(define error-type-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'error) (string? (hash-ref h 'error #f))))))

;; ============================================================
;; Event name -> contract mapping (R2 wiring)
;; ============================================================

(define event-payload-contracts
  (hasheq 'agent.blocked
          reason-payload/c
          'agent.started
          session-id-payload/c
          'turn.blocked
          reason-payload/c
          'turn.cancelled
          turn-cancelled-payload/c
          'iteration.decision
          iteration-decision-payload/c
          'context.compacted
          compact-result-payload/c
          'context.budget-exceeded
          budget-payload/c
          'message.injected.drain
          injection-count-payload/c
          'tool.error
          error-detail-payload/c
          'stream.delta
          delta-payload/c
          'provider.error
          error-type-payload/c
          'provider.response.duration
          duration-payload/c
          'model.name
          model-name-payload/c))

(define (event-payload-contract name)
  (hash-ref event-payload-contracts
            (if (string? name)
                (string->symbol name)
                name)
            #f))

(provide event-payload-contracts
         event-payload-contract)
