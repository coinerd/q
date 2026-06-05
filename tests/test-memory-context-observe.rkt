#lang racket/base
;; tests/test-memory-context-observe.rkt — Observe-only memory context retrieval tests
;;
;; v0.95.6: Tests that observe-only retrieval works correctly:
;; - Memory disabled: context output unchanged
;; - Observe mode: backend queried, prompt unchanged
;; - Telemetry includes count/latency/token estimate
;; - Backend timeout/failure does not fail prompt assembly

(require rackunit
         "../runtime/context-assembly/memory-builder.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/protocol.rkt"
         "../tools/builtins/memory-tools.rkt"
         "../tools/tool.rkt"
         "../runtime/session/session-config.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-config #:memory-enabled? [enabled? #f])
  (hash->session-config (hasheq 'memory-backend (if enabled? 'memory-hash #f))))

(define (store-item backend content #:scope [scope 'session])
  (parameterize ([current-memory-backend backend])
    (tool-store-memory (hash 'content content 'scope scope) #f)))

;; ---------------------------------------------------------------------------
;; Memory disabled: output unchanged
;; ---------------------------------------------------------------------------

(test-case "observe: memory disabled returns empty with no-backend telemetry"
  (define cfg (make-test-config #:memory-enabled? #f))
  (define result (observe-memory-for-context cfg))
  (check-equal? (car result) '())
  (define tel (cdr result))
  (check-equal? (memory-telemetry-retrieved-count tel) 0)
  (check-false (memory-telemetry-backend-available? tel))
  (check-false (memory-telemetry-error-message tel)))

(test-case "observe: memory enabled but no backend returns error telemetry"
  (define cfg (make-test-config #:memory-enabled? #t))
  (parameterize ([current-memory-backend #f])
    (define result (observe-memory-for-context cfg))
    (check-equal? (car result) '())
    (define tel (cdr result))
    (check-false (memory-telemetry-backend-available? tel))
    (check-equal? (memory-telemetry-error-message tel) "no backend configured")))

;; ---------------------------------------------------------------------------
;; Observe mode: backend queried, results returned
;; ---------------------------------------------------------------------------

(test-case "observe: retrieves items from backend"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "test fact one" #:scope 'session)
  (store-item b "test fact two" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session))
    (check-equal? (length (car result)) 2)
    (define tel (cdr result))
    (check-equal? (memory-telemetry-retrieved-count tel) 2)
    (check-true (memory-telemetry-backend-available? tel))
    (check-false (memory-telemetry-timed-out? tel))
    (check-false (memory-telemetry-error-message tel))))

(test-case "observe: scopes query correctly"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "session item" #:scope 'session)
  (store-item b "project item" #:scope 'project)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session))
    (check-equal? (length (car result)) 1)
    (check-equal? (memory-item-content (car (car result))) "session item")))

(test-case "observe: respects limit"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (for ([i (in-range 10)])
    (store-item b (format "item ~a" i) #:scope 'session))
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session #:limit 3))
    (check-equal? (length (car result)) 3)
    (check-equal? (memory-telemetry-retrieved-count (cdr result)) 3)))

;; ---------------------------------------------------------------------------
;; Telemetry
;; ---------------------------------------------------------------------------

(test-case "observe: telemetry has non-negative latency"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "fact" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session))
    (define tel (cdr result))
    (check-true (>= (memory-telemetry-latency-ms tel) 0))))

(test-case "observe: telemetry has token estimate"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "a somewhat longer piece of text" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session))
    (define tel (cdr result))
    (check-true (> (memory-telemetry-token-estimate tel) 0))))

(test-case "observe: telemetry-only variant returns just telemetry"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "fact" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define tel (observe-memory-telemetry cfg #:scope 'session))
    (check-equal? (memory-telemetry-retrieved-count tel) 1)
    (check-true (memory-telemetry-backend-available? tel))))

;; ---------------------------------------------------------------------------
;; Telemetry jsexpr conversion
;; ---------------------------------------------------------------------------

(test-case "memory-telemetry->jsexpr produces valid hash"
  (define tel (memory-telemetry 5 120 25 #t #f #f))
  (define js (memory-telemetry->jsexpr tel))
  (check-equal? (hash-ref js 'retrieved_count) 5)
  (check-equal? (hash-ref js 'latency_ms) 120)
  (check-equal? (hash-ref js 'token_estimate) 25)
  (check-true (hash-ref js 'backend_available))
  (check-false (hash-ref js 'timed_out))
  (check-false (hash-ref js 'error_message)))

(test-case "memory-telemetry->jsexpr with error"
  (define tel (memory-telemetry 0 50 0 #t #f "backend error"))
  (define js (memory-telemetry->jsexpr tel))
  (check-equal? (hash-ref js 'error_message) "backend error"))

;; ---------------------------------------------------------------------------
;; Token estimation
;; ---------------------------------------------------------------------------

(test-case "estimate-tokens: empty string"
  (check-equal? (estimate-tokens "") 0))

(test-case "estimate-tokens: short string"
  (check-equal? (estimate-tokens "hello") 1))

(test-case "estimate-tokens: longer string"
  (check-equal? (estimate-tokens "a somewhat longer string") 6))

(test-case "estimate-tokens-for-items: sums across items"
  (define items (list (memory-item "id1" 'semantic 'session "short" (hasheq) (hasheq) "" "")
                      (memory-item "id2" 'semantic 'session "another one here" (hasheq) (hasheq) "" "")))
  (check-true (> (estimate-tokens-for-items items) 0)))

;; ---------------------------------------------------------------------------
;; Backend failure: fail closed
;; ---------------------------------------------------------------------------

(test-case "observe: failing backend returns error telemetry"
  (define cfg (make-test-config #:memory-enabled? #t))
  (define err-res (memory-result #f #f (hasheq 'code 'error 'message "db down" 'retryable? #t) (hasheq)))
  (define bad-backend
    (memory-backend "bad"
     (lambda (item) err-res)
     (lambda (query) err-res)
     (lambda (id item) err-res)
     (lambda (id scope) err-res)
     (lambda () '())
     (lambda () #t)
     (lambda (action params) err-res)))
  (parameterize ([current-memory-backend bad-backend])
    (define result (observe-memory-for-context cfg #:scope 'session))
    (check-equal? (car result) '())
    (define tel (cdr result))
    (check-true (memory-telemetry-backend-available? tel))
    (check-equal? (memory-telemetry-error-message tel) "db down")))

(test-case "observe: exception in backend returns error telemetry"
  (define cfg (make-test-config #:memory-enabled? #t))
  (define crash-backend
    (memory-backend "crash"
     (lambda (item) (error "crash!"))
     (lambda (query) (error "crash!"))
     (lambda (id item) (error "crash!"))
     (lambda (id scope) (error "crash!"))
     (lambda () '())
     (lambda () #t)
     (lambda (action params) (error "crash!"))))
  (parameterize ([current-memory-backend crash-backend])
    (define result (observe-memory-for-context cfg #:scope 'session))
    (check-equal? (car result) '())
    (define tel (cdr result))
    (check-true (memory-telemetry-backend-available? tel))
    (check-true (string? (memory-telemetry-error-message tel)))))

;; ---------------------------------------------------------------------------
;; No prompt text changes in observe mode
;; ---------------------------------------------------------------------------

(test-case "observe: does not modify any prompt text"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "secret fact" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session))
    ;; The result is a (cons items telemetry) — no prompt text generated
    (check-true (list? (car result)))
    (check-true (memory-telemetry? (cdr result)))))
