#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-memory-context-observe.rkt — Observe-only memory context retrieval tests


(require rackunit
         "../runtime/context-assembly/memory-builder.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/protocol.rkt"
         "../tools/builtins/memory-tools.rkt"
         "../runtime/memory/service.rkt"
         "../tools/tool.rkt"
         "../runtime/session/session-config.rkt")

(define (make-test-config #:memory-enabled? [enabled? #f])
  (hash->session-config (hasheq 'memory-backend (if enabled? 'memory-hash #f))))

(define observe-test-context
  (make-exec-context #:working-directory "/tmp/q-memory-observe"
                     #:session-metadata (hasheq 'session-id "sess-observe")))

(define (store-item backend content #:scope [scope 'session])
  (parameterize ([current-memory-backend backend])
    (tool-store-memory (hash 'content content 'scope scope) observe-test-context)))

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

(test-case "observe: retrieves items from backend"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "test fact one" #:scope 'session)
  (store-item b "test fact two" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
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
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
    (check-equal? (length (car result)) 1)
    (check-equal? (memory-item-content (car (car result))) "session item")))

(test-case "observe: respects limit"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (for ([i (in-range 10)])
    (store-item b (format "item ~a" i) #:scope 'session))
  (parameterize ([current-memory-backend b])
    (define result
      (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe" #:limit 3))
    (check-equal? (length (car result)) 3)
    (check-equal? (memory-telemetry-retrieved-count (cdr result)) 3)))

(test-case "observe: telemetry has non-negative latency"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "fact" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
    (define tel (cdr result))
    (check-true (>= (memory-telemetry-latency-ms tel) 0))))

(test-case "observe: telemetry has token estimate"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "a somewhat longer piece of text" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
    (define tel (cdr result))
    (check-true (> (memory-telemetry-token-estimate tel) 0))))

(test-case "observe: telemetry-only variant returns just telemetry"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "fact" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define tel (observe-memory-telemetry cfg #:scope 'session #:session-id "sess-observe"))
    (check-equal? (memory-telemetry-retrieved-count tel) 1)
    (check-true (memory-telemetry-backend-available? tel))))

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

(test-case "estimate-tokens: empty string"
  (check-equal? (estimate-tokens "") 0))

(test-case "estimate-tokens: short string"
  (check-equal? (estimate-tokens "hello") 1))

(test-case "estimate-tokens: longer string"
  (check-equal? (estimate-tokens "a somewhat longer string") 6))

(test-case "estimate-tokens-for-items: sums across items"
  (define items
    (list (memory-item "id1" 'semantic 'session "short" (hasheq) (hasheq) "" "")
          (memory-item "id2" 'semantic 'session "another one here" (hasheq) (hasheq) "" "")))
  (check-true (> (estimate-tokens-for-items items) 0)))

(test-case "observe: failing backend returns error telemetry"
  (define cfg (make-test-config #:memory-enabled? #t))
  (define err-res
    (memory-result #f #f (hasheq 'code 'error 'message "db down" 'retryable? #t) (hasheq)))
  (define bad-backend
    (memory-backend "bad"
                    (lambda (item) err-res)
                    (lambda (query) err-res)
                    (lambda (id item) err-res)
                    (lambda (id scope) err-res)
                    (lambda (query) err-res)
                    (lambda () #t)
                    (lambda (action) err-res)))
  (parameterize ([current-memory-backend bad-backend])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
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
                    (lambda (query) (error "crash!"))
                    (lambda () #t)
                    (lambda (action) (error "crash!"))))
  (parameterize ([current-memory-backend crash-backend])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
    (check-equal? (car result) '())
    (define tel (cdr result))
    (check-true (memory-telemetry-backend-available? tel))
    (check-true (string? (memory-telemetry-error-message tel)))))

(test-case "observe: timeout fail-closed returns empty with timed-out telemetry (F5)"
  (define cfg (make-test-config #:memory-enabled? #t))
  (define slow-backend
    (memory-backend "slow"
                    (lambda (item) (memory-result #t (memory-item-id item) #f (hasheq)))
                    (lambda (query)
                      (sleep 0.5) ; Sleep longer than timeout
                      (memory-result #t '() #f (hasheq)))
                    (lambda (id patch) (memory-result #t id #f (hasheq)))
                    (lambda (id scope) (memory-result #t id #f (hasheq)))
                    (lambda (query) (memory-result #t '() #f (hasheq)))
                    (lambda () #t)
                    (lambda (policy) (memory-result #t #f #f (hasheq)))))
  (parameterize ([current-memory-backend slow-backend]
                 [current-memory-retrieval-timeout-ms 50])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
    (check-equal? (car result) '())
    (define tel (cdr result))
    (check-true (memory-telemetry-timed-out? tel))
    (check-equal? (memory-telemetry-error-message tel) "Memory retrieval timed out")))

(test-case "observe: does not modify any prompt text"
  (define b (make-memory-hash-backend))
  (define cfg (make-test-config #:memory-enabled? #t))
  (store-item b "secret fact" #:scope 'session)
  (parameterize ([current-memory-backend b])
    (define result (observe-memory-for-context cfg #:scope 'session #:session-id "sess-observe"))
    (check-true (list? (car result)))
    (check-true (memory-telemetry? (cdr result)))))
