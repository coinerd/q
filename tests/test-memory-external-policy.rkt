#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-memory-external-policy.rkt — External backend policy tests
;;
;; v0.95.11: Tests for external backend safety:
;; - External backend blocked when disabled (default)
;; - Redaction strips secrets from payloads
;; - External backend receives redacted payloads only
;; - Availability check prevents calls when unavailable
;; - Timeout enforced on calls

(require rackunit
         racket/string
         "../runtime/memory/backends/external-protocol.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt")

;; ---------------------------------------------------------------------------
;; Mock transport — records calls
;; ---------------------------------------------------------------------------

(define recorded-calls (box '()))

(define (mock-transport method payload)
  (set-box! recorded-calls (cons (cons method payload) (unbox recorded-calls)))
  (case method
    [(store) (memory-result #t 'stored #f (hasheq))]
    [(retrieve) (memory-result #t '() #f (hasheq))]
    [(update) (memory-result #t #f #f (hasheq))]
    [(delete) (memory-result #t #f #f (hasheq))]
    [(list) (memory-result #t '() #f (hasheq))]
    [(manage) (memory-result #t #f #f (hasheq))]))

(define (with-fresh-calls thunk)
  (set-box! recorded-calls '())
  (thunk))

;; ---------------------------------------------------------------------------
;; Disabled by default
;; ---------------------------------------------------------------------------

(test-case "external: disabled by default"
  (define ext (make-external-backend "test" mock-transport))
  (check-false (current-external-backend-enabled))
  (check-false (gen:memory-available? ext)))

(test-case "external: store blocked when disabled"
  (define ext (make-external-backend "test" mock-transport))
  (define item
    (memory-item "id" 'semantic 'session "content" (hasheq) (hasheq) "2025-01-01" "2025-01-01"))
  (define r (gen:store-memory! ext item))
  (check-false (memory-result-ok? r)))

(test-case "external: retrieve returns empty when disabled"
  (define ext (make-external-backend "test" mock-transport))
  (define q (memory-query #f #f #f #f #f #f 100 #f))
  (define r (gen:retrieve-memory ext q))
  (check-true (memory-result-ok? r))
  (check-equal? (memory-result-value r) '()))

(test-case "external: delete blocked when disabled"
  (define ext (make-external-backend "test" mock-transport))
  (define r (gen:delete-memory! ext "id" 'session))
  (check-false (memory-result-ok? r)))

;; ---------------------------------------------------------------------------
;; Enabled calls work
;; ---------------------------------------------------------------------------

(test-case "external: store works when enabled"
  (define ext (make-external-backend "test" mock-transport))
  (with-fresh-calls (lambda ()
                      (parameterize ([current-external-backend-enabled #t])
                        (define item
                          (memory-item "id1"
                                       'semantic
                                       'session
                                       "safe content"
                                       (hasheq)
                                       (hasheq)
                                       "2025-01-01"
                                       "2025-01-01"))
                        (define r (gen:store-memory! ext item))
                        (check-true (memory-result-ok? r))
                        ;; Transport should have been called
                        (check >= (length (unbox recorded-calls)) 1)))))

(test-case "external: retrieve works when enabled"
  (define ext (make-external-backend "test" mock-transport))
  (with-fresh-calls (lambda ()
                      (parameterize ([current-external-backend-enabled #t])
                        (define q (memory-query "test" 'session #f #f #f #f 10 #f))
                        (define r (gen:retrieve-memory ext q))
                        (check-true (memory-result-ok? r))
                        (check >= (length (unbox recorded-calls)) 1)))))

;; ---------------------------------------------------------------------------
;; Redaction
;; ---------------------------------------------------------------------------

(test-case "redact-content: strips API keys"
  (define redacted (redact-content "api_key: sk-abc123def456ghi789"))
  (check-false (string-contains? redacted "sk-abc123"))
  (check-true (string-contains? redacted "REDACTED")))

(test-case "redact-content: strips passwords"
  (define redacted (redact-content "password: supersecretpass123"))
  (check-false (string-contains? redacted "supersecretpass"))
  (check-true (string-contains? redacted "REDACTED")))

(test-case "redact-content: strips bearer tokens"
  (define redacted (redact-content "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI"))
  (check-false (string-contains? redacted "eyJhbGciOiJI"))
  (check-true (string-contains? redacted "REDACTED")))

(test-case "redact-content: safe content unchanged"
  (define content "The project uses React for the frontend architecture.")
  (check-equal? (redact-content content) content))

(test-case "external: stored content is redacted before transport"
  (define ext (make-external-backend "test" mock-transport))
  (with-fresh-calls (lambda ()
                      (parameterize ([current-external-backend-enabled #t])
                        (define item
                          (memory-item "id2"
                                       'semantic
                                       'session
                                       "password: secret12345"
                                       (hasheq)
                                       (hasheq)
                                       "2025-01-01"
                                       "2025-01-01"))
                        (gen:store-memory! ext item)
                        ;; Check the transport received redacted content
                        (define call (car (unbox recorded-calls)))
                        (define payload (cdr call))
                        (define sent-content (hash-ref payload 'content ""))
                        (check-true (string-contains? sent-content "REDACTED"))
                        (check-false (string-contains? sent-content "secret12345"))))))

;; ---------------------------------------------------------------------------
;; Error handling
;; ---------------------------------------------------------------------------

(test-case "external: transport error returns graceful failure"
  (define bad-transport (lambda (method payload) (error "Network timeout")))
  (define ext (make-external-backend "bad" bad-transport))
  (parameterize ([current-external-backend-enabled #t])
    (define item
      (memory-item "id3" 'semantic 'session "content" (hasheq) (hasheq) "2025-01-01" "2025-01-01"))
    (define r (gen:store-memory! ext item))
    (check-false (memory-result-ok? r))
    (check-true (string-contains? (hash-ref (memory-result-error r) 'message "") "Network timeout"))))

(test-case "external: list returns memory-result on error"
  (define bad-transport (lambda (method payload) (error "Connection refused")))
  (define ext (make-external-backend "bad" bad-transport))
  (parameterize ([current-external-backend-enabled #t])
    (define result (gen:list-memory ext (memory-query #f #f #f #f #f #f 100 #f)))
    (check-true (memory-result? result))
    (check-false (memory-result-ok? result))))

(test-case "external: disabled list returns memory-result"
  (define ext (make-external-backend "test" mock-transport))
  (define result (gen:list-memory ext (memory-query #f #f #f #f #f #f 10 #f)))
  (check-true (memory-result? result))
  (check-true (memory-result-ok? result))
  (check-equal? (memory-result-value result) '()))

(test-case "external: recursive redaction covers nested payloads"
  (define redacted
    (redact-jsexpr
     (hasheq 'patch (hasheq 'text "token=abc123456789") 'items (list "password: hunter2"))))
  (check-false (regexp-match? #px"abc123456789" (format "~a" redacted)))
  (check-false (regexp-match? #px"hunter2" (format "~a" redacted)))
  (check-true (regexp-match? #px"REDACTED" (format "~a" redacted))))

(test-case "external: transport timeout returns retryable memory-result"
  (define slow-transport
    (lambda (method payload)
      (sleep 0.2)
      (memory-result #t 'late #f (hasheq))))
  (define ext (make-external-backend "slow" slow-transport #:timeout-ms 10))
  (parameterize ([current-external-backend-enabled #t])
    (define item (memory-item "id-timeout" 'semantic 'session "content" (hasheq) (hasheq) "" ""))
    (define result (gen:store-memory! ext item))
    (check-false (memory-result-ok? result))
    (check-true (hash-ref (memory-result-error result) 'retryable? #f))))

;; ---------------------------------------------------------------------------
;; Availability
;; ---------------------------------------------------------------------------

(test-case "external: available only when enabled"
  (define ext (make-external-backend "test" mock-transport))
  (check-false (gen:memory-available? ext))
  (parameterize ([current-external-backend-enabled #t])
    (check-true (gen:memory-available? ext))))

(test-case "external: name includes backend name"
  (define ext (make-external-backend "pinecone" mock-transport))
  (check-true (string-contains? (memory-backend-name ext) "pinecone")))

;; ---------------------------------------------------------------------------
;; Safe-mode blocking (P2-2)
;; ---------------------------------------------------------------------------

(require (only-in "../util/safe-mode/safe-mode-state.rkt" safe-mode? current-safe-mode))

(test-case "external: store blocked in safe mode (F39)"
  (define ext (make-external-backend "test" mock-transport))
  ;; Enable external backend AND safe mode deterministically
  (parameterize ([current-external-backend-enabled #t]
                 [current-safe-mode #t])
    (define item
      (memory-item "safe-id"
                   'semantic
                   'session
                   "safe content"
                   (hasheq)
                   (hasheq)
                   "2025-01-01"
                   "2025-01-01"))
    (define r (gen:store-memory! ext item))
    (check-false (memory-result-ok? r))
    (check-true (string-contains? (hash-ref (memory-result-error r) 'message "") "safe mode"))))

(test-case "external: retrieve returns empty in safe mode (F39)"
  (define ext (make-external-backend "test" mock-transport))
  (parameterize ([current-external-backend-enabled #t]
                 [current-safe-mode #t])
    (define q (memory-query #f #f #f #f #f #f 100 #f))
    ;; When safe mode is active, external-call returns error (safe-mode blocks all calls)
    (define r (gen:retrieve-memory ext q))
    (check-true (memory-result? r))
    (check-false (memory-result-ok? r))
    (check-equal? (hash-ref (memory-result-error r) 'code) 'safe-mode)))

;; ---------------------------------------------------------------------------
;; M13-F7: External fail-closed on malformed response
;; ---------------------------------------------------------------------------

(test-case "external: malformed hash response fails closed"
  (define bad-transport (lambda (method payload) (hasheq 'random 'stuff 'no-valid 'keys)))
  (parameterize ([current-external-backend-enabled #t])
    (define ext (make-external-backend "malformed-test" bad-transport))
    (define q (memory-query "test" 'session #f #f #f #f 5 #f))
    (define r (gen:retrieve-memory ext q))
    (check-true (memory-result? r))
    (check-false (memory-result-ok? r))
    (check-equal? (hash-ref (memory-result-error r) 'code) 'invalid-response)))

(test-case "external: arbitrary non-protocol responses fail closed"
  (for ([bad-value (in-list (list "oops" '(not protocol) 42 '#(bad vector)))])
    (define bad-transport (lambda (method payload) bad-value))
    (parameterize ([current-external-backend-enabled #t])
      (define ext (make-external-backend "bad-value-test" bad-transport))
      (define q (memory-query "test" 'session #f #f #f #f 5 #f))
      (define r (gen:retrieve-memory ext q))
      (check-true (memory-result? r))
      (check-false (memory-result-ok? r))
      (check-equal? (hash-ref (memory-result-error r) 'code) 'invalid-response))))

(test-case "external: valid protocol hash succeeds"
  (define good-transport
    (lambda (method payload) (hasheq 'ok? #t 'value '() 'error #f 'metadata (hasheq))))
  (parameterize ([current-external-backend-enabled #t])
    (define ext (make-external-backend "valid-test" good-transport))
    (define q (memory-query "test" 'session #f #f #f #f 5 #f))
    (define r (gen:retrieve-memory ext q))
    (check-true (memory-result? r))
    (check-true (memory-result-ok? r))))

(test-case "external: error protocol hash returns error"
  (define err-transport
    (lambda (method payload) (hasheq 'ok? #f 'error (hasheq 'code 'test-error 'message "test"))))
  (parameterize ([current-external-backend-enabled #t])
    (define ext (make-external-backend "err-test" err-transport))
    (define q (memory-query "test" 'session #f #f #f #f 5 #f))
    (define r (gen:retrieve-memory ext q))
    (check-true (memory-result? r))
    (check-false (memory-result-ok? r))))
